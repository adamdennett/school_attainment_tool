# scratch_add_nondisadvantaged_decomp.R
# Adds a non-disadvantaged variance decomposition to the slide-deck cache
# (school_effect_decomp.rds), mirroring EXACTLY the disadvantaged decomposition
# in output/model_experiments.qmd (Analysis F). Validates by reproducing the
# cached disadvantaged decomposition first; only writes if it matches.

suppressMessages({library(tidyverse); library(lme4); library(lmerTest); library(performance); library(here)})

ctrl <- lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 20000))

# ---- 1. Panel + region backfill (model_experiments.qmd lines 36-63) ----
panel <- readRDS(here("data", "panel_data.rds"))
la_region_lookup <- read.csv(
  here("data", "meta", "Performancetables_130249", "2022-2023", "la_and_region_codes_meta.csv"),
  stringsAsFactors = FALSE) %>%
  mutate(gor_name_broad = case_when(
    grepl("^North East",   REGION.NAME) ~ "North East",
    grepl("^North West",   REGION.NAME) ~ "North West",
    grepl("^North Yorkshire|^South and West Yorkshire", REGION.NAME) ~ "Yorkshire and the Humber",
    grepl("^East Midlands", REGION.NAME) ~ "East Midlands",
    grepl("^West Midlands", REGION.NAME) ~ "West Midlands",
    grepl("^East of England", REGION.NAME) ~ "East of England",
    grepl("^London",       REGION.NAME) ~ "London",
    grepl("^South East",   REGION.NAME) ~ "South East",
    grepl("^South West",   REGION.NAME) ~ "South West",
    TRUE ~ NA_character_))
panel <- panel %>%
  left_join(la_region_lookup %>% select(LEA, gor_name_broad), by = "LEA") %>%
  mutate(gor_name = coalesce(gor_name, gor_name_broad)) %>% select(-gor_name_broad)

# ---- 2. imputed_full_data (Analysis E filters, lines 103-132) ----
imputed_full_data <- panel %>%
  filter(MINORGROUP %in% c("Academy", "Maintained school")) %>%
  filter(!is.na(ATT8SCR), ATT8SCR > 0, PTFSM6CLA1A > 0, PERCTOT > 0, PNUMEAL > 0,
         !is.na(OFSTEDRATING_1), !is.na(gor_name), !is.na(LANAME)) %>%
  filter(!is.na(remained_in_the_same_school),
         !is.na(teachers_on_leadership_pay_range_percent),
         average_number_of_days_taken > 0, !is.na(gorard_segregation)) %>%
  mutate(OFSTEDRATING_1 = factor(OFSTEDRATING_1,
           levels = c("Outstanding", "Good", "Requires Improvement", "Inadequate"), ordered = TRUE),
         gor_name = factor(gor_name), LANAME = factor(LANAME), year_label = factor(year_label)) %>%
  droplevels()
contrasts(imputed_full_data$OFSTEDRATING_1) <- contr.treatment(levels(imputed_full_data$OFSTEDRATING_1))

# ---- 3. Census join, z-scores on FULL fd (lines 4718-4745) ----
ctx <- readRDS(here("data", "census", "school_census_context.rds"))
zs <- function(x) as.numeric(scale(x))
fd <- imputed_full_data %>%
  inner_join(ctx %>% select(URN, starts_with("cens_"), starts_with("mean_dist")), by = "URN") %>%
  mutate(z_l4plus_k150 = zs(cens_l4plus_k150),
         z_ltsick_k150 = zs(cens_ltsick_k150),
         z_unemp_k150  = zs(cens_unemp_k150))

# ---- 4. Decomposition machinery (lines 5032-5068, 5122-5144) ----
workforce_terms <- c("remained_in_the_same_school",
                     "teachers_on_leadership_pay_range_percent",
                     "log(average_number_of_days_taken)")
split_fixed_var <- function(m, endo_terms = workforce_terms) {
  X <- getME(m, "X"); beta <- fixef(m); cn <- colnames(X)
  is_int <- cn == "(Intercept)"; is_endo <- cn %in% endo_terms
  eta_endo <- as.vector(X[, is_endo, drop = FALSE] %*% beta[is_endo])
  eta_exo  <- as.vector(X[, !is_endo & !is_int, drop = FALSE] %*% beta[!is_endo & !is_int])
  eta_full <- eta_exo + eta_endo
  c(exo = cov(eta_exo, eta_full), endo = cov(eta_endo, eta_full))
}
decompose_model <- function(m, att8_mean) {
  vc <- as.data.frame(VarCorr(m)) %>% filter(is.na(var2))
  fx <- split_fixed_var(m); total_var <- sum(fx) + sum(vc$vcov)
  vc %>%
    transmute(Component = recode(grp,
      "URN" = "School (persistent, unexplained)", "LANAME:gor_name" = "LA (within region)",
      "gor_name" = "Region", "OFSTEDRATING_1" = "Ofsted rating", "year_label" = "Year",
      "Residual" = "Transient (cohort-year noise)"), Variance = vcov) %>%
    bind_rows(tibble(
      Component = c("Exogenous, non-school (cohort, prior attainment, absence, neighbourhood)",
                    "Endogenous, within-school (workforce)"),
      Variance = c(fx[["exo"]], fx[["endo"]]))) %>%
    mutate(`Share of total %` = round(100 * Variance / total_var, 1),
           `SD (log Att8)` = round(sqrt(pmax(Variance, 0)), 4),
           `~Attainment 8 points` = round(att8_mean * sqrt(pmax(Variance, 0)), 1)) %>%
    arrange(desc(Variance))
}

fit_group <- function(outcome) {
  d <- fd %>% filter(!is.na(.data[[outcome]]), .data[[outcome]] > 0) %>% droplevels()
  contrasts(d$OFSTEDRATING_1) <- contr.treatment(levels(d$OFSTEDRATING_1))
  base <- lmer(as.formula(paste0("log(", outcome, ") ~ log(PTFSM6CLA1A) + log(PERCTOT) + log(PNUMEAL) + ",
    "PTPRIORLO + ADMPOL_PT + gorard_segregation + remained_in_the_same_school + ",
    "teachers_on_leadership_pay_range_percent + log(average_number_of_days_taken) + ",
    "(1 | year_label) + (1 | OFSTEDRATING_1) + (1 | gor_name/LANAME)")),
    data = d, REML = FALSE, na.action = na.exclude, control = ctrl)
  census <- update(base, . ~ . + z_l4plus_k150 + z_ltsick_k150 + z_unemp_k150)
  school <- update(census, . ~ . + (1 | URN))
  list(school = school, meanv = mean(d[[outcome]], na.rm = TRUE), n = nobs(school))
}

# ---- 5. Validation: reproduce disadvantaged, compare to cache ----
message("Fitting disadvantaged (validation) ...")
g_dis <- fit_group("ATT8SCR_FSM6CLA1A")
decomp_dis_new <- decompose_model(g_dis$school, g_dis$meanv)

cache_path <- here("data", "cache", "school_effect_decomp.rds")
cache <- readRDS(cache_path)
join_key <- c("Component", "Share of total %")
chk <- inner_join(cache$decomp_dis[join_key], decomp_dis_new[join_key],
                  by = "Component", suffix = c("_cached", "_new"))
chk$diff <- abs(chk$`Share of total %_cached` - chk$`Share of total %_new`)
max_diff <- max(chk$diff)
cat("\n--- Disadvantaged validation (max share diff vs cache):", round(max_diff, 2), "pp ---\n")
print(chk)
cat("mean_att8_dis: cache", round(cache$mean_att8_dis, 2), "| reproduced", round(g_dis$meanv, 2), "\n")

if (max_diff > 0.3) stop("Disadvantaged reproduction diverges from cache (", round(max_diff,2),
                         "pp) — do not trust the non-disadvantaged fit. Investigate before writing.")

# ---- 6. Non-disadvantaged ----
message("\nFitting non-disadvantaged ...")
g_non <- fit_group("ATT8SCR_NFSM6CLA1A")
decomp_non <- decompose_model(g_non$school, g_non$meanv)
mean_att8_non <- g_non$meanv
cat("\n=== Non-disadvantaged decomposition (N =", g_non$n, ", mean Att8 =",
    round(mean_att8_non, 1), ") ===\n")
print(decomp_non)

# ---- 7. Write back ----
cache$decomp_non <- decomp_non
cache$mean_att8_non <- mean_att8_non
saveRDS(cache, cache_path)
cat("\nSaved decomp_non + mean_att8_non into", basename(cache_path), "\n")

# mirror into analysis_f_results.rds if present (keep siblings in sync)
afr_path <- here("data", "cache", "analysis_f_results.rds")
if (file.exists(afr_path)) {
  afr <- readRDS(afr_path)
  afr$decomp_non <- decomp_non; afr$mean_att8_non <- mean_att8_non
  saveRDS(afr, afr_path)
  cat("Also updated", basename(afr_path), "\n")
}
