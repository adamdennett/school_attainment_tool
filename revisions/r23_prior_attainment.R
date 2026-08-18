# =====================================================================
# R2.3 / R2.1 — Is the prior-attainment control adequate?
#
# Reviewer 2 asks why only % low prior attainment (PTPRIORLO) is used,
# whether mean KS2 score is available, and whether estimates elsewhere
# are inflated as a result.
#
# Mean KS2 scaled score (KS2ASS) and the middle/high prior-attainment
# bands ARE in the data, but only for 2021-22 to 2023-24 — DfE did not
# publish them for 2024-25. That is why the main model uses PTPRIORLO
# (the only measure spanning all four years).
#
# Here we refit on the 3-year subsample where the richer measures exist,
# holding the estimation sample fixed, and compare:
#
#   P0  published specification (PTPRIORLO only)
#   P1  + mean KS2 scaled score (KS2ASS, centred at 100)
#   P2  + % high prior attainment (PTPRIORHI)
#   P3  + both
#
# We report coefficient movement on the key intake variables, R2 change,
# and whether B&H's LA effect (the R2.7 quantity) is affected.
# =====================================================================

suppressPackageStartupMessages({
  library(tidyverse); library(lme4); library(lmerTest)
  library(performance); library(here)
})

ctrl <- lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 20000))

panel <- readRDS(here::here("data", "panel_data.rds"))

base <- panel %>%
  filter(MINORGROUP %in% c("Academy", "Maintained school")) %>%
  filter(
    !is.na(ATT8SCR), ATT8SCR > 0,
    PTFSM6CLA1A > 0, PERCTOT > 0, PNUMEAL > 0,
    !is.na(OFSTEDRATING_1), !is.na(gor_name), !is.na(LANAME)
  ) %>%
  filter(
    !is.na(remained_in_the_same_school),
    !is.na(teachers_on_leadership_pay_range_percent),
    average_number_of_days_taken > 0,
    !is.na(gorard_segregation),
    !is.na(PTPRIORLO)
  ) %>%
  mutate(
    OFSTEDRATING_1 = factor(OFSTEDRATING_1,
                            levels = c("Outstanding", "Good",
                                       "Requires Improvement", "Inadequate"),
                            ordered = TRUE),
    gor_name = factor(gor_name), LANAME = factor(LANAME),
    year_label = factor(year_label)
  )

# Restrict to rows where the richer prior-attainment measures exist
d <- base %>%
  filter(!is.na(KS2ASS), !is.na(PTPRIORHI)) %>%
  mutate(ks2_c = KS2ASS - 100) %>%     # centre scaled score at 100
  droplevels()
contrasts(d$OFSTEDRATING_1) <- contr.treatment(levels(d$OFSTEDRATING_1))

cat("Full 4-year sample: ", nrow(base), "school-years\n")
cat("3-year subsample:   ", nrow(d), "school-years,",
    n_distinct(d$LANAME), "LAs,",
    paste(levels(d$year_label), collapse = "/"), "\n\n")

cat("Correlations among prior-attainment measures:\n")
print(round(cor(d[, c("PTPRIORLO", "PTPRIORHI", "KS2ASS")],
                use = "complete.obs"), 3))
cat("\n")

rhs_common <- paste(
  "log(PTFSM6CLA1A) + log(PERCTOT) + log(PNUMEAL) + PTPRIORLO +",
  "ADMPOL_PT + gorard_segregation + remained_in_the_same_school +",
  "teachers_on_leadership_pay_range_percent +",
  "log(average_number_of_days_taken) +",
  "(1 | year_label) + (1 | OFSTEDRATING_1) + (1 | gor_name/LANAME)"
)

specs <- list(
  P0 = NULL,
  P1 = "ks2_c",
  P2 = "PTPRIORHI",
  P3 = "ks2_c + PTPRIORHI"
)

fit <- function(extra, outcome) {
  rhs <- if (is.null(extra)) rhs_common else paste(extra, "+", rhs_common)
  lmer(as.formula(paste0("log(", outcome, ") ~ ", rhs)),
       data = d, REML = TRUE, control = ctrl)
}

key_terms <- c("log(PTFSM6CLA1A)", "log(PERCTOT)", "log(PNUMEAL)",
               "PTPRIORLO", "ks2_c", "PTPRIORHI",
               "gorard_segregation")

coefs_of <- function(m, label) {
  s <- summary(m)$coefficients
  tibble(term = rownames(s), est = s[, "Estimate"],
         se = s[, "Std. Error"], t = s[, "t value"],
         p = s[, "Pr(>|t|)"], spec = label)
}

bh_effect <- function(m) {
  re <- ranef(m)[["LANAME:gor_name"]]
  v  <- re[["(Intercept)"]]
  names(v) <- sub(":.*$", "", rownames(re))
  c(effect = unname(v[grepl("Brighton", names(v))]),
    rank   = unname(rank(-v)[grepl("Brighton", names(v))]),
    n_la   = length(v))
}

run <- function(outcome, label) {
  cat("\n##############################################\n")
  cat("##", label, "\n")
  cat("##############################################\n\n")
  models <- lapply(specs, fit, outcome = outcome)
  names(models) <- names(specs)

  cc <- bind_rows(lapply(names(models),
                         function(n) coefs_of(models[[n]], n))) %>%
    filter(term %in% key_terms)

  cat("--- Key coefficients ---\n")
  wide <- cc %>% select(term, spec, est) %>%
    pivot_wider(names_from = spec, values_from = est)
  print(as.data.frame(wide), digits = 3, row.names = FALSE)

  cat("\n--- t values ---\n")
  wt <- cc %>% select(term, spec, t) %>%
    pivot_wider(names_from = spec, values_from = t)
  print(as.data.frame(wt), digits = 3, row.names = FALSE)

  cat("\n--- % change in coefficient vs P0 ---\n")
  base_est <- cc %>% filter(spec == "P0") %>% select(term, base = est)
  pct <- cc %>% filter(spec != "P0") %>% left_join(base_est, by = "term") %>%
    mutate(pct = 100 * (est - base) / abs(base)) %>%
    select(term, spec, pct) %>%
    pivot_wider(names_from = spec, values_from = pct)
  print(as.data.frame(pct), digits = 3, row.names = FALSE)

  cat("\n--- Fit ---\n")
  fitstats <- bind_rows(lapply(names(models), function(n) {
    r <- performance::r2(models[[n]])
    tibble(spec = n,
           R2_marginal = as.numeric(r$R2_marginal),
           R2_conditional = as.numeric(r$R2_conditional),
           AIC = AIC(models[[n]]))
  }))
  print(as.data.frame(fitstats), digits = 4, row.names = FALSE)

  cat("\n--- Brighton and Hove LA effect (links to R2.7) ---\n")
  bh <- bind_rows(lapply(names(models), function(n) {
    b <- bh_effect(models[[n]])
    tibble(spec = n, effect = b["effect"], rank = b["rank"], n_la = b["n_la"])
  }))
  print(as.data.frame(bh), digits = 3, row.names = FALSE)

  invisible(models)
}

sink(here::here("revisions", "r23_output.txt"), split = TRUE)
cat("R2.3 — richer prior-attainment controls, 3-year subsample\n")
cat("=========================================================\n\n")
cat("Full 4-year sample: ", nrow(base), "school-years\n")
cat("3-year subsample:   ", nrow(d), "school-years,",
    n_distinct(d$LANAME), "LAs\n\n")
cat("Correlations among prior-attainment measures:\n")
print(round(cor(d[, c("PTPRIORLO", "PTPRIORHI", "KS2ASS")],
                use = "complete.obs"), 3))
run("ATT8SCR", "ALL PUPILS")
run("ATT8SCR_FSM6CLA1A", "DISADVANTAGED PUPILS")
sink()

cat("\nDone.\n")
