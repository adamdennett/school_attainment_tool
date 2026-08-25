# =====================================================================
# Which prior-attainment measure should the model use?
#
# The published model controls for prior attainment with the % of KS4
# pupils at LOW KS2 prior attainment (PTPRIORLO). That choice was not
# tested: mean KS2 scaled score (KS2ASS) was equally available for the
# three years where prior attainment is observed, and equally
# imputable for 2024-25 (where NO prior-attainment measure exists,
# because the cohort's KS2 tests were cancelled).
#
# Here we impute KS2ASS for 2024-25 using EXACTLY the same rule the
# pipeline uses for PTPRIORLO (carry-forward from 2023-24; fallback to
# the school's mean across observed years), then compare, on the full
# four-year panel:
#
#   L    PTPRIORLO only          <- published specification
#   M    KS2ASS only             <- mean prior attainment instead
#   B    both
#
# Reported: model fit (AIC/BIC, marginal/conditional R2) and the
# coefficients on the other predictors -- especially absence, the
# paper's central quantity.
# =====================================================================

suppressPackageStartupMessages({
  library(tidyverse); library(lme4); library(lmerTest)
  library(performance); library(here)
})

ctrl <- lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 20000))
panel <- readRDS(here::here("data", "panel_data.rds"))

# ---- Replicate the pipeline's imputation rule for KS2ASS -------------
# (PTPRIORLO is already imputed in panel_data.rds by 04_compute_derived.R)
impute_like_pipeline <- function(df, v) {
  is_2425 <- df$year_label == "2024-25"
  hist <- df %>% filter(year_label != "2024-25")
  cf <- hist %>% filter(year_label == "2023-24", !is.na(.data[[v]])) %>%
    select(URN, cf_value = all_of(v))
  sm <- hist %>% filter(!is.na(.data[[v]])) %>%
    group_by(URN) %>%
    summarise(sm_value = mean(.data[[v]], na.rm = TRUE), .groups = "drop")
  idx <- which(is_2425 & is.na(df[[v]]))
  lut_cf <- setNames(cf$cf_value, cf$URN)
  lut_sm <- setNames(sm$sm_value, sm$URN)
  n_cf <- 0; n_sm <- 0
  for (i in idx) {
    u <- as.character(df$URN[i])
    if (!is.na(lut_cf[u])) { df[[v]][i] <- lut_cf[u]; n_cf <- n_cf + 1; next }
    if (!is.na(lut_sm[u])) { df[[v]][i] <- lut_sm[u]; n_sm <- n_sm + 1 }
  }
  cat(sprintf("  %s: imputed %d rows (%d carry-forward, %d school-mean); %d still NA\n",
              v, n_cf + n_sm, n_cf, n_sm, sum(is.na(df[[v]][is_2425]))))
  df
}

cat("Imputing KS2ASS for 2024-25 with the pipeline rule:\n")
panel <- impute_like_pipeline(panel, "KS2ASS")

# ---- Estimation data: same filters as the published model ------------
d <- panel %>%
  filter(MINORGROUP %in% c("Academy", "Maintained school")) %>%
  filter(!is.na(ATT8SCR), ATT8SCR > 0,
         PTFSM6CLA1A > 0, PERCTOT > 0, PNUMEAL > 0,
         !is.na(OFSTEDRATING_1), !is.na(gor_name), !is.na(LANAME)) %>%
  filter(!is.na(remained_in_the_same_school),
         !is.na(teachers_on_leadership_pay_range_percent),
         average_number_of_days_taken > 0,
         !is.na(gorard_segregation),
         !is.na(PTPRIORLO), !is.na(KS2ASS)) %>%   # common sample for fair comparison
  mutate(OFSTEDRATING_1 = factor(OFSTEDRATING_1,
                                 levels = c("Outstanding", "Good",
                                            "Requires Improvement", "Inadequate")),
         gor_name = factor(gor_name), LANAME = factor(LANAME),
         year_label = factor(year_label),
         ks2_c = KS2ASS - 100) %>%
  droplevels()
contrasts(d$OFSTEDRATING_1) <- contr.treatment(levels(d$OFSTEDRATING_1))

cat(sprintf("\nEstimation sample: %d school-years across %s\n\n",
            nrow(d), paste(levels(d$year_label), collapse = ", ")))
cat("Correlation PTPRIORLO vs KS2ASS (imputed panel): ",
    sprintf("%.3f\n\n", cor(d$PTPRIORLO, d$KS2ASS, use = "complete.obs")))

rest <- paste("log(PTFSM6CLA1A) + log(PERCTOT) + log(PNUMEAL) + ADMPOL_PT +",
              "gorard_segregation + remained_in_the_same_school +",
              "teachers_on_leadership_pay_range_percent +",
              "log(average_number_of_days_taken) +",
              "(1 | year_label) + (1 | OFSTEDRATING_1) + (1 | gor_name/LANAME)")

specs <- list(
  `L: PTPRIORLO (published)` = "PTPRIORLO",
  `M: mean KS2 instead`      = "ks2_c",
  `B: both`                  = "PTPRIORLO + ks2_c"
)

run <- function(outcome, label) {
  cat("\n##############################################\n")
  cat("##", label, "\n")
  cat("##############################################\n\n")
  models <- lapply(specs, function(p)
    lmer(as.formula(paste0("log(", outcome, ") ~ ", p, " + ", rest)),
         data = d, REML = FALSE, control = ctrl))   # ML for AIC comparability
  names(models) <- names(specs)

  cat("--- Fit (ML) ---\n")
  fit <- bind_rows(lapply(names(models), function(n) {
    r <- performance::r2(models[[n]])
    tibble(spec = n,
           marginal_R2 = as.numeric(r$R2_marginal),
           conditional_R2 = as.numeric(r$R2_conditional),
           AIC = AIC(models[[n]]), BIC = BIC(models[[n]]))
  }))
  fit$dAIC <- fit$AIC - min(fit$AIC)
  print(as.data.frame(fit), digits = 5, row.names = FALSE)

  cat("\n--- Coefficients on the other predictors ---\n")
  keys <- c("log(PTFSM6CLA1A)", "log(PERCTOT)", "log(PNUMEAL)",
            "gorard_segregation")
  cc <- bind_rows(lapply(names(models), function(n) {
    s <- summary(models[[n]])$coefficients
    tibble(spec = n, term = keys, est = s[keys, "Estimate"],
           t = s[keys, "t value"])
  }))
  print(as.data.frame(cc %>% select(term, spec, est) %>%
                        pivot_wider(names_from = spec, values_from = est)),
        digits = 4, row.names = FALSE)
  cat("\n  (t values)\n")
  print(as.data.frame(cc %>% select(term, spec, t) %>%
                        pivot_wider(names_from = spec, values_from = t)),
        digits = 3, row.names = FALSE)

  cat("\n--- % change in each coefficient, M vs L ---\n")
  w <- cc %>% select(term, spec, est) %>%
    pivot_wider(names_from = spec, values_from = est)
  chg <- tibble(term = w$term,
                pct = 100 * (w$`M: mean KS2 instead` - w$`L: PTPRIORLO (published)`) /
                  abs(w$`L: PTPRIORLO (published)`))
  print(as.data.frame(chg), digits = 3, row.names = FALSE)

  cat("\n--- prior-attainment terms themselves ---\n")
  for (n in names(models)) {
    s <- summary(models[[n]])$coefficients
    pv <- intersect(c("PTPRIORLO", "ks2_c"), rownames(s))
    for (p in pv)
      cat(sprintf("  %-26s %-10s est=%8.5f  t=%7.2f\n", n, p,
                  s[p, "Estimate"], s[p, "t value"]))
  }
  invisible(models)
}

sink(here::here("revisions", "r23b_output.txt"), split = TRUE)
cat("Prior-attainment measure choice: low-attainer share vs mean KS2\n")
cat("================================================================\n")
cat(sprintf("Estimation sample: %d school-years\n", nrow(d)))
cat(sprintf("cor(PTPRIORLO, KS2ASS) = %.3f\n", cor(d$PTPRIORLO, d$KS2ASS)))
run("ATT8SCR", "ALL PUPILS")
run("ATT8SCR_FSM6CLA1A", "DISADVANTAGED PUPILS")
sink()
cat("\nDone.\n")
