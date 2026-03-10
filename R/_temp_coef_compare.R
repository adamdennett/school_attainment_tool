library(tidyverse)
library(lme4)
library(lmerTest)
library(performance)
library(here)

source(here::here("R", "helpers.R"))

# Load data
panel <- readRDS(here::here("data", "panel_data.rds"))
finance <- readRDS(here::here("data", "financial_returns.rds"))

finance_slim <- finance %>%
  select(URN, academic_year,
         fin_total_income_pp = total_income_pp,
         fin_total_expenditure_pp = total_expenditure_pp,
         fin_staff_costs_pp = staff_costs_pp,
         fin_teaching_staff_pp = teaching_staff_pp,
         fin_grant_funding_pp = grant_funding_pp,
         fin_premises_costs_pp = premises_costs_pp,
         fin_staff_income_ratio = staff_income_ratio,
         fin_teaching_expenditure_ratio = teaching_expenditure_ratio,
         fin_balance_income_ratio = balance_income_ratio,
         fin_source = source)

panel <- panel %>%
  left_join(finance_slim, by = c("URN", "academic_year"))

# Build model dataset
model_data <- panel %>%
  filter(MINORGROUP %in% c("Academy", "Maintained school")) %>%
  filter(
    !is.na(ATT8SCR), ATT8SCR > 0,
    PTFSM6CLA1A > 0, PERCTOT > 0, PNUMEAL > 0,
    !is.na(OFSTEDRATING_1), !is.na(gor_name), !is.na(LANAME),
    remained_in_the_same_school > 0,
    !is.na(teachers_on_leadership_pay_range_percent),
    average_number_of_days_taken > 0,
    !is.na(gorard_segregation)
  ) %>%
  mutate(
    OFSTEDRATING_1 = factor(OFSTEDRATING_1,
                            levels = c("Outstanding", "Good",
                                       "Requires Improvement", "Inadequate"),
                            ordered = TRUE),
    gor_name   = factor(gor_name),
    LANAME     = factor(LANAME),
    year_label = factor(year_label)
  ) %>%
  droplevels()

contrasts(model_data$OFSTEDRATING_1) <-
  contr.treatment(levels(model_data$OFSTEDRATING_1))

# Finance-complete subset
d_fin <- model_data %>%
  filter(
    !is.na(fin_total_income_pp), fin_total_income_pp > 0,
    !is.na(fin_staff_income_ratio),
    !is.na(fin_balance_income_ratio)
  ) %>%
  droplevels()

contrasts(d_fin$OFSTEDRATING_1) <-
  contr.treatment(levels(d_fin$OFSTEDRATING_1))

cat("Model dataset:", nrow(d_fin), "rows\n\n")

# --- Fit baseline ---
mod_baseline <- lmer(
  log(ATT8SCR) ~
    log(PTFSM6CLA1A) + log(PERCTOT) + log(PNUMEAL) +
    PTPRIORLO + ADMPOL_PT + gorard_segregation +
    log(remained_in_the_same_school) +
    teachers_on_leadership_pay_range_percent +
    log(average_number_of_days_taken) +
    (1 | year_label) + (1 | OFSTEDRATING_1) + (1 | gor_name/LANAME),
  data = d_fin, REML = TRUE, na.action = na.exclude,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 20000))
)

# --- Fit extended model with log(income per pupil) ---
mod_extended <- lmer(
  log(ATT8SCR) ~
    log(PTFSM6CLA1A) + log(PERCTOT) + log(PNUMEAL) +
    PTPRIORLO + ADMPOL_PT + gorard_segregation +
    log(remained_in_the_same_school) +
    teachers_on_leadership_pay_range_percent +
    log(average_number_of_days_taken) +
    log(fin_total_income_pp) +
    (1 | year_label) + (1 | OFSTEDRATING_1) + (1 | gor_name/LANAME),
  data = d_fin, REML = TRUE, na.action = na.exclude,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 20000))
)

# --- Extract and compare fixed effects ---
coef_base <- as.data.frame(summary(mod_baseline)$coefficients)
coef_ext  <- as.data.frame(summary(mod_extended)$coefficients)

coef_base$term <- rownames(coef_base)
coef_ext$term  <- rownames(coef_ext)

comparison <- coef_base %>%
  select(term,
         base_est = Estimate,
         base_se = `Std. Error`,
         base_t = `t value`,
         base_p = `Pr(>|t|)`) %>%
  full_join(
    coef_ext %>%
      select(term,
             ext_est = Estimate,
             ext_se = `Std. Error`,
             ext_t = `t value`,
             ext_p = `Pr(>|t|)`),
    by = "term"
  ) %>%
  mutate(
    change_est = ext_est - base_est,
    pct_change = ifelse(base_est != 0, 100 * (ext_est - base_est) / abs(base_est), NA)
  )

cat("\n========== COEFFICIENT COMPARISON ==========\n\n")
cat(sprintf("%-45s %10s %10s %10s %8s\n",
            "Term", "Baseline", "Extended", "Change", "% Change"))
cat(paste(rep("-", 90), collapse = ""), "\n")

for (i in seq_len(nrow(comparison))) {
  cat(sprintf("%-45s %10.5f %10.5f %10.5f %7.1f%%\n",
              comparison$term[i],
              ifelse(is.na(comparison$base_est[i]), NA, comparison$base_est[i]),
              comparison$ext_est[i],
              ifelse(is.na(comparison$change_est[i]), NA, comparison$change_est[i]),
              ifelse(is.na(comparison$pct_change[i]), NA, comparison$pct_change[i])))
}

cat("\n\n========== SIGNIFICANCE CHANGES ==========\n\n")
cat(sprintf("%-45s %12s %12s\n", "Term", "Baseline p", "Extended p"))
cat(paste(rep("-", 75), collapse = ""), "\n")
for (i in seq_len(nrow(comparison))) {
  bp <- ifelse(is.na(comparison$base_p[i]), "NEW", sprintf("%.5f", comparison$base_p[i]))
  ep <- sprintf("%.5f", comparison$ext_p[i])
  cat(sprintf("%-45s %12s %12s\n", comparison$term[i], bp, ep))
}

# --- Random effects comparison ---
cat("\n\n========== RANDOM EFFECTS VARIANCE ==========\n\n")
vc_base <- as.data.frame(VarCorr(mod_baseline))
vc_ext  <- as.data.frame(VarCorr(mod_extended))

cat("BASELINE:\n")
for (i in seq_len(nrow(vc_base))) {
  cat(sprintf("  %-30s  Var = %.6f  SD = %.4f\n",
              paste(vc_base$grp[i], vc_base$var1[i], sep = " / "),
              vc_base$vcov[i], vc_base$sdcor[i]))
}

cat("\nEXTENDED (+ log income pp):\n")
for (i in seq_len(nrow(vc_ext))) {
  cat(sprintf("  %-30s  Var = %.6f  SD = %.4f\n",
              paste(vc_ext$grp[i], vc_ext$var1[i], sep = " / "),
              vc_ext$vcov[i], vc_ext$sdcor[i]))
}

# --- R2 comparison ---
r2_base <- r2(mod_baseline)
r2_ext  <- r2(mod_extended)

cat("\n\n========== R-SQUARED ==========\n\n")
cat(sprintf("Baseline:  R2m = %.4f,  R2c = %.4f\n", r2_base$R2_marginal, r2_base$R2_conditional))
cat(sprintf("Extended:  R2m = %.4f,  R2c = %.4f\n", r2_ext$R2_marginal, r2_ext$R2_conditional))
cat(sprintf("Change:    ΔR2m = %+.4f, ΔR2c = %+.4f\n",
            r2_ext$R2_marginal - r2_base$R2_marginal,
            r2_ext$R2_conditional - r2_base$R2_conditional))

# --- Correlation of log(income pp) with existing predictors ---
cat("\n\n========== CORRELATIONS: log(fin_total_income_pp) vs predictors ==========\n\n")

pred_vars <- c("PTFSM6CLA1A", "PERCTOT", "PNUMEAL", "PTPRIORLO",
               "ADMPOL_PT", "gorard_segregation", "remained_in_the_same_school",
               "teachers_on_leadership_pay_range_percent", "average_number_of_days_taken")

for (v in pred_vars) {
  vals <- d_fin[[v]]
  income <- d_fin$fin_total_income_pp
  # Use log where appropriate
  if (v %in% c("PTFSM6CLA1A", "PERCTOT", "PNUMEAL", "remained_in_the_same_school", "average_number_of_days_taken")) {
    r <- cor(log(vals), log(income), use = "complete.obs")
    cat(sprintf("  log(%-45s) vs log(income_pp): r = %+.3f\n", v, r))
  } else {
    r <- cor(vals, log(income), use = "complete.obs")
    cat(sprintf("  %-49s vs log(income_pp): r = %+.3f\n", v, r))
  }
}
