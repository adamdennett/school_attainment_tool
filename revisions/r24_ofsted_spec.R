# =====================================================================
# R2.4 — Should Ofsted rating be a random effect or an ordinal fixed effect?
#
# Reviewer 2: "Random effects are usually reserved for grouping variables
# with many levels; Ofsted rating has only four ordered categories and its
# relationship to the outcome seems important to your argument. I would
# have assumed that an ordinal fixed effect would be more appropriate."
#
# Four specifications, identical except for how Ofsted enters:
#
#   O-re    (1 | OFSTEDRATING_1)      <- the published specification
#   O-fe    categorical fixed effect (treatment contrasts, ref = Outstanding)
#   O-ord   single linear ordinal term (Outstanding=1 ... Inadequate=4)
#   O-none  Ofsted omitted entirely (bears on the endogeneity concern:
#           inspectors observe attainment, so the rating is partly an
#           outcome rather than a predictor)
#
# We report the random-effect variance (to show how thinly it is
# estimated from four levels), fit statistics, whether substantive
# conclusions move, and the knock-on effect on B&H's LA effect.
# =====================================================================

suppressPackageStartupMessages({
  library(tidyverse); library(lme4); library(lmerTest)
  library(performance); library(here)
})

ctrl <- lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 20000))

panel <- readRDS(here::here("data", "panel_data.rds"))

d <- panel %>%
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
    !is.na(gorard_segregation), !is.na(PTPRIORLO)
  ) %>%
  mutate(
    OFSTEDRATING_1 = factor(OFSTEDRATING_1,
                            levels = c("Outstanding", "Good",
                                       "Requires Improvement", "Inadequate"),
                            ordered = TRUE),
    # unordered copy for interpretable treatment contrasts
    ofsted_fe  = factor(as.character(OFSTEDRATING_1),
                        levels = c("Outstanding", "Good",
                                   "Requires Improvement", "Inadequate")),
    # linear ordinal score
    ofsted_ord = as.numeric(OFSTEDRATING_1),
    gor_name = factor(gor_name), LANAME = factor(LANAME),
    year_label = factor(year_label)
  ) %>%
  droplevels()

contrasts(d$OFSTEDRATING_1) <- contr.treatment(levels(d$OFSTEDRATING_1))

rhs_common <- paste(
  "log(PTFSM6CLA1A) + log(PERCTOT) + log(PNUMEAL) + PTPRIORLO +",
  "ADMPOL_PT + gorard_segregation + remained_in_the_same_school +",
  "teachers_on_leadership_pay_range_percent +",
  "log(average_number_of_days_taken) +",
  "(1 | year_label) + (1 | gor_name/LANAME)"
)

specs <- list(
  `O-re`   = "(1 | OFSTEDRATING_1)",
  `O-fe`   = "ofsted_fe",
  `O-ord`  = "ofsted_ord",
  `O-none` = NULL
)

fit <- function(term, outcome) {
  rhs <- if (is.null(term)) rhs_common else paste(term, "+", rhs_common)
  lmer(as.formula(paste0("log(", outcome, ") ~ ", rhs)),
       data = d, REML = TRUE, control = ctrl)
}

key_terms <- c("log(PTFSM6CLA1A)", "log(PERCTOT)", "log(PNUMEAL)",
               "PTPRIORLO", "gorard_segregation",
               "ofsted_ord", "ofsted_feGood",
               "ofsted_feRequires Improvement", "ofsted_feInadequate")

coefs_of <- function(m, label) {
  s <- summary(m)$coefficients
  tibble(term = rownames(s), est = s[, "Estimate"],
         t = s[, "t value"], p = s[, "Pr(>|t|)"], spec = label)
}

bh_effect <- function(m) {
  re <- ranef(m)[["LANAME:gor_name"]]
  v  <- re[["(Intercept)"]]
  names(v) <- sub(":.*$", "", rownames(re))
  i <- grepl("Brighton", names(v))
  c(effect = unname(v[i]), rank = unname(rank(-v)[i]), n_la = length(v))
}

run <- function(outcome, label, base_att8) {
  cat("\n##############################################\n")
  cat("##", label, "\n")
  cat("##############################################\n\n")

  models <- lapply(specs, fit, outcome = outcome)
  names(models) <- names(specs)

  cat("--- Ofsted random-effect variance (O-re) ---\n")
  vc <- as.data.frame(VarCorr(models[["O-re"]]))
  print(vc[, c("grp", "vcov", "sdcor")], digits = 4, row.names = FALSE)
  cat("  (estimated from 4 levels — the reviewer's concern)\n")

  cat("\n--- Ofsted fixed-effect coefficients (O-fe, ref = Outstanding) ---\n")
  fe <- coefs_of(models[["O-fe"]], "O-fe") %>%
    filter(grepl("^ofsted_fe", term)) %>%
    mutate(att8_pts = base_att8 * (exp(est) - 1)) %>%
    select(term, est, att8_pts, t, p)
  print(as.data.frame(fe), digits = 3, row.names = FALSE)

  cat("\n--- Ofsted linear ordinal term (O-ord) ---\n")
  od <- coefs_of(models[["O-ord"]], "O-ord") %>%
    filter(term == "ofsted_ord") %>%
    mutate(att8_pts_per_band = base_att8 * (exp(est) - 1)) %>%
    select(term, est, att8_pts_per_band, t, p)
  print(as.data.frame(od), digits = 3, row.names = FALSE)

  cc <- bind_rows(lapply(names(models),
                         function(n) coefs_of(models[[n]], n))) %>%
    filter(term %in% key_terms)

  cat("\n--- Key coefficients across specifications ---\n")
  print(as.data.frame(cc %>% select(term, spec, est) %>%
                        pivot_wider(names_from = spec, values_from = est)),
        digits = 3, row.names = FALSE)

  cat("\n--- t values ---\n")
  print(as.data.frame(cc %>% select(term, spec, t) %>%
                        pivot_wider(names_from = spec, values_from = t)),
        digits = 3, row.names = FALSE)

  cat("\n--- Fit ---\n")
  fs <- bind_rows(lapply(names(models), function(n) {
    r <- performance::r2(models[[n]])
    tibble(spec = n,
           n_par = length(fixef(models[[n]])),
           R2_marginal = as.numeric(r$R2_marginal),
           R2_conditional = as.numeric(r$R2_conditional),
           AIC = AIC(models[[n]]), BIC = BIC(models[[n]]))
  }))
  print(as.data.frame(fs), digits = 5, row.names = FALSE)

  cat("\n--- Brighton and Hove LA effect ---\n")
  bh <- bind_rows(lapply(names(models), function(n) {
    b <- bh_effect(models[[n]])
    tibble(spec = n, effect = b["effect"], rank = b["rank"], n_la = b["n_la"])
  }))
  print(as.data.frame(bh), digits = 3, row.names = FALSE)

  invisible(models)
}

sink(here::here("revisions", "r24_output.txt"), split = TRUE)
cat("R2.4 — Ofsted as random effect vs ordinal / categorical fixed effect\n")
cat("====================================================================\n\n")
cat("Estimation sample:", nrow(d), "school-years,",
    dplyr::n_distinct(d$URN), "schools\n\n")
cat("Ofsted rating distribution (school-years):\n")
print(table(d$ofsted_fe))
cat("\nAs % of school-years:\n")
print(round(100 * prop.table(table(d$ofsted_fe)), 1))

mean_all <- mean(d$ATT8SCR, na.rm = TRUE)
mean_dis <- mean(d$ATT8SCR_FSM6CLA1A[d$ATT8SCR_FSM6CLA1A > 0], na.rm = TRUE)
cat(sprintf("\nMean ATT8: all %.1f | disadvantaged %.1f\n", mean_all, mean_dis))

run("ATT8SCR", "ALL PUPILS", mean_all)
run("ATT8SCR_FSM6CLA1A", "DISADVANTAGED PUPILS", mean_dis)
sink()

cat("\nDone.\n")
