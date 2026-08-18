# =====================================================================
# R2.7 — Does Brighton and Hove's LA effect for disadvantaged pupils
#        depend on controlling for absence?
#
# Reviewer 2 asks whether the "7th best in England" result is justifiable
# given it conditions on absence, when B&H has the 2nd-worst absence rate
# in the country. They ask for the result "with and without this factor".
#
# We fit four specifications, IDENTICAL except for the absence term, on
# the SAME set of rows so LA rankings are directly comparable:
#
#   A-none    no absence term at all
#   A-raw     log(PERCTOT)                 <- the published specification
#   A-expL    log(expected absence), stage 1 WITH LA random effect
#   A-expNoL  log(expected absence), stage 1 WITHOUT LA random effect
#
# The two expected-absence variants matter. If stage 1 includes an LA
# random effect, expected absence absorbs the LA's own mean absence, so
# B&H's excess absence is treated as exogenous context. If it does not,
# expected absence reflects only what INTAKE predicts, and B&H's excess
# absence over that stays in the residual — i.e. is treated as
# potentially system-manageable. This is precisely the distinction the
# reviewer is probing.
# =====================================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(lme4)
  library(lmerTest)
  library(here)
})

OUT <- here::here("revisions", "r27_results.rds")
ctrl <- lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 20000))

# ---- Data (mirrors the paper's Analysis E preparation) --------------
panel <- readRDS(here::here("data", "panel_data.rds"))

imputed_full_data <- panel %>%
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

contrasts(imputed_full_data$OFSTEDRATING_1) <-
  contr.treatment(levels(imputed_full_data$OFSTEDRATING_1))

# ---- Stage 1: expected absence, two variants ------------------------
cat("Fitting stage 1 (expected absence)...\n")

s1_data <- imputed_full_data %>%
  filter(!is.na(PTPRIORLO)) %>%
  droplevels()

# Variant L: LA random effect included -> absorbs LA-level mean absence
s1_L <- lmer(
  log(PERCTOT) ~ log(PTFSM6CLA1A) + log(PNUMEAL) + PTPRIORLO +
    gorard_segregation + (1 | year_label) + (1 | gor_name/LANAME),
  data = s1_data, REML = TRUE, control = ctrl
)

# Variant NoL: no LA random effect -> only intake (+ region) predicts absence
s1_NoL <- lmer(
  log(PERCTOT) ~ log(PTFSM6CLA1A) + log(PNUMEAL) + PTPRIORLO +
    gorard_segregation + (1 | year_label) + (1 | gor_name),
  data = s1_data, REML = TRUE, control = ctrl
)

s1_data <- s1_data %>%
  mutate(
    log_exp_abs_L   = predict(s1_L,   newdata = ., re.form = NULL,
                              allow.new.levels = TRUE),
    log_exp_abs_NoL = predict(s1_NoL, newdata = ., re.form = NULL,
                              allow.new.levels = TRUE)
  )

# ---- Common estimation sample for stage 2 ---------------------------
d <- s1_data %>%
  filter(!is.na(ATT8SCR_FSM6CLA1A), ATT8SCR_FSM6CLA1A > 0) %>%
  droplevels()
contrasts(d$OFSTEDRATING_1) <- contr.treatment(levels(d$OFSTEDRATING_1))

cat("Stage 2 estimation sample:", nrow(d), "school-years,",
    n_distinct(d$LANAME), "LAs\n\n")

# ---- Stage 2: four specifications, disadvantaged outcome ------------
base_rhs <- paste(
  "log(PTFSM6CLA1A) + log(PNUMEAL) + PTPRIORLO + ADMPOL_PT +",
  "gorard_segregation + remained_in_the_same_school +",
  "teachers_on_leadership_pay_range_percent +",
  "log(average_number_of_days_taken) +",
  "(1 | year_label) + (1 | OFSTEDRATING_1) + (1 | gor_name/LANAME)"
)

specs <- list(
  `A-none`   = NULL,
  `A-raw`    = "log(PERCTOT)",
  `A-expL`   = "log_exp_abs_L",
  `A-expNoL` = "log_exp_abs_NoL"
)

fit_spec <- function(abs_term, outcome) {
  rhs <- if (is.null(abs_term)) base_rhs else paste(abs_term, "+", base_rhs)
  f <- as.formula(paste0("log(", outcome, ") ~ ", rhs))
  lmer(f, data = d, REML = TRUE, control = ctrl)
}

# Extract LA-within-region random effects and rank (1 = best)
la_effects <- function(m, label) {
  re <- ranef(m, condVar = TRUE)[["LANAME:gor_name"]]
  se <- sqrt(attr(ranef(m, condVar = TRUE)[["LANAME:gor_name"]],
                  "postVar")[1, 1, ])
  tibble(
    group   = rownames(re),
    effect  = re[["(Intercept)"]],
    se      = se
  ) %>%
    mutate(
      la     = sub(":.*$", "", group),
      ci_lo  = effect - 1.96 * se,
      ci_hi  = effect + 1.96 * se,
      sig    = ci_lo > 0 | ci_hi < 0,
      rank   = rank(-effect),          # 1 = best
      n_las  = n(),
      spec   = label
    )
}

run_outcome <- function(outcome) {
  cat("=== Outcome:", outcome, "===\n")
  models <- list()
  effs   <- list()
  for (nm in names(specs)) {
    cat("  fitting", nm, "...\n")
    m <- fit_spec(specs[[nm]], outcome)
    models[[nm]] <- m
    effs[[nm]]   <- la_effects(m, nm)
  }
  list(models = models, effects = bind_rows(effs))
}

res_dis <- run_outcome("ATT8SCR_FSM6CLA1A")
res_all <- run_outcome("ATT8SCR")

# ---- Report ---------------------------------------------------------
report <- function(res, label) {
  cat("\n\n##############################################\n")
  cat("##", label, "\n")
  cat("##############################################\n\n")

  e <- res$effects
  n_las <- unique(e$n_las)[1]
  cat("Number of LAs:", n_las, "\n\n")

  cat("--- Brighton and Hove ---\n")
  bh <- e %>% filter(grepl("Brighton", la)) %>%
    select(spec, effect, se, ci_lo, ci_hi, sig, rank)
  print(as.data.frame(bh), digits = 3, row.names = FALSE)

  cat("\n--- Rank correlation between specifications (all LAs) ---\n")
  w <- e %>% select(la, spec, effect) %>%
    pivot_wider(names_from = spec, values_from = effect)
  sp <- names(specs)
  cm <- matrix(NA_real_, length(sp), length(sp), dimnames = list(sp, sp))
  for (i in seq_along(sp)) for (j in seq_along(sp))
    cm[i, j] <- cor(w[[sp[i]]], w[[sp[j]]], method = "spearman")
  print(round(cm, 3))

  cat("\n--- LAs moving most between A-raw and A-none ---\n")
  mv <- e %>% select(la, spec, rank) %>%
    pivot_wider(names_from = spec, values_from = rank) %>%
    mutate(shift = `A-none` - `A-raw`) %>%   # +ve = worse without control
    arrange(desc(abs(shift)))
  cat("(positive shift = LA ranks WORSE when absence is not controlled)\n")
  print(as.data.frame(head(mv, 10)), digits = 3, row.names = FALSE)

  cat("\n--- Top 10 LAs under each specification ---\n")
  for (s in sp) {
    top <- e %>% filter(spec == s) %>% arrange(rank) %>%
      slice_head(n = 10) %>% pull(la)
    cat(sprintf("%-9s: %s\n", s, paste(top, collapse = ", ")))
  }
}

sink(here::here("revisions", "r27_output.txt"), split = TRUE)
report(res_dis, "DISADVANTAGED PUPILS (ATT8SCR_FSM6CLA1A)")
report(res_all, "ALL PUPILS (ATT8SCR)")

# Where does B&H sit on absence, for context?
cat("\n\n--- Context: B&H absence vs national ---\n")
ctx <- d %>%
  group_by(LANAME) %>%
  summarise(mean_abs = mean(PERCTOT, na.rm = TRUE),
            mean_exp_abs_L = mean(exp(log_exp_abs_L), na.rm = TRUE),
            mean_exp_abs_NoL = mean(exp(log_exp_abs_NoL), na.rm = TRUE),
            mean_fsm = mean(PTFSM6CLA1A, na.rm = TRUE),
            .groups = "drop") %>%
  mutate(abs_rank = rank(mean_abs))   # 1 = lowest absence = best
cat("National mean absence:", round(mean(d$PERCTOT, na.rm = TRUE), 2), "%\n")
print(as.data.frame(ctx %>% filter(grepl("Brighton", LANAME))),
      digits = 4, row.names = FALSE)
cat("(abs_rank 1 = lowest absence; n LAs =", nrow(ctx), ")\n")
sink()

saveRDS(list(dis = res_dis$effects, all = res_all$effects,
             context = ctx),
        OUT)
cat("\nSaved to", OUT, "\n")
