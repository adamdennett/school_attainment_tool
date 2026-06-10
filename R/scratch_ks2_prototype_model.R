# scratch_ks2_prototype_model.R - quick sanity-check models on the new KS2 panel
# (exploratory only; proper modelling will live in 21_fit_ks2_models.R)
suppressMessages({
  library(dplyr)
  library(lme4)
  library(performance)
})

p <- readRDS(here::here("data", "ks2", "ks2_panel.rds")) %>%
  mutate(across(c(READ_AVERAGE, MAT_AVERAGE, PERCTOT, PPERSABS10,
                  PTFSM6CLA1A, PTEALGRP2, PTMOBN, TELIG), as.numeric)) %>%
  filter(!is.na(READ_AVERAGE), !is.na(MAT_AVERAGE), !is.na(PERCTOT),
         !is.na(PTFSM6CLA1A), !is.na(PTEALGRP2), !is.na(PTMOBN),
         !is.na(OFSTEDRATING_external), PERCTOT > 0)

cat("Model rows:", nrow(p), " schools:", n_distinct(p$URN), "\n\n")

zs <- function(x) as.numeric(scale(x))
p <- p %>%
  mutate(
    z_read = zs(READ_AVERAGE), z_mat = zs(MAT_AVERAGE),
    z_abs = zs(PERCTOT), z_pers = zs(PPERSABS10),
    z_fsm = zs(PTFSM6CLA1A), z_eal = zs(PTEALGRP2),
    z_mob = zs(PTMOBN), z_size = zs(TELIG),
    ofsted = factor(OFSTEDRATING_external)
  )

m_read <- lmer(z_read ~ z_abs + z_pers + z_fsm + z_eal + z_mob + z_size +
                 ofsted + year_label + (1 | LEA), data = p, REML = FALSE)
cat("=== Reading scaled score (standardised) ===\n")
print(round(summary(m_read)$coefficients[, 1:3], 3))
print(r2(m_read))

m_mat <- update(m_read, z_mat ~ .)
cat("\n=== Maths scaled score (standardised) ===\n")
print(round(summary(m_mat)$coefficients[, 1:3], 3))
print(r2(m_mat))

cat("\nAbsence vs FSM standardised coefficient ratio (reading): ",
    round(abs(fixef(m_read)["z_abs"]) / abs(fixef(m_read)["z_fsm"]), 2), "\n")
cat("Absence vs FSM standardised coefficient ratio (maths):   ",
    round(abs(fixef(m_mat)["z_abs"]) / abs(fixef(m_mat)["z_fsm"]), 2), "\n")
