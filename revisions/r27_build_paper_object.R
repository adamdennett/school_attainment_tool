# Consolidate the R2.7 analysis into a single compact object the paper loads,
# so RPE_Paper.qmd does not refit four LMEs at render time (mirrors the
# pre-fitted models_imputed.rds pattern).
#
# Produces data/r27_la_absence.rds — a named list with everything §5.2 needs.
# Regenerate by running revisions/r27_absence_control.R first, then this.

suppressPackageStartupMessages({ library(tidyverse); library(here) })

res <- readRDS(here::here("revisions", "r27_results.rds"))
panel <- readRDS(here::here("data", "panel_data.rds"))

d <- panel %>%
  filter(MINORGROUP %in% c("Academy", "Maintained school"),
         !is.na(ATT8SCR), ATT8SCR > 0,
         PTFSM6CLA1A > 0, PERCTOT > 0, PNUMEAL > 0,
         !is.na(OFSTEDRATING_1), !is.na(gor_name), !is.na(LANAME))

mean_dis <- mean(d$ATT8SCR_FSM6CLA1A[d$ATT8SCR_FSM6CLA1A > 0], na.rm = TRUE)
mean_all <- mean(d$ATT8SCR, na.rm = TRUE)

# ---- B&H effect tables under each specification -----------------------
spec_labels <- c(`A-none` = "No absence control",
                 `A-raw` = "Absence controlled (main model)",
                 `A-expNoL` = "Intake-predicted absence controlled",
                 `A-expL` = "Intake-predicted absence (place-absorbed)")

bh_table <- function(eff, base) {
  eff %>%
    filter(grepl("Brighton", la)) %>%
    transmute(spec, label = spec_labels[spec],
              effect, ci_lo, ci_hi, sig, rank,
              att8_pts = base * (exp(effect) - 1),
              n_las = n_las) %>%
    arrange(match(spec, c("A-none", "A-raw", "A-expNoL", "A-expL")))
}

bh_dis <- bh_table(res$dis, mean_dis)
bh_all <- bh_table(res$all, mean_all)

# ---- Rank-correlation and significance-loss (all LAs) -----------------
rank_cor <- function(eff, a, b) {
  w <- eff %>% select(la, spec, effect) %>%
    pivot_wider(names_from = spec, values_from = effect)
  cor(w[[a]], w[[b]], method = "spearman")
}
sig_loss <- function(eff) {
  s <- eff %>% select(la, spec, sig) %>%
    pivot_wider(names_from = spec, values_from = sig)
  sum(s$`A-raw` & !s$`A-none`)
}

# ---- Intake-excess context --------------------------------------------
ctx <- res$context %>% filter(grepl("Brighton", LANAME))
nat_fsm <- mean(d$PTFSM6CLA1A, na.rm = TRUE)

# ---- Absence trend and by-year worst-rank -----------------------------
la_yr <- d %>%
  group_by(year_label, LANAME) %>%
  summarise(abs = mean(PERCTOT, na.rm = TRUE), .groups = "drop")
nat_yr <- la_yr %>% group_by(year_label) %>%
  summarise(nat = mean(abs), .groups = "drop")
bh_yr <- la_yr %>% filter(grepl("Brighton", LANAME)) %>%
  select(year_label, bh = abs) %>%
  left_join(nat_yr, by = "year_label") %>%
  group_by(year_label) %>% mutate() %>% ungroup()
# worst-rank per year (1 = worst absence)
worst <- la_yr %>% group_by(year_label) %>%
  mutate(worst_rank = rank(-abs), n = n()) %>% ungroup() %>%
  filter(grepl("Brighton", LANAME)) %>%
  select(year_label, bh_abs = abs, worst_rank, n_las = n)

yrs <- levels(factor(la_yr$year_label))
first_yr <- yrs[1]; last_yr <- yrs[length(yrs)]
chg <- la_yr %>% filter(year_label %in% c(first_yr, last_yr)) %>%
  pivot_wider(names_from = year_label, values_from = abs) %>%
  rename(y1 = 2, y2 = 3) %>% mutate(change = y2 - y1) %>% filter(!is.na(change))
bh_chg <- chg %>% filter(grepl("Brighton", LANAME)) %>% pull(change)
bh_improve_rank <- rank(chg$change)[which(grepl("Brighton", chg$LANAME))]

out <- list(
  bh_dis = bh_dis,
  bh_all = bh_all,
  n_las_dis = unique(res$dis$n_las)[1],
  n_las_all = unique(res$all$n_las)[1],
  spearman_dis_raw_none = rank_cor(res$dis, "A-raw", "A-none"),
  spearman_dis_raw_expL = rank_cor(res$dis, "A-raw", "A-expL"),
  spearman_dis_none_expNoL = rank_cor(res$dis, "A-none", "A-expNoL"),
  sig_loss_dis = sig_loss(res$dis),
  sig_loss_all = sig_loss(res$all),
  bh_obs_abs = ctx$mean_abs,
  bh_exp_abs_intake = ctx$mean_exp_abs_NoL,
  bh_exp_abs_place = ctx$mean_exp_abs_L,
  bh_mean_fsm = ctx$mean_fsm,
  nat_mean_fsm = nat_fsm,
  absence_by_year = worst,
  trend_national_change = mean(chg$change),
  trend_bh_change = bh_chg,
  trend_bh_improve_rank = bh_improve_rank,
  trend_n_improved = sum(chg$change < 0),
  trend_n_las = nrow(chg),
  trend_pct_improved = mean(chg$change < 0),
  att8_gap_dis = bh_dis$att8_pts[bh_dis$spec == "A-raw"] -
                 bh_dis$att8_pts[bh_dis$spec == "A-none"]
)

saveRDS(out, here::here("data", "r27_la_absence.rds"))
cat("Saved data/r27_la_absence.rds\n")
cat(sprintf("Disadvantaged: raw rank %d (of %d), none rank %d; gap %.2f ATT8 pts\n",
            bh_dis$rank[bh_dis$spec == "A-raw"], out$n_las_dis,
            bh_dis$rank[bh_dis$spec == "A-none"], out$att8_gap_dis))
cat(sprintf("Absence excess over intake: %.2f pp | trend: B&H %+.2f vs nat %+.2f, improve-rank %d/%d\n",
            out$bh_obs_abs - out$bh_exp_abs_intake, out$trend_bh_change,
            out$trend_national_change, out$trend_bh_improve_rank, out$trend_n_las))
