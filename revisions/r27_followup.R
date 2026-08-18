# Follow-up quantities for the R2.7 write-up:
#  1. Effects translated into Attainment 8 points
#  2. B&H absence rank by year (checks the "2nd worst in England" claim)
#  3. Size of B&H's absence excess over intake prediction

suppressPackageStartupMessages({
  library(tidyverse); library(here)
})

res <- readRDS(here::here("revisions", "r27_results.rds"))
panel <- readRDS(here::here("data", "panel_data.rds"))

d <- panel %>%
  filter(MINORGROUP %in% c("Academy", "Maintained school"),
         !is.na(ATT8SCR), ATT8SCR > 0,
         PTFSM6CLA1A > 0, PERCTOT > 0, PNUMEAL > 0,
         !is.na(OFSTEDRATING_1), !is.na(gor_name), !is.na(LANAME))

mean_dis <- mean(d$ATT8SCR_FSM6CLA1A[d$ATT8SCR_FSM6CLA1A > 0], na.rm = TRUE)
mean_all <- mean(d$ATT8SCR, na.rm = TRUE)

cat("=== 1. Effects in Attainment 8 points ===\n")
cat(sprintf("National mean ATT8: all pupils %.1f | disadvantaged %.1f\n\n",
            mean_all, mean_dis))

for (grp in c("dis", "all")) {
  base <- if (grp == "dis") mean_dis else mean_all
  lab  <- if (grp == "dis") "DISADVANTAGED" else "ALL PUPILS"
  cat("---", lab, "---\n")
  bh <- res[[grp]] %>% filter(grepl("Brighton", la)) %>%
    mutate(att8_pts = base * (exp(effect) - 1)) %>%
    select(spec, effect, att8_pts, rank)
  print(as.data.frame(bh), digits = 3, row.names = FALSE)
  raw  <- bh$att8_pts[bh$spec == "A-raw"]
  none <- bh$att8_pts[bh$spec == "A-none"]
  cat(sprintf("  Gap (A-raw minus A-none): %.2f ATT8 points\n\n", raw - none))
}

cat("\n=== 2. B&H absence rank by year ===\n")
by_yr <- d %>%
  group_by(year_label, LANAME) %>%
  summarise(mean_abs = mean(PERCTOT, na.rm = TRUE), .groups = "drop") %>%
  group_by(year_label) %>%
  mutate(worst_rank = rank(-mean_abs),   # 1 = worst absence
         n_las = n()) %>%
  ungroup()

print(as.data.frame(
  by_yr %>% filter(grepl("Brighton", LANAME)) %>%
    select(year_label, mean_abs, worst_rank, n_las)),
  digits = 4, row.names = FALSE)

cat("\nWorst 5 LAs for absence in latest year:\n")
latest <- max(levels(factor(d$year_label)))
print(as.data.frame(
  by_yr %>% filter(year_label == latest) %>% arrange(worst_rank) %>%
    slice_head(n = 5) %>% select(LANAME, mean_abs, worst_rank)),
  digits = 4, row.names = FALSE)

cat("\n\n=== 3. B&H absence excess over intake prediction ===\n")
ctx <- res$context %>% filter(grepl("Brighton", LANAME))
cat(sprintf("Observed absence:                    %.2f%%\n", ctx$mean_abs))
cat(sprintf("Expected (stage 1 WITH LA effect):   %.2f%%  -> excess %.2f pp\n",
            ctx$mean_exp_abs_L, ctx$mean_abs - ctx$mean_exp_abs_L))
cat(sprintf("Expected (stage 1 intake only):      %.2f%%  -> excess %.2f pp\n",
            ctx$mean_exp_abs_NoL, ctx$mean_abs - ctx$mean_exp_abs_NoL))
cat(sprintf("B&H mean FSM: %.1f%% (national %.1f%%)\n",
            ctx$mean_fsm, mean(d$PTFSM6CLA1A, na.rm = TRUE)))

cat("\n\n=== 4. How many LAs change significance status? ===\n")
for (grp in c("dis", "all")) {
  lab <- if (grp == "dis") "DISADVANTAGED" else "ALL PUPILS"
  s <- res[[grp]] %>% select(la, spec, sig) %>%
    pivot_wider(names_from = spec, values_from = sig)
  cat(sprintf("%s: sig under A-raw = %d | sig under A-none = %d | ",
              lab, sum(s$`A-raw`), sum(s$`A-none`)))
  cat(sprintf("lose significance = %d\n", sum(s$`A-raw` & !s$`A-none`)))
}
