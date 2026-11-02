# 21_value_added_review.R - Is the value-added measure specified well?
# ---------------------------------------------------------------------------
# The value-added (VA) measure is the residual of the published model
# (data/models_imputed.rds): observed Attainment 8 minus the model's
# prediction, back-transformed with the lognormal correction, averaged over
# a school's years. This script tests the specification behind it, on
# exactly the published model's rows (data/model_data_imputed.rds):
#
#   1. Importance  - each term dropped in turn: change in fit (ML AIC), its
#                    standardised effect, and how far school VA moves.
#   2. Omissions   - how strongly the published VA correlates with
#                    candidate variables the model leaves out. A VA that
#                    tracks neighbourhood education, say, is crediting
#                    schools with their surroundings.
#   3. Variants    - removals (workforce, Ofsted), additions (neighbourhood
#                    education, governance route, trust membership, SEN,
#                    mobility, single-sex, size, religious character) and a
#                    combined candidate, each compared with the published
#                    VA school by school.
#   4. Design      - VA within the local authority (the published one, which
#                    nets off the LA effect) against VA against England, and
#                    how much of VA is persistent school signal (a school
#                    random intercept).
#
# Output: data/value_added_review.rds, read by value_added_review.qmd.
#   Rscript R/21_value_added_review.R        (about 20 minutes: ~60 lmer fits)
# ---------------------------------------------------------------------------

suppressPackageStartupMessages({ library(dplyr); library(tidyr); library(lme4) })
source(here::here("R", "19_trust_effects.R"))   # route_of(), trust_by_year(), attach_trusts()

CTRL <- lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 20000))
YEARS <- c("2021-22", "2022-23", "2023-24", "2024-25")

PUB_FIXED <- c("log(PTFSM6CLA1A)", "log(PERCTOT)", "log(PNUMEAL)", "ks2_c", "ADMPOL_PT",
               "gorard_segregation", "remained_in_the_same_school",
               "teachers_on_leadership_pay_range_percent", "log(average_number_of_days_taken)")
WORKFORCE <- c("remained_in_the_same_school", "teachers_on_leadership_pay_range_percent",
               "log(average_number_of_days_taken)")
RE_YEAR <- "(1 | year_label)"; RE_OFSTED <- "(1 | OFSTEDRATING_1)"; RE_PLACE <- "(1 | gor_name/LANAME)"
OUTCOMES <- c(all = "ATT8SCR", disadvantaged = "ATT8SCR_FSM6CLA1A")

TERM_LABELS <- c(
  "log(PTFSM6CLA1A)" = "Disadvantage (% FSM6)", "log(PERCTOT)" = "Absence",
  "log(PNUMEAL)" = "English as an additional language", "ks2_c" = "Prior attainment (KS2)",
  "ADMPOL_PT" = "Admissions (selective)", "gorard_segregation" = "Segregation (LA)",
  "remained_in_the_same_school" = "Teacher retention",
  "teachers_on_leadership_pay_range_percent" = "Teachers on leadership pay",
  "log(average_number_of_days_taken)" = "Teacher sickness days",
  "OFSTEDRATING_1" = "Ofsted rating (random)", "LANAME" = "Local authority (random)",
  "z_l4" = "Neighbourhood education (% level 4+)", "route" = "Governance route",
  "in_mat" = "In a multi-academy trust", "sen_ehcp" = "SEN with EHCP %",
  "sen_support" = "SEN support %", "mobile" = "Pupil mobility %",
  "single_sex" = "Single-sex school", "girls_pct" = "Share of girls %", "log(TOTPUPS)" = "School size",
  "religious" = "Religious character")

# ---- Data: the published rows, plus every candidate ----------------------
review_data <- function() {
  d <- readRDS(here::here("data", "model_data_imputed.rds")) |>
    mutate(URN = as.integer(URN)) |>
    filter(PTFSM6CLA1A > 0, PERCTOT > 0, PNUMEAL > 0, average_number_of_days_taken > 0)
  cens <- readRDS(here::here("data", "census", "school_census_context.rds")) |>
    transmute(URN = as.integer(URN), l4 = cens_l4plus_k150)
  d <- d |>
    left_join(cens, by = "URN") |>
    attach_trusts(trust_by_year()) |>
    mutate(
      OFSTEDRATING_1 = factor(OFSTEDRATING_1, levels = c("Outstanding", "Good",
                              "Requires Improvement", "Inadequate")),
      year_label = factor(year_label), gor_name = factor(gor_name), LANAME = factor(LANAME),
      z_l4 = (l4 - mean(l4, na.rm = TRUE)) / sd(l4, na.rm = TRUE),
      route = factor(route, c("Maintained", "Converter academy", "Sponsor-led academy",
                              "Free school, UTC or studio")),
      sen_ehcp = PSENELSE, sen_support = PSENELK,
      mobile = 100 - PTNMOB, girls_pct = PNORG,
      single_sex = factor(ifelse(GENDER %in% c("Boys", "Girls"), GENDER, "Mixed"),
                          c("Mixed", "Girls", "Boys")),
      religious = !is.na(RELCHAR) & !RELCHAR %in% c("None", "Does not apply"))
  contrasts(d$OFSTEDRATING_1) <- contr.treatment(levels(d$OFSTEDRATING_1))
  # one common sample, so every comparison is on the same school-years
  keep <- complete.cases(d[, c("ATT8SCR", "ks2_c", "ADMPOL_PT", "gorard_segregation",
                               "remained_in_the_same_school",
                               "teachers_on_leadership_pay_range_percent",
                               "OFSTEDRATING_1", "z_l4", "route", "mobile")])
  message(sprintf("Common sample: %d of %d school-years", sum(keep), nrow(d)))
  d[keep, ]
}

fml <- function(y, fixed, re) as.formula(paste0("log(", y, ") ~ ", paste(fixed, collapse = " + "),
                                                " + ", paste(re, collapse = " + ")))
fit <- function(f, d, reml = TRUE) lmer(f, data = d, REML = reml, control = CTRL)

#' School-year VA in Attainment 8 points, the published way: observed minus
#' the bias-corrected prediction. re = NULL predicts with every random
#' effect (VA within the LA); a formula leaves some out.
va_points <- function(m, d, y, re = NULL) {
  lp <- if (is.null(re)) fitted(m) else predict(m, newdata = d, re.form = re)
  d[[y]] - exp(lp) * exp(0.5 * sigma(m)^2)
}

school_va <- function(d, v) tibble(URN = d$URN, LANAME = as.character(d$LANAME), v = v) |>
  group_by(URN, LANAME) |> summarise(va = mean(v), years = n(), .groups = "drop")

#' How a variant's school VA differs from the published one's
compare_va <- function(base, alt) {
  j <- inner_join(base, alt, by = c("URN", "LANAME"), suffix = c("_p", "_a"))
  q <- function(x) ntile(x, 5)
  bot <- function(x) x <= quantile(x, 0.1)
  tibble(r = cor(j$va_p, j$va_a), rho = cor(j$va_p, j$va_a, method = "spearman"),
         mean_abs_change = mean(abs(j$va_a - j$va_p)),
         p90_abs_change = unname(quantile(abs(j$va_a - j$va_p), 0.9)),
         moved_quintile = mean(q(j$va_p) != q(j$va_a)),
         bottom_decile_kept = sum(bot(j$va_p) & bot(j$va_a)) / sum(bot(j$va_p)))
}

la_eff <- function(m, la = "Brighton and Hove") {
  re <- ranef(m)[["LANAME:gor_name"]]
  x <- tibble(key = rownames(re), eff = re[, 1]) |> mutate(rank = rank(-eff), n = n())
  c(sd_la = attr(VarCorr(m)[["LANAME:gor_name"]], "stddev")[[1]],
    bh_eff = x$eff[startsWith(x$key, paste0(la, ":"))],
    bh_rank = x$rank[startsWith(x$key, paste0(la, ":"))], n_la = nrow(x))
}

# ---- 1. Importance of each published term --------------------------------
importance <- function(d, y) {
  re <- c(RE_YEAR, RE_OFSTED, RE_PLACE)
  full <- fit(fml(y, PUB_FIXED, re), d, reml = FALSE)
  base_va <- school_va(d, va_points(full, d, y))
  X <- model.matrix(full)
  out <- lapply(c(PUB_FIXED, "OFSTEDRATING_1", "LANAME"), function(t) {
    if (t %in% PUB_FIXED) {
      m <- fit(fml(y, setdiff(PUB_FIXED, t), re), d, reml = FALSE)
      col <- colnames(X)[colnames(X) == t | startsWith(colnames(X), t)][1]
      std <- unname(fixef(full)[col] * sd(X[, col]))
    } else {
      re2 <- if (t == "OFSTEDRATING_1") c(RE_YEAR, RE_PLACE) else c(RE_YEAR, RE_OFSTED, "(1 | gor_name)")
      m <- fit(fml(y, PUB_FIXED, re2), d, reml = FALSE)
      std <- if (t == "OFSTEDRATING_1") attr(VarCorr(full)$OFSTEDRATING_1, "stddev")[[1]]
             else attr(VarCorr(full)[["LANAME:gor_name"]], "stddev")[[1]]
    }
    cmp <- compare_va(base_va, school_va(d, va_points(m, d, y)))
    tibble(term = t, label = TERM_LABELS[[t]], std_effect_pct = 100 * std,
           delta_aic = AIC(m) - AIC(full), r_va = cmp$r, mean_abs_change = cmp$mean_abs_change,
           moved_quintile = cmp$moved_quintile)
  })
  bind_rows(out) |> arrange(desc(delta_aic))
}

# ---- 2. What the published VA correlates with ----------------------------
omissions <- function(d, base_va) {
  sch <- d |> group_by(URN) |>
    summarise(l4 = mean(l4), sen_ehcp = mean(sen_ehcp), sen_support = mean(sen_support),
              mobile = mean(mobile, na.rm = TRUE), size = mean(log(TOTPUPS)),
              sponsored = mean(route == "Sponsor-led academy"),
              converter = mean(route == "Converter academy"),
              in_mat = mean(in_mat), religious = mean(religious),
              girls = mean(single_sex == "Girls"), boys = mean(single_sex == "Boys"),
              retention = mean(remained_in_the_same_school),
              fsm = mean(PTFSM6CLA1A), ks2 = mean(ks2_c), .groups = "drop") |>
    inner_join(base_va, by = "URN")
  vars <- c(l4 = "Neighbourhood education (% level 4+)", sen_ehcp = "SEN with EHCP %",
            sen_support = "SEN support %", mobile = "Pupil mobility %", size = "School size (log)",
            sponsored = "Sponsor-led academy", converter = "Converter academy",
            in_mat = "In a multi-academy trust", religious = "Religious character",
            girls = "Girls' school", boys = "Boys' school", retention = "Teacher retention",
            fsm = "Disadvantage (% FSM6)", ks2 = "Prior attainment (KS2)")
  tibble(var = names(vars), label = unname(vars),
         r = sapply(names(vars), function(v) cor(sch[[v]], sch$va, use = "pair")),
         in_model = names(vars) %in% c("retention", "fsm", "ks2"))
}

# ---- 3. Variants ----------------------------------------------------------
SPECS <- function() {
  re_all <- c(RE_YEAR, RE_OFSTED, RE_PLACE); re_no_ofsted <- c(RE_YEAR, RE_PLACE)
  intake <- setdiff(PUB_FIXED, WORKFORCE)
  list(
    published        = list(f = PUB_FIXED, re = re_all, what = "Published model"),
    no_workforce     = list(f = intake, re = re_all, what = "Without the three workforce terms"),
    no_ofsted        = list(f = PUB_FIXED, re = re_no_ofsted, what = "Without the Ofsted rating"),
    intake_context   = list(f = intake, re = re_no_ofsted, what = "Without workforce or Ofsted (intake and context only)"),
    no_absence       = list(f = setdiff(PUB_FIXED, "log(PERCTOT)"), re = re_all, what = "Without absence"),
    plus_l4          = list(f = c(PUB_FIXED, "z_l4"), re = re_all, what = "Published + neighbourhood education"),
    plus_route       = list(f = c(PUB_FIXED, "route"), re = re_all, what = "Published + governance route"),
    plus_mat         = list(f = c(PUB_FIXED, "in_mat"), re = re_all, what = "Published + multi-academy trust"),
    plus_sen         = list(f = c(PUB_FIXED, "sen_ehcp", "sen_support"), re = re_all, what = "Published + SEN (EHCP and support)"),
    plus_mobility    = list(f = c(PUB_FIXED, "mobile"), re = re_all, what = "Published + pupil mobility"),
    plus_single_sex  = list(f = c(PUB_FIXED, "single_sex"), re = re_all, what = "Published + single-sex school"),
    plus_girls_pct   = list(f = c(PUB_FIXED, "girls_pct"), re = re_all, what = "Published + share of girls"),
    plus_size        = list(f = c(PUB_FIXED, "log(TOTPUPS)"), re = re_all, what = "Published + school size"),
    plus_religious   = list(f = c(PUB_FIXED, "religious"), re = re_all, what = "Published + religious character"),
    candidate        = list(f = c(intake, "z_l4", "sen_ehcp", "sen_support", "mobile", "single_sex"),
                            re = re_no_ofsted,
                            what = "Candidate: intake and context, + neighbourhood education, SEN, mobility, single-sex"))
}

run_variants <- function(d, y) {
  specs <- SPECS()
  fits <- lapply(names(specs), function(k) {
    message("  ", y, ": ", k)
    s <- specs[[k]]
    m <- fit(fml(y, s$f, s$re), d)
    list(m = m, aic = AIC(refitML(m)), va = school_va(d, va_points(m, d, y)))
  }) |> setNames(names(specs))
  base <- fits$published$va
  summary <- bind_rows(lapply(names(fits), function(k) {
    m <- fits[[k]]$m
    r2 <- performance::r2_nakagawa(m, tolerance = 1e-10)
    bind_cols(tibble(spec = k, what = specs[[k]]$what, n_fixed = length(specs[[k]]$f),
                     delta_aic = fits[[k]]$aic - fits$published$aic,
                     r2_marginal = r2$R2_marginal, r2_conditional = r2$R2_conditional,
                     sd_resid = sigma(m)),
              as_tibble(as.list(la_eff(m))), compare_va(base, fits[[k]]$va))
  }))
  coefs <- bind_rows(lapply(names(fits), function(k) {
    s <- summary(fits[[k]]$m)$coefficients
    tibble(spec = k, term = rownames(s), estimate = s[, 1], se = s[, 2], t = s[, 3])
  }))
  list(summary = summary, coefs = coefs,
       va = bind_rows(lapply(names(fits), function(k) fits[[k]]$va |> mutate(spec = k))),
       models = lapply(fits, `[[`, "m"))
}

# ---- 4. Design: within-LA against national VA, and the school signal ----
design <- function(d, y, m_pub, m_cand) {
  no_place <- ~ (1 | year_label) + (1 | OFSTEDRATING_1)
  pub_la  <- school_va(d, va_points(m_pub, d, y))
  pub_nat <- school_va(d, va_points(m_pub, d, y, no_place))
  cand_la  <- school_va(d, va_points(m_cand, d, y))
  cand_nat <- school_va(d, va_points(m_cand, d, y, ~ (1 | year_label)))
  # How much of the residual is a persistent school effect
  ms <- fit(update(formula(m_pub), . ~ . + (1 | URN)), d)
  vc <- as.data.frame(VarCorr(ms))
  tau2 <- vc$vcov[vc$grp == "URN"]; sig2 <- sigma(ms)^2
  yrs <- d |> count(URN) |> pull(n)
  blup <- tibble(URN = as.integer(rownames(ranef(ms)$URN)), blup = ranef(ms)$URN[, 1])
  list(within_vs_national = bind_rows(
         compare_va(pub_la, pub_nat) |> mutate(spec = "published"),
         compare_va(cand_la, cand_nat) |> mutate(spec = "candidate"),
         # the whole of the recommended change, from where we are now
         compare_va(pub_la, cand_nat) |> mutate(spec = "published within LA vs candidate national")),
       schools = pub_la |> rename(pub_la = va) |>
         inner_join(pub_nat |> select(URN, pub_nat = va), by = "URN") |>
         inner_join(cand_la |> select(URN, cand_la = va), by = "URN") |>
         inner_join(cand_nat |> select(URN, cand_nat = va), by = "URN"),
       school_sd = sqrt(tau2), resid_sd = sqrt(sig2), icc = tau2 / (tau2 + sig2),
       reliability_mean = mean(tau2 / (tau2 + sig2 / yrs)),
       blup_vs_mean = cor(blup$blup, pub_la$va[match(blup$URN, pub_la$URN)]))
}

build_value_added_review <- function() {
  d <- review_data()
  out <- list(n = nrow(d), n_schools = n_distinct(d$URN))
  for (k in names(OUTCOMES)) {
    y <- OUTCOMES[[k]]
    dk <- d[!is.na(d[[y]]) & d[[y]] > 0, ]
    message("== ", k, " (", nrow(dk), " rows)")
    message(" importance ...")
    imp <- importance(dk, y)
    message(" variants ...")
    v <- run_variants(dk, y)
    message(" design ...")
    des <- design(dk, y, v$models$published, v$models$candidate)
    om <- omissions(dk, v$va |> filter(spec == "published") |> select(URN, LANAME, va))
    # Every Sussex school: published VA within its LA and against England,
    # the same for the candidate, and the single-change variants
    names_ <- dk |> transmute(URN, SCHNAME = as.character(SCHNAME)) |> group_by(URN) |> slice(1) |> ungroup()
    bh <- des$schools |>
      inner_join(v$va |> filter(spec %in% c("no_workforce", "no_ofsted", "plus_l4", "plus_mobility")) |>
                   select(URN, spec, va) |> pivot_wider(names_from = spec, values_from = va), by = "URN") |>
      inner_join(names_, by = "URN") |>
      filter(LANAME %in% c("Brighton and Hove", "East Sussex", "West Sussex"))
    route_va <- v$va |> filter(spec %in% c("published", "intake_context", "candidate")) |>
      inner_join(dk |> group_by(URN) |> summarise(route = names(which.max(table(route))), .groups = "drop"),
                 by = "URN") |>
      group_by(spec, route) |> summarise(va = mean(va), schools = n(), .groups = "drop")
    v$models <- NULL
    out[[k]] <- list(importance = imp, variants = v, design = des, omissions = om,
                     sussex = bh, route_va = route_va)
  }
  out$built_at <- Sys.time()
  saveRDS(out, here::here("data", "value_added_review.rds"))
  invisible(out)
}

if (sys.nframe() == 0) build_value_added_review()
