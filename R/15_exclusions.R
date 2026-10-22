# 15_exclusions.R - Do exclusions explain the attendance problem?
# ---------------------------------------------------------------------------
# Brighton & Hove's council has started asking how exclusions work in the
# city. The implicit theory - schools exclude, exclusion costs attendance,
# attendance costs attainment - is testable from published data, and this
# script assembles what is needed to test it.
#
# Three sources, all Open Government Licence v3.0:
#
#   absence_2term_school   school level, autumn+spring, 2016/17-2025/26.
#                          Carries sess_auth_excluded: sessions missed
#                          because the pupil was suspended (register code
#                          E). This is the only way to measure exclusion's
#                          contribution to absence directly rather than by
#                          assumption.
#   exclusions_school      school level suspensions and permanent
#                          exclusions, 2006/07-2024/25.
#   exclusions_la          LA level, carrying sessions_susp, used to check
#                          the register-based figure against the exclusions
#                          collection's own count of missed sessions.
#
# The exclusions collection runs one year behind absence and KS4, so the
# school-level joins stop at 2024/25 while the absence decomposition runs
# to 2025/26. Both facts are carried in the output so the document cannot
# quietly present them as the same window.
#
# Writes data/exclusions_absence.rds.
#
# Run from project root.
# ---------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(readr)
})
source(here::here("R", "00b_download_ees_api.R"))

SECONDARY <- "State-funded secondary"

# Years the panel of school characteristics covers, and so the years any
# model controlling for intake can use.
PANEL_YEARS <- c("202122", "202223", "202324", "202425")

# Register codes, in the order the published absence file names them, with
# the labels this analysis uses. Code E - "excluded, no alternative
# provision made" - is the one the whole question turns on.
REASONS <- c(
  sess_auth_illness                = "Illness",
  sess_unauth_other                = "Unauthorised, other",
  sess_auth_other                  = "Authorised, other",
  sess_auth_appointments           = "Medical appointments",
  sess_auth_temp_reduced_timetable = "Reduced timetable",
  sess_unauth_holiday              = "Unauthorised holiday",
  sess_unauth_late                 = "Late",
  sess_auth_excluded               = "Suspended (code E)",
  sess_auth_religious              = "Religious observance",
  sess_unauth_noyet                = "No reason yet",
  sess_auth_interview              = "Interview",
  sess_auth_reg_performance        = "Performance or sport",
  sess_auth_study                  = "Study leave",
  sess_auth_traveller              = "Traveller absence",
  sess_auth_holiday                = "Authorised holiday",
  sess_auth_ext_holiday            = "Extended holiday"
)


#' Secondary absence, school level, with exclusion sessions separated out
#'
#' abs_ex_excl is the absence rate a school would post if every session
#' missed through suspension were removed. The gap between it and the
#' headline rate is the whole of exclusion's mechanical contribution.
read_absence <- function() {
  zip <- ees_path("absence_2term_school")
  inner <- "data/Absence_2term_school.csv"
  d <- read_csv(unz(zip, inner), col_types = cols(.default = col_character()),
                progress = FALSE)
  num <- c("enrolments", "enrolments_pa_10_exact", "sess_possible",
           "sess_overall", "sess_authorised", "sess_unauthorised",
           names(REASONS))
  d |>
    filter(education_phase == SECONDARY) |>
    mutate(across(any_of(num), ~suppressWarnings(as.numeric(.x)))) |>
    transmute(
      time_period = as.character(time_period),
      urn         = as.character(school_urn),
      la_name, school_name,
      enrolments, pa = enrolments_pa_10_exact,
      poss = sess_possible, overall = sess_overall,
      authorised = sess_authorised, unauthorised = sess_unauthorised,
      across(any_of(names(REASONS))),
      abs_rate    = 100 * overall / poss,
      excl_pp     = 100 * sess_auth_excluded / poss,
      abs_ex_excl = 100 * (overall - sess_auth_excluded) / poss,
      pa_rate     = 100 * pa / enrolments
    ) |>
    filter(poss > 0)
}


#' Secondary suspensions and permanent exclusions, school level
#'
#' Suppressed cells are published as "x"; as.numeric turns them into NA,
#' which is the right answer - a suppressed school is unknown, not zero.
read_exclusions <- function() {
  zip <- ees_path("exclusions_school")
  inner <- "data/exc_school.csv"
  d <- read_csv(unz(zip, inner), col_types = cols(.default = col_character()),
                progress = FALSE)
  d |>
    filter(education_phase == SECONDARY) |>
    transmute(
      time_period = as.character(time_period),
      urn         = as.character(school_urn),
      la_name,
      headcount          = suppressWarnings(as.numeric(headcount)),
      perm_excl          = suppressWarnings(as.numeric(perm_excl)),
      perm_excl_rate     = suppressWarnings(as.numeric(perm_excl_rate)),
      suspension         = suppressWarnings(as.numeric(suspension)),
      susp_rate          = suppressWarnings(as.numeric(susp_rate)),
      one_plus_susp      = suppressWarnings(as.numeric(one_plus_susp)),
      one_plus_susp_rate = suppressWarnings(as.numeric(one_plus_susp_rate))
    )
}


#' England secondary absence by year, with exclusion separated
#'
#' Summed over sessions rather than averaged over schools, so a 2,000-pupil
#' school counts for twice a 1,000-pupil one - which is what a national
#' rate means.
national_series <- function(abs_sch) {
  abs_sch |>
    group_by(time_period) |>
    summarise(schools  = n(),
              enrol    = sum(enrolments, na.rm = TRUE),
              poss     = sum(poss, na.rm = TRUE),
              overall  = sum(overall, na.rm = TRUE),
              excl     = sum(sess_auth_excluded, na.rm = TRUE),
              illness  = sum(sess_auth_illness, na.rm = TRUE),
              unauth   = sum(unauthorised, na.rm = TRUE),
              pa       = sum(pa, na.rm = TRUE),
              .groups  = "drop") |>
    mutate(abs_rate          = 100 * overall / poss,
           excl_pp           = 100 * excl / poss,
           excl_share_of_abs = 100 * excl / overall,
           illness_share     = 100 * illness / overall,
           unauth_share      = 100 * unauth / overall,
           pa_rate           = 100 * pa / enrol)
}


#' Every absence reason for one year, ranked
reason_split <- function(abs_sch, year) {
  one <- abs_sch |> filter(time_period == year)
  tot <- summarise(one, poss = sum(poss, na.rm = TRUE),
                   overall = sum(overall, na.rm = TRUE))
  one |>
    summarise(across(all_of(names(REASONS)), ~sum(.x, na.rm = TRUE))) |>
    pivot_longer(everything(), names_to = "code", values_to = "sessions") |>
    mutate(reason         = unname(REASONS[code]),
           pct_of_absence = 100 * sessions / tot$overall,
           pp_of_possible = 100 * sessions / tot$poss) |>
    arrange(desc(sessions))
}


#' Join absence, exclusions and the school characteristics panel
#'
#' Schools under 100 on roll are dropped: a rate computed on 40 pupils
#' moves several points when one child is suspended once, and those swings
#' are noise in every correlation below.
build_join <- function(abs_sch, exc) {
  panel <- readRDS(here::here("data", "panel_data.rds")) |>
    filter(MINORGROUP %in% c("Academy", "Maintained school")) |>
    transmute(time_period = gsub("-", "", year_label),
              urn   = as.character(URN),
              SCHNAME, LANAME,
              att8  = ATT8SCR,
              fsm   = PTFSM6CLA1A,
              eal   = PNUMEAL,
              perctot = PERCTOT,
              sen_ehc = PSENELSE,
              sen_sup = PSENELK)

  abs_sch |>
    inner_join(exc, by = c("time_period", "urn"), suffix = c("", ".e")) |>
    filter(enrolments >= 100, !is.na(susp_rate)) |>
    left_join(panel, by = c("time_period", "urn"))
}


#' How far a suspension rate goes once the intake is held still
#'
#' Four specifications on the same rows, from the raw cross-section to a
#' school fixed-effects model. The last is the one that matters: it asks
#' whether a school that suspends more THAN IT USUALLY DOES also has worse
#' attendance than it usually does, which differences away everything
#' fixed about its intake, its catchment and its leadership.
absence_models <- function(j) {
  d <- j |> filter(time_period %in% PANEL_YEARS, !is.na(fsm), fsm > 0)
  balanced <- d |> count(urn) |> filter(n == length(PANEL_YEARS))
  fe_d <- semi_join(d, balanced, by = "urn")

  spec <- function(label, fit, data) {
    cf <- coef(summary(fit))["susp_rate", ]
    tibble(spec = label, beta = cf[[1]], se = cf[[2]], t = cf[[3]],
           r2 = summary(fit)$r.squared, n = nrow(data))
  }

  bind_rows(
    spec("Raw", lm(abs_ex_excl ~ susp_rate, d), d),
    spec("+ disadvantage",
         lm(abs_ex_excl ~ susp_rate + fsm, d), d),
    spec("+ disadvantage, EAL, SEN, year",
         lm(abs_ex_excl ~ susp_rate + fsm + eal + sen_ehc + sen_sup +
              factor(time_period), d), d),
    spec("+ school fixed effects",
         lm(abs_ex_excl ~ susp_rate + factor(time_period) + factor(urn), fe_d),
         fe_d)
  ) |>
    mutate(schools_fe = nrow(balanced))
}


#' Does suspension explain attainment once absence is already in?
#'
#' The specification follows the Lever report: log Attainment 8 on logged
#' rates. The comparison that answers the question is the R-squared each
#' term adds on top of the other.
attainment_models <- function(j) {
  d <- j |> filter(time_period %in% PANEL_YEARS,
                   !is.na(att8), att8 > 0, fsm > 0, perctot > 0, eal > 0)
  base   <- lm(log(att8) ~ log(fsm) + log(eal), d)
  w_abs  <- update(base, . ~ . + log(perctot))
  w_susp <- update(base, . ~ . + susp_rate)
  both   <- update(base, . ~ . + log(perctot) + susp_rate)
  tibble(
    model = c("Intake only", "+ absence", "+ suspension rate",
              "+ both", "Absence added on top of suspension",
              "Suspension added on top of absence"),
    r2 = c(summary(base)$r.squared, summary(w_abs)$r.squared,
           summary(w_susp)$r.squared, summary(both)$r.squared,
           summary(both)$r.squared - summary(w_susp)$r.squared,
           summary(both)$r.squared - summary(w_abs)$r.squared),
    n = nrow(d)
  )
}


#' Brighton & Hove's absence against England's, split by register code
#'
#' Measured in percentage points of possible sessions rather than as a
#' share of absence. Shares are compositional: a city with more absence
#' can have a smaller share of it coming from illness while still having
#' far more illness absence. Percentage points of the same denominator
#' subtract cleanly, so the gaps add up to the total gap and the
#' decomposition is exact.
reason_gap <- function(abs_sch, year) {
  roll <- function(d) d |>
    summarise(poss    = sum(poss, na.rm = TRUE),
              overall = sum(overall, na.rm = TRUE),
              across(all_of(names(REASONS)), ~sum(.x, na.rm = TRUE)),
              .groups = "drop")
  spread <- function(d) d |>
    pivot_longer(all_of(names(REASONS)), names_to = "code", values_to = "sessions") |>
    mutate(pp = 100 * sessions / poss, share = 100 * sessions / overall)

  one <- abs_sch |> filter(time_period == year)
  eng <- spread(roll(one))
  bh  <- spread(roll(filter(one, grepl("Brighton and Hove", la_name))))

  total_gap <- (100 * bh$overall[1] / bh$poss[1]) -
               (100 * eng$overall[1] / eng$poss[1])

  bh |>
    select(code, pp_bh = pp, share_bh = share, sessions_bh = sessions) |>
    left_join(eng |> select(code, pp_eng = pp, share_eng = share), by = "code") |>
    mutate(reason        = unname(REASONS[code]),
           gap_pp        = pp_bh - pp_eng,
           pct_of_gap    = 100 * (pp_bh - pp_eng) / total_gap,
           total_gap     = total_gap,
           abs_bh        = 100 * bh$overall[1] / bh$poss[1],
           abs_eng       = 100 * eng$overall[1] / eng$poss[1]) |>
    arrange(desc(gap_pp))
}


#' Where Brighton & Hove ranks among LAs on each register code
#'
#' Local authorities with under a million possible sessions are dropped:
#' at that size one small school swings a rate enough to make a rank
#' meaningless. 1 = the worst rate in the country on that code.
reason_ranks <- function(abs_sch, year) {
  la <- abs_sch |>
    filter(time_period == year) |>
    group_by(la_name) |>
    summarise(poss    = sum(poss, na.rm = TRUE),
              overall = sum(overall, na.rm = TRUE),
              across(all_of(names(REASONS)), ~sum(.x, na.rm = TRUE)),
              .groups = "drop") |>
    filter(poss > 1e6) |>
    mutate(across(all_of(names(REASONS)), ~100 * .x / poss),
           total = 100 * overall / poss)

  i <- which(grepl("Brighton and Hove", la$la_name))
  tibble(
    code = c(names(REASONS), "total"),
    rank_bh = vapply(c(names(REASONS), "total"),
                     function(v) rank(-la[[v]])[i], numeric(1)),
    rate_bh = vapply(c(names(REASONS), "total"),
                     function(v) la[[v]][i], numeric(1))
  ) |>
    mutate(reason = ifelse(code == "total", "All absence", unname(REASONS[code])),
           n_la = nrow(la)) |>
    arrange(rank_bh)
}


#' Illness absence by LA, with disadvantage, so the question "is this
#' just a poorer city?" can be answered rather than asserted.
illness_context <- function(abs_sch, year) {
  la <- abs_sch |>
    filter(time_period == year) |>
    group_by(la_name) |>
    summarise(schools = n(),
              poss    = sum(poss, na.rm = TRUE),
              total   = 100 * sum(overall, na.rm = TRUE) / sum(poss, na.rm = TRUE),
              illness = 100 * sum(sess_auth_illness, na.rm = TRUE) / sum(poss, na.rm = TRUE),
              unauth_other = 100 * sum(sess_unauth_other, na.rm = TRUE) / sum(poss, na.rm = TRUE),
              .groups = "drop") |>
    filter(poss > 1e6)

  panel <- readRDS(here::here("data", "panel_data.rds")) |>
    filter(MINORGROUP %in% c("Academy", "Maintained school")) |>
    filter(year_label == max(year_label)) |>
    group_by(LANAME) |>
    summarise(fsm = weighted.mean(PTFSM6CLA1A, TOTPUPS, na.rm = TRUE), .groups = "drop")

  j <- la |> left_join(panel, by = c("la_name" = "LANAME")) |> filter(!is.na(fsm))
  fit <- lm(illness ~ fsm, j)
  j |>
    mutate(expected = fitted(fit),
           resid    = resid(fit),
           rank_illness = rank(-illness),
           rank_resid   = rank(-resid),
           r2 = summary(fit)$r.squared,
           n_la = nrow(j))
}


#' The same split, year by year, so a one-year artefact cannot pass as
#' a standing feature of the city.
reason_series <- function(abs_sch) {
  pp <- function(num, den) 100 * sum(num, na.rm = TRUE) / sum(den, na.rm = TRUE)
  roll <- function(d) d |>
    summarise(total        = pp(overall, poss),
              illness      = pp(sess_auth_illness, poss),
              authorised   = pp(authorised, poss),
              unauthorised = pp(unauthorised, poss),
              auth_other   = pp(sess_auth_other, poss),
              unauth_other = pp(sess_unauth_other, poss),
              excluded     = pp(sess_auth_excluded, poss),
              .groups      = "drop")
  bh  <- abs_sch |> filter(grepl("Brighton and Hove", la_name)) |>
    group_by(time_period) |> roll()
  eng <- abs_sch |> group_by(time_period) |> roll()
  left_join(bh, eng, by = "time_period", suffix = c("_bh", "_eng"))
}


#' Local authority picture for the latest exclusions year
la_table <- function(j, year) {
  j |>
    filter(time_period == year) |>
    group_by(la_name) |>
    summarise(schools   = n(),
              headcount = sum(headcount, na.rm = TRUE),
              susp_rate = 100 * sum(suspension, na.rm = TRUE) /
                                sum(headcount, na.rm = TRUE),
              perm      = sum(perm_excl, na.rm = TRUE),
              abs_rate  = 100 * sum(overall, na.rm = TRUE) /
                                sum(poss, na.rm = TRUE),
              excl_pp   = 100 * sum(sess_auth_excluded, na.rm = TRUE) /
                                sum(poss, na.rm = TRUE),
              pa_rate   = 100 * sum(pa, na.rm = TRUE) /
                                sum(enrolments, na.rm = TRUE),
              .groups   = "drop") |>
    filter(headcount > 2000) |>
    mutate(rank_susp = rank(-susp_rate),
           rank_abs  = rank(-abs_rate),
           n_la      = n())
}


#' Brighton and Hove, year by year, against England on the same basis
bh_series <- function(j) {
  roll <- function(d) d |>
    summarise(schools   = n(),
              susp_rate = 100 * sum(suspension, na.rm = TRUE) /
                                sum(headcount, na.rm = TRUE),
              perm      = sum(perm_excl, na.rm = TRUE),
              abs_rate  = 100 * sum(overall, na.rm = TRUE) /
                                sum(poss, na.rm = TRUE),
              excl_pp   = 100 * sum(sess_auth_excluded, na.rm = TRUE) /
                                sum(poss, na.rm = TRUE),
              pa_rate   = 100 * sum(pa, na.rm = TRUE) /
                                sum(enrolments, na.rm = TRUE),
              .groups   = "drop")
  bh  <- j |> filter(grepl("Brighton and Hove", la_name)) |>
    group_by(time_period) |> roll()
  eng <- j |> group_by(time_period) |> roll()
  left_join(bh, eng, by = "time_period", suffix = c("_bh", "_eng"))
}


build_exclusions <- function() {
  message("Reading absence ...")
  abs_sch <- read_absence()
  message("  ", nrow(abs_sch), " secondary school-years, ",
          min(abs_sch$time_period), "-", max(abs_sch$time_period))

  message("Reading exclusions ...")
  exc <- read_exclusions()
  message("  ", nrow(exc), " secondary school-years, ",
          min(exc$time_period), "-", max(exc$time_period))

  latest_abs <- max(abs_sch$time_period)
  latest_exc <- max(exc$time_period)

  message("Joining ...")
  j <- build_join(abs_sch, exc)
  message("  ", nrow(j), " joined school-years")

  out <- list(
    national   = national_series(abs_sch),
    reasons    = reason_split(abs_sch, latest_abs),
    reason_gap     = reason_gap(abs_sch, latest_abs),
    reason_ranks   = reason_ranks(abs_sch, latest_abs),
    reason_series  = reason_series(abs_sch),
    illness_ctx    = illness_context(abs_sch, latest_abs),
    bh_by_school   = abs_sch |> filter(grepl("Brighton and Hove", la_name),
                                       time_period == latest_abs),
    joined     = j,
    abs_models = absence_models(j),
    att_models = attainment_models(j),
    la         = la_table(j, latest_exc),
    bh         = bh_series(j),
    bh_schools = j |> filter(grepl("Brighton and Hove", la_name)),
    latest_absence    = latest_abs,
    latest_exclusions = latest_exc,
    panel_years       = PANEL_YEARS,
    reason_labels     = REASONS,
    built_at          = Sys.time()
  )

  path <- here::here("data", "exclusions_absence.rds")
  saveRDS(out, path)
  message("Wrote ", path)
  invisible(out)
}


if (sys.nframe() == 0) build_exclusions()
