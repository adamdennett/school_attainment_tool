# 05b_value_added_2526.R - Extend the published value-added measure to 2025/26
# ---------------------------------------------------------------------------
# The value-added measure the Brighton work uses is the residual of the
# published model: observed Attainment 8 minus what the model expects from
# the school's intake. It is the `residual_ATT8SCR_imputed` column in
# panel_data.rds, built by safe_predict() in 05_fit_model.R, and it is what
# tbl-league-all in pulling_the_right_lever_v2.qmd reports and what
# open_scenarios.rds$choice$va averages.
#
# The right way to add a fifth year is therefore to PREDICT FROM THE
# PUBLISHED MODEL, not to refit one. Refitting - even on the same
# specification - moves all four published years. Predicting leaves them
# bit-for-bit identical and puts 2025/26 on the same yardstick.
#
# Two details of safe_predict() have to be reproduced exactly or the
# residuals come out wrong:
#
#   1. A lognormal bias correction. The model is fitted on log(ATT8SCR),
#      so the back-transformation is exp(log_pred) * exp(0.5 * sigma^2),
#      not exp(log_pred). Omitting it inflates every residual by about
#      0.2 Attainment 8 points - small, uniform, and entirely systematic.
#
#   2. Treatment contrasts on OFSTEDRATING_1. The model was fitted with
#      them; an ordered factor otherwise defaults to polynomial contrasts
#      and predict() silently returns different numbers.
#
# Three predictors do not exist for 2025/26 and are carried forward from
# 2024-25 exactly as impute_missing_predictors() does for 2024-25 itself
# (ks2_c and the three workforce variables). Full-year absence does not
# exist either, and is bridged from the autumn+spring series - see
# bridge_absence().
#
# Usage:
#   source("R/05b_value_added_2526.R")
#   va <- value_added_5yr()      # URN x year, with the published years intact
#
# Run from project root.
# ---------------------------------------------------------------------------

source(here::here("R", "helpers.R"))

VA_MODEL_FILE <- "models_imputed.rds"

# Carried forward into 2025/26, in the order impute_missing_predictors() uses
VA_CARRY_VARS <- c("ks2_c", "remained_in_the_same_school",
                   "teachers_on_leadership_pay_range_percent",
                   "average_number_of_days_taken")

# The three pupil groups, and the outcome each model is fitted on. These are
# the same three tbl-league-all / tbl-league-dis in the published report rank
# schools on, so all three have to be carried into the new year together or
# the sets are not comparable.
VA_GROUPS <- c(all               = "ATT8SCR",
               disadvantaged     = "ATT8SCR_FSM6CLA1A",
               non_disadvantaged = "ATT8SCR_NFSM6CLA1A")

# ---------------------------------------------------------------------------
# BACK-TRANSFORMATION
#
# The models are fitted on log(ATT8SCR), so exp(y_hat) is the conditional
# MEDIAN. The expected score given a school's intake is
# exp(y_hat) * exp(sigma^2 / 2).
#
#   "corrected"    Observed - exp(y_hat) * exp(sigma^2 / 2)   <- use this
#   "uncorrected"  Observed - exp(y_hat)
#
# These used to disagree across the project. safe_predict() in
# R/05_fit_model.R applied the correction, writing residual_ATT8SCR_imputed
# into panel_data.rds and from there into the simulator's value-added
# scenario; the league tables and the observed-vs-predicted figure in
# pulling_the_right_lever_v2.qmd did not, and so printed residuals about 0.2
# Attainment 8 points higher for the same school and year. That report was
# moved onto the corrected form in October 2026 and both now agree.
#
# "uncorrected" is kept only to reproduce the older published tables. The
# shift is multiplicative on the predicted value, so it is not constant -
# 0.11 to 0.43 points across the national range - and it can reorder schools
# that sit close together, though it barely does (Spearman 0.9999).
# ---------------------------------------------------------------------------
VA_CONVENTIONS <- c("corrected", "uncorrected")


#' Put autumn+spring absence onto the full-year scale
#'
#' The model needs log(PERCTOT), a full academic year. 2025/26 publishes only
#' autumn and spring. Both exist for every earlier year, so the relationship
#' is estimated rather than assumed, on the log scale the model uses.
#'
#' @param panel A panel carrying PERCTOT and PERCTOT_2term
#' @return list(model, r2, sigma)
bridge_absence <- function(panel) {
  d <- panel |>
    dplyr::filter(!is.na(.data$PERCTOT), .data$PERCTOT > 0,
                  !is.na(.data$PERCTOT_2term), .data$PERCTOT_2term > 0)
  m <- stats::lm(log(PERCTOT) ~ log(PERCTOT_2term), data = d)
  message(sprintf("  absence bridge: n=%s  R2=%.4f  residual SD %.4f (log scale)",
                  format(nrow(d), big.mark = ","), summary(m)$r.squared, sigma(m)))
  list(model = m, r2 = summary(m)$r.squared, sigma = sigma(m), n = nrow(d))
}


#' Carry forward the predictors 2025/26 does not publish
#'
#' Same method as impute_missing_predictors(): previous year first, then the
#' school's mean across available years. Only fills the new year.
carry_forward_predictors <- function(panel, new_year = "2025-2026",
                                     prev_year = "2024-2025") {
  hist <- panel |> dplyr::filter(.data$academic_year != new_year)
  prev <- hist |> dplyr::filter(.data$academic_year == prev_year)
  is_new <- panel$academic_year == new_year

  if (!"has_imputed_predictors" %in% names(panel)) {
    panel$has_imputed_predictors <- FALSE
  }
  panel$has_imputed_predictors[is_new] <- FALSE

  for (v in intersect(VA_CARRY_VARS, names(panel))) {
    cf <- prev |> dplyr::filter(!is.na(.data[[v]])) |>
      dplyr::select("URN", val = dplyr::all_of(v))
    sm <- hist |> dplyr::filter(!is.na(.data[[v]])) |>
      dplyr::group_by(.data$URN) |>
      dplyr::summarise(val = mean(.data[[v]], na.rm = TRUE), .groups = "drop")

    idx <- which(is_new & is.na(panel[[v]]))
    filled <- dplyr::coalesce(cf$val[match(panel$URN[idx], cf$URN)],
                              sm$val[match(panel$URN[idx], sm$URN)])
    panel[[v]][idx] <- filled
    panel$has_imputed_predictors[idx[!is.na(filled)]] <- TRUE
    message(sprintf("  %-42s carried forward for %s of %s schools",
                    v, format(sum(!is.na(filled)), big.mark = ","),
                    format(length(idx), big.mark = ",")))
  }
  panel
}


#' Predict from a fitted model exactly as safe_predict() does
#'
#' Reproduces 05_fit_model.R: complete cases on the model's variables,
#' positive values where the formula takes a log, treatment contrasts on
#' OFSTEDRATING_1, and the lognormal bias correction on the way back.
#'
#' @param convention "corrected" (the expected score) or "uncorrected"
#'   (the conditional median). See VA_CONVENTIONS.
#' @return A full-length vector of predictions on the Attainment 8 scale
predict_like_published <- function(model, data, required_vars,
                                   convention = "corrected") {

  convention <- match.arg(convention, VA_CONVENTIONS)

  keep <- stats::complete.cases(data[, required_vars, drop = FALSE])
  for (v in required_vars) {
    if (is.numeric(data[[v]])) keep <- keep & (data[[v]] > 0 | is.na(data[[v]]))
  }
  if (!any(keep)) return(rep(NA_real_, nrow(data)))

  sub <- droplevels(data[keep, , drop = FALSE])
  if (is.factor(sub$OFSTEDRATING_1)) {
    stats::contrasts(sub$OFSTEDRATING_1) <-
      stats::contr.treatment(levels(sub$OFSTEDRATING_1))
  }

  log_pred <- stats::predict(model, newdata = sub, re.form = NULL,
                             allow.new.levels = TRUE)
  bias <- if (convention == "corrected") exp(0.5 * sigma(model)^2) else 1

  out <- rep(NA_real_, nrow(data))
  out[keep] <- exp(log_pred) * bias
  out
}


#' Value added for all five years, on the published model
#' Value added for all five years, all three pupil groups
#'
#' Adds, for each group g in VA_GROUPS, a predicted_<outcome>_va column and a
#' va_<outcome> column. All three are produced together so the all-pupil,
#' disadvantaged and non-disadvantaged sets are always comparable.
#'
#' @param panel The five-year panel (default: data/panel_data_5yr.rds)
#' @param convention "corrected" (the expected score, matching the report
#'   and residual_ATT8SCR_imputed) or "uncorrected". See VA_CONVENTIONS.
#' @param verify Check the four published years against what is on record
#' @return The panel with the value-added columns added
value_added_5yr <- function(panel = NULL, convention = "corrected",
                            verify = TRUE) {

  convention <- match.arg(convention, VA_CONVENTIONS)
  if (is.null(panel)) panel <- readRDS(here::here("data", "panel_data_5yr.rds"))
  mods <- readRDS(here::here("data", VA_MODEL_FILE))

  message("Bridging absence for the new year ...")
  br <- bridge_absence(panel)
  panel <- panel |>
    dplyr::mutate(PERCTOT = dplyr::if_else(
      .data$academic_year == "2025-2026" &
        !is.na(.data$PERCTOT_2term) & .data$PERCTOT_2term > 0,
      exp(stats::predict(br$model, newdata = dplyr::pick(dplyr::everything()))),
      .data$PERCTOT))

  message("Carrying forward the predictors 2025/26 does not publish ...")
  panel <- carry_forward_predictors(panel)

  # safe_predict() requires complete cases on EVERY model variable, including
  # all three outcomes, so a school missing one group's score is dropped from
  # all three. Keeping that rule is what makes the sets comparable.
  req <- c(unname(VA_GROUPS), "PTFSM6CLA1A", "PERCTOT", "PNUMEAL", "ks2_c",
           "ADMPOL_PT", "gorard_segregation", "remained_in_the_same_school",
           "teachers_on_leadership_pay_range_percent",
           "average_number_of_days_taken", "OFSTEDRATING_1", "gor_name",
           "LANAME", "year_label")

  message("Predicting from the published models (", convention, " convention) ...")
  for (g in names(VA_GROUPS)) {
    out <- VA_GROUPS[[g]]
    p <- predict_like_published(mods[[g]], panel, req, convention = convention)
    panel[[paste0("predicted_", out, "_va")]] <- p
    panel[[paste0("va_", out)]] <- panel[[out]] - p
    message(sprintf("  %-18s %s schools with a figure", g,
                    format(sum(!is.na(panel[[paste0("va_", out)]])), big.mark = ",")))
  }

  message("")
  print(as.data.frame(panel |>
    dplyr::group_by(.data$academic_year) |>
    dplyr::summarise(
      schools    = dplyr::n(),
      all        = sum(!is.na(.data$va_ATT8SCR)),
      disadv     = sum(!is.na(.data$va_ATT8SCR_FSM6CLA1A)),
      non_disadv = sum(!is.na(.data$va_ATT8SCR_NFSM6CLA1A)),
      .groups = "drop")))

  if (verify && convention == "corrected" &&
      "residual_ATT8SCR_imputed" %in% names(panel)) {
    v <- panel |> dplyr::filter(!is.na(.data$residual_ATT8SCR_imputed),
                                !is.na(.data$va_ATT8SCR))
    worst <- max(abs(v$va_ATT8SCR - v$residual_ATT8SCR_imputed))
    message(sprintf("\nAgainst residual_ATT8SCR_imputed: %s rows, max difference %.3g",
                    format(nrow(v), big.mark = ","), worst))
    if (worst > 1e-6) {
      warning("The published years do not reproduce (max ", signif(worst, 3), ").")
    } else {
      message("  Exact.")
    }
  }

  invisible(panel)
}


#' A value-added league table, in the shape the published report prints
#'
#' @param va Output of value_added_5yr()
#' @param group One of names(VA_GROUPS)
#' @param la Local authority to restrict to
#' @return One row per school, one column per year, plus the mean
va_league_table <- function(va, group = "all", la = "Brighton and Hove") {

  group <- match.arg(group, names(VA_GROUPS))
  col   <- paste0("va_", VA_GROUPS[[group]])

  va |>
    dplyr::filter(.data$LANAME == la, !is.na(.data[[col]])) |>
    dplyr::select("SCHNAME", "year_label", dplyr::all_of(col)) |>
    dplyr::rename(value = dplyr::all_of(col)) |>
    dplyr::group_by(.data$SCHNAME) |>
    dplyr::mutate(Mean = mean(.data$value, na.rm = TRUE)) |>
    dplyr::ungroup() |>
    tidyr::pivot_wider(names_from = "year_label", values_from = "value") |>
    dplyr::arrange(dplyr::desc(.data$Mean)) |>
    dplyr::relocate("Mean", .after = dplyr::last_col())
}


if (sys.nframe() == 0) value_added_5yr()
