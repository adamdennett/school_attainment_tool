# predict_scenario.R - Prediction functions for the Shiny policy simulator
# -------------------------------------------------------------------------
# This module provides functions to:
#   1. Predict baseline attainment for a school (using model + random effects)
#   2. Predict attainment under a modified scenario (e.g. reduce absence by 5pp)
#   3. Decompose the total predicted change into per-variable contributions
#   4. Generate prediction intervals using merTools::predictInterval
#
# Replicates the prediction pattern from week8_practical.qmd lines 1386-1399
# -------------------------------------------------------------------------

source(here::here("R", "helpers.R"))


#' Predict attainment for a school under baseline and scenario conditions
#'
#' @param model A fitted lmerMod object
#' @param school_data A single-row dataframe with the school's current values
#' @param modifications Named list of changes to apply (on the original scale).
#'   e.g. list(PERCTOT = -5, PTFSM6CLA1A = -3) means reduce absence by 5pp
#'   and reduce FSM% by 3pp from the school's current values.
#' @param use_random_effects Logical. If TRUE (default), include the school's
#'   random effects (BLUP) in the prediction. If FALSE, predict using only
#'   fixed effects (population average).
#' @return A list with baseline, scenario, change, and percent change values
predict_scenario <- function(model, school_data, modifications = list(),
                              use_random_effects = TRUE) {

  stopifnot(nrow(school_data) == 1)

  # Set re.form based on whether to include random effects
  re_form <- if (use_random_effects) NULL else NA

  # Fix contrasts: OFSTEDRATING_1 is an ordered factor (polynomial contrasts)

  # but the model was fitted with treatment contrasts.
  if (is.factor(school_data$OFSTEDRATING_1)) {
    contrasts(school_data$OFSTEDRATING_1) <-
      contr.treatment(levels(school_data$OFSTEDRATING_1))
  }

  # 1. Baseline prediction (current values)
  baseline_newdata <- school_data

  # 2. Apply modifications to create scenario data
  scenario_newdata <- school_data
  for (var_name in names(modifications)) {
    if (var_name %in% names(scenario_newdata)) {
      current_val <- scenario_newdata[[var_name]]
      change <- modifications[[var_name]]
      new_val <- current_val + change
      # Clamp to valid ranges (percentages can't be negative)
      new_val <- max(new_val, 0.01)
      scenario_newdata[[var_name]] <- new_val
    } else {
      warning("Variable '", var_name, "' not found in school data - skipping")
    }
  }

  # 3. Predict on log scale
  baseline_log <- tryCatch(
    predict(model, newdata = baseline_newdata, re.form = re_form, allow.new.levels = TRUE),
    error = function(e) NA_real_
  )

  scenario_log <- tryCatch(
    predict(model, newdata = scenario_newdata, re.form = re_form, allow.new.levels = TRUE),
    error = function(e) NA_real_
  )

  # 4. Back-transform with bias correction
  # When log(Y) ~ Normal(mu, sigma^2), E[Y] = exp(mu + sigma^2/2)
  sigma2 <- sigma(model)^2
  bias_correction <- exp(0.5 * sigma2)

  baseline_att8 <- exp(baseline_log) * bias_correction
  scenario_att8 <- exp(scenario_log) * bias_correction

  # 5. Compute change
  change_att8 <- scenario_att8 - baseline_att8
  pct_change <- ifelse(baseline_att8 > 0, (change_att8 / baseline_att8) * 100, NA_real_)

  list(
    baseline = as.numeric(baseline_att8),
    scenario = as.numeric(scenario_att8),
    change = as.numeric(change_att8),
    pct_change = as.numeric(pct_change),
    baseline_log = as.numeric(baseline_log),
    scenario_log = as.numeric(scenario_log),
    modifications = modifications,
    bias_correction = bias_correction
  )
}


#' Decompose the total scenario effect into per-variable contributions
#'
#' For each modified variable, computes the marginal effect of changing
#' just that variable (holding all others at current values).
#'
#' @param model A fitted lmerMod object
#' @param school_data A single-row dataframe
#' @param modifications Named list of changes
#' @param use_random_effects Logical
#' @return A tibble with variable, current_value, scenario_value, marginal_effect
decompose_scenario <- function(model, school_data, modifications = list(),
                                use_random_effects = TRUE) {

  if (length(modifications) == 0) {
    return(tibble(
      variable = character(),
      current_value = numeric(),
      scenario_value = numeric(),
      marginal_effect = numeric(),
      pct_of_total = numeric()
    ))
  }

  re_form <- if (use_random_effects) NULL else NA
  sigma2 <- sigma(model)^2
  bias_correction <- exp(0.5 * sigma2)

  # Fix contrasts (same as in predict_scenario)
  if (is.factor(school_data$OFSTEDRATING_1)) {
    contrasts(school_data$OFSTEDRATING_1) <-
      contr.treatment(levels(school_data$OFSTEDRATING_1))
  }

  # Baseline prediction
  baseline_log <- predict(model, newdata = school_data, re.form = re_form,
                           allow.new.levels = TRUE)
  baseline_att8 <- exp(baseline_log) * bias_correction

  # Per-variable marginal effects
  results <- map_dfr(names(modifications), function(var_name) {
    if (!(var_name %in% names(school_data))) return(NULL)

    # Change just this one variable
    single_mod <- list()
    single_mod[[var_name]] <- modifications[[var_name]]
    pred <- predict_scenario(model, school_data, single_mod, use_random_effects)

    tibble(
      variable = var_name,
      display_name = variable_display_name(var_name),
      current_value = as.numeric(school_data[[var_name]]),
      scenario_value = as.numeric(school_data[[var_name]] + modifications[[var_name]]),
      marginal_effect = pred$change
    )
  })

  # Compute percentage of total change
  total_effect <- predict_scenario(model, school_data, modifications, use_random_effects)$change

  results <- results %>%
    mutate(
      pct_of_total = ifelse(
        abs(total_effect) > 0.001,
        round(marginal_effect / total_effect * 100, 1),
        0
      )
    )

  results
}


#' Run all three models for equity comparison
#'
#' @param models Named list of models (all, disadvantaged, non_disadvantaged)
#' @param school_data Single-row dataframe
#' @param modifications Named list of changes
#' @return A tibble comparing the effect across all three models
predict_equity_comparison <- function(models, school_data, modifications = list()) {

  outcome_labels <- c(
    all = "All Pupils",
    disadvantaged = "Disadvantaged",
    non_disadvantaged = "Non-Disadvantaged"
  )

  results <- map_dfr(names(models), function(model_name) {
    pred <- predict_scenario(models[[model_name]], school_data, modifications)
    tibble(
      model = model_name,
      label = outcome_labels[model_name],
      baseline = pred$baseline,
      scenario = pred$scenario,
      change = pred$change,
      pct_change = pred$pct_change
    )
  })

  # Add the disadvantage gap
  if ("disadvantaged" %in% results$model && "non_disadvantaged" %in% results$model) {
    disadv <- results %>% filter(model == "disadvantaged")
    nondis <- results %>% filter(model == "non_disadvantaged")

    gap_baseline <- nondis$baseline - disadv$baseline
    gap_scenario <- nondis$scenario - disadv$scenario

    results <- bind_rows(results, tibble(
      model = "gap",
      label = "Disadvantage Gap",
      baseline = gap_baseline,
      scenario = gap_scenario,
      change = gap_scenario - gap_baseline,
      pct_change = ifelse(gap_baseline > 0,
                          ((gap_scenario - gap_baseline) / gap_baseline) * 100,
                          NA_real_)
    ))
  }

  results
}


#' Human-readable display names for model variables
variable_display_name <- function(var_name) {
  display_map <- c(
    PERCTOT = "Overall Absence Rate (%)",
    PPERSABS10 = "Persistent Absence Rate (%)",
    PTFSM6CLA1A = "Disadvantaged Pupils (%)",
    PNUMEAL = "English as Additional Language (%)",
    PTPRIORLO = "Low Prior Attainment (%)",
    PTPRIORHI = "High Prior Attainment (%)",
    ADMPOL_PT = "Admissions Policy",
    gorard_segregation = "LA Segregation Index",
    remained_in_the_same_school = "Teachers Remaining at School",
    teachers_on_leadership_pay_range_percent = "Leadership Pay Range (%)",
    average_number_of_days_taken = "Teacher Sickness Days (avg)",
    year_numeric = "Year (trend)"
  )

  ifelse(var_name %in% names(display_map), display_map[var_name], var_name)
}


#' Get the slider configuration for adjustable variables
#'
#' Returns a list of slider parameters for each adjustable variable,
#' including display name, min/max adjustment range, step size, and unit.
#' Only variables present in the core model are included so that the
#' simulator works across all four years including 2024-25.
get_slider_config <- function() {
  list(
    PERCTOT = list(
      display_name = "Overall Absence Rate",
      unit = "%",
      min_change = -10, max_change = 10,
      step = 0.5
    ),
    PTFSM6CLA1A = list(
      display_name = "% Disadvantaged Pupils",
      unit = "%",
      min_change = -20, max_change = 20,
      step = 1
    ),
    PNUMEAL = list(
      display_name = "% English as Additional Language",
      unit = "%",
      min_change = -20, max_change = 20,
      step = 1
    ),
    gorard_segregation = list(
      display_name = "LA Segregation Index",
      unit = "",
      min_change = -0.15, max_change = 0.15,
      step = 0.01
    )
  )
}
