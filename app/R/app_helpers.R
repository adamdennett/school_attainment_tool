# app_helpers.R - Self-contained helper functions for the Shiny app
# -----------------------------------------------------------------
# This file bundles all functions the app needs so it can be deployed
# standalone (e.g. shinyapps.io) without sourcing from the project R/ dir.
#
# Includes:
#   - variable_display_name()   (from predict_scenario.R)
#   - get_slider_config()       (from predict_scenario.R)
#   - predict_slim()            (from predict_slim.R)
#   - predict_scenario_slim()   (from predict_slim.R)
#   - decompose_scenario_slim() (from predict_slim.R)
#   - predict_equity_comparison_slim() (from predict_slim.R)
# -----------------------------------------------------------------


# ===================================================================
# Variable display names and slider config
# ===================================================================

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
    OFSTEDRATING_1 = "Ofsted Rating",
    remained_in_the_same_school = "Teachers Remaining at School",
    teachers_on_leadership_pay_range_percent = "Leadership Pay Range (%)",
    average_number_of_days_taken = "Teacher Sickness Days (avg)",
    year_numeric = "Year (trend)"
  )

  ifelse(var_name %in% names(display_map), display_map[var_name], var_name)
}


# PTPRIORLO coefficients from the full panel model (Analysis A)
# These are used as approximate adjustments since PTPRIORLO is not
# in the core model (100% NA in 2024-25). The coefficient represents
# the effect on log(ATT8) per 1 percentage-point change in PTPRIORLO.
PTPRIORLO_BETA <- c(
  all              = -0.006271547,
  disadvantaged    = -0.006049313,
  non_disadvantaged = -0.005707144
)


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
    PTPRIORLO = list(
      display_name = "% Low Prior Attainment (KS2)",
      unit = "%",
      min_change = -30, max_change = 30,
      step = 1
    )
  )
}


# ===================================================================
# Lightweight prediction engine (replaces lme4::predict)
# ===================================================================
#
# The slim model structure (from 06_prepare_app_data.R) contains:
#   $beta              - named vector of fixed-effect coefficients
#   $ranef             - list of random-effect BLUPs per grouping factor
#   $sigma             - residual standard deviation (for bias correction)
#   $ofsted_levels     - factor levels for OFSTEDRATING_1
#   $gor_levels        - factor levels for gor_name
#   $la_levels         - factor levels for LANAME
#   $year_levels       - factor levels for year_label
#   $ofsted_contrasts  - contrast matrix used in fitting
#   $formula_rhs       - the RHS of the model formula (for reference)
#
# The core model formula is:
#   log(Y) ~ log(PTFSM6CLA1A) + log(PERCTOT) + log(PNUMEAL) +
#            ADMPOL_PT + gorard_segregation +
#            (1|year_label) + (1|OFSTEDRATING_1) + (1|gor_name/LANAME)


#' Predict from a slim model object
#'
#' Reconstructs predictions manually from fixed-effect coefficients
#' and random-effect BLUPs, without needing the full lme4 model.
#'
#' @param slim_model A list with components: beta, ranef, sigma, etc.
#' @param newdata A data frame (one or more rows) with the predictor columns.
#' @param include_re Logical. If TRUE (default), include random effects.
#'   If FALSE, return fixed-effects-only (population average) prediction.
#' @param ofsted_override Optional character string to override the Ofsted
#'   rating used for the random intercept (e.g. "Outstanding", "Good").
#' @return Numeric vector of predicted values on the ORIGINAL scale
#'   (i.e. back-transformed from log scale with Duan-type bias correction).
predict_slim <- function(slim_model, newdata, include_re = TRUE,
                         ofsted_override = NULL) {

  beta <- slim_model$beta
  n <- nrow(newdata)

  # ---- 1. Build the fixed-effects design vector ----
  # Intercept
  log_pred <- rep(beta[["(Intercept)"]], n)

  # Continuous log-transformed predictors
  log_vars <- c("PTFSM6CLA1A" = "log(PTFSM6CLA1A)",
                "PERCTOT"      = "log(PERCTOT)",
                "PNUMEAL"      = "log(PNUMEAL)")

  for (raw_name in names(log_vars)) {
    beta_name <- log_vars[[raw_name]]
    if (beta_name %in% names(beta)) {
      vals <- as.numeric(newdata[[raw_name]])
      # Guard against log(0) or log(negative)
      vals <- ifelse(vals > 0, log(vals), NA_real_)
      log_pred <- log_pred + beta[[beta_name]] * vals
    }
  }

  # Linear continuous predictors
  linear_vars <- c("gorard_segregation")
  for (v in linear_vars) {
    if (v %in% names(beta) && v %in% names(newdata)) {
      log_pred <- log_pred + beta[[v]] * as.numeric(newdata[[v]])
    }
  }

  # Categorical: ADMPOL_PT (treatment-coded dummies)
  # Reference level is the first level (NON SEL for ADMPOL_PT)
  # The beta names are like "ADMPOL_PTOTHER NON SEL" and "ADMPOL_PTSEL"
  admpol_dummies <- grep("^ADMPOL_PT", names(beta), value = TRUE)
  if (length(admpol_dummies) > 0 && "ADMPOL_PT" %in% names(newdata)) {
    admpol_vals <- as.character(newdata$ADMPOL_PT)
    for (dummy_name in admpol_dummies) {
      # Extract the level name: "ADMPOL_PTOTHER NON SEL" -> "OTHER NON SEL"
      level_name <- sub("^ADMPOL_PT", "", dummy_name)
      indicator <- as.numeric(admpol_vals == level_name)
      log_pred <- log_pred + beta[[dummy_name]] * indicator
    }
  }

  # ---- 2. Add random effects ----
  if (include_re) {
    ranef <- slim_model$ranef

    # year_label random intercept
    if ("year_label" %in% names(ranef) && "year_label" %in% names(newdata)) {
      yr_blups <- ranef$year_label[["(Intercept)"]]
      names(yr_blups) <- rownames(ranef$year_label)
      yr_vals <- as.character(newdata$year_label)
      yr_re <- yr_blups[yr_vals]
      yr_re[is.na(yr_re)] <- 0  # new levels get 0
      log_pred <- log_pred + yr_re
    }

    # OFSTEDRATING_1 random intercept (with optional override)
    if ("OFSTEDRATING_1" %in% names(ranef)) {
      of_blups <- ranef$OFSTEDRATING_1[["(Intercept)"]]
      names(of_blups) <- rownames(ranef$OFSTEDRATING_1)
      if (!is.null(ofsted_override)) {
        of_vals <- rep(ofsted_override, n)
      } else if ("OFSTEDRATING_1" %in% names(newdata)) {
        of_vals <- as.character(newdata$OFSTEDRATING_1)
      } else {
        of_vals <- rep(NA_character_, n)
      }
      of_re <- of_blups[of_vals]
      of_re[is.na(of_re)] <- 0
      log_pred <- log_pred + of_re
    }

    # gor_name random intercept
    if ("gor_name" %in% names(ranef) && "gor_name" %in% names(newdata)) {
      gor_blups <- ranef$gor_name[["(Intercept)"]]
      names(gor_blups) <- rownames(ranef$gor_name)
      gor_vals <- as.character(newdata$gor_name)
      gor_re <- gor_blups[gor_vals]
      gor_re[is.na(gor_re)] <- 0
      log_pred <- log_pred + gor_re
    }

    # LANAME:gor_name nested random intercept
    la_gor_key <- "LANAME:gor_name"
    if (la_gor_key %in% names(ranef) &&
        "LANAME" %in% names(newdata) && "gor_name" %in% names(newdata)) {
      la_blups <- ranef[[la_gor_key]][["(Intercept)"]]
      names(la_blups) <- rownames(ranef[[la_gor_key]])
      # The key format is "LANAME:gor_name"
      la_gor_vals <- paste0(as.character(newdata$LANAME), ":",
                            as.character(newdata$gor_name))
      la_re <- la_blups[la_gor_vals]
      la_re[is.na(la_re)] <- 0
      log_pred <- log_pred + la_re
    }
  }

  # ---- 3. Back-transform from log scale with bias correction ----
  sigma <- slim_model$sigma
  bias_correction <- exp(0.5 * sigma^2)

  pred <- exp(log_pred) * bias_correction

  as.numeric(pred)
}


#' Predict a policy scenario using slim model objects
#'
#' @param slim_model A slim model list (from slim_core_models.rds)
#' @param school_data A single-row dataframe with the school's current values
#' @param modifications Named list of changes to apply (on the original scale).
#'   e.g. list(PERCTOT = -5, PTFSM6CLA1A = -3)
#' @param include_re Logical. If TRUE (default), include random effects.
#' @param ofsted_override Optional character: Ofsted rating to use for scenario.
#' @param model_name Character name of the model (e.g. "all") for PTPRIORLO lookup.
#' @return A list with baseline, scenario, change, and percent change values
predict_scenario_slim <- function(slim_model, school_data, modifications = list(),
                                   include_re = TRUE, ofsted_override = NULL,
                                   model_name = NULL) {

  stopifnot(nrow(school_data) == 1)

  # 1. Baseline prediction (current values)
  baseline <- predict_slim(slim_model, school_data, include_re = include_re)

  # Separate PTPRIORLO from core model modifications
  ptpriorlo_change <- modifications[["PTPRIORLO"]]
  core_modifications <- modifications[names(modifications) != "PTPRIORLO"]

  # 2. Apply core modifications to create scenario data
  scenario_data <- school_data
  for (var_name in names(core_modifications)) {
    if (var_name %in% names(scenario_data)) {
      current_val <- scenario_data[[var_name]]
      new_val <- current_val + core_modifications[[var_name]]
      new_val <- max(new_val, 0.01)
      scenario_data[[var_name]] <- new_val
    } else {
      warning("Variable '", var_name, "' not found in school data - skipping")
    }
  }

  # 3. Scenario prediction (with optional Ofsted override)
  scenario <- predict_slim(slim_model, scenario_data, include_re = include_re,
                            ofsted_override = ofsted_override)

  # 4. Apply PTPRIORLO adjustment (approximate, from full model coefficient)
  if (!is.null(ptpriorlo_change) && abs(ptpriorlo_change) > 0.001 &&
      !is.null(model_name) && model_name %in% names(PTPRIORLO_BETA)) {
    # PTPRIORLO enters the full model linearly on the log scale:
    # delta_log_Y = beta_ptpriorlo * delta_PTPRIORLO
    # So the multiplicative effect on Y is: exp(beta * delta)
    beta_ptpriorlo <- PTPRIORLO_BETA[[model_name]]
    scenario <- scenario * exp(beta_ptpriorlo * ptpriorlo_change)
  }

  # 5. Compute change
  change <- scenario - baseline
  pct_change <- ifelse(baseline > 0, (change / baseline) * 100, NA_real_)

  list(
    baseline = as.numeric(baseline),
    scenario = as.numeric(scenario),
    change = as.numeric(change),
    pct_change = as.numeric(pct_change),
    modifications = modifications
  )
}


#' Decompose the total scenario effect into per-variable contributions
#'
#' @param slim_model A slim model list
#' @param school_data A single-row dataframe
#' @param modifications Named list of changes
#' @param include_re Logical
#' @param ofsted_override Optional Ofsted rating override
#' @param model_name Character model name for PTPRIORLO lookup
#' @return A tibble with variable, current_value, scenario_value, marginal_effect
decompose_scenario_slim <- function(slim_model, school_data, modifications = list(),
                                     include_re = TRUE, ofsted_override = NULL,
                                     model_name = NULL) {

  # Check if there's an Ofsted change (not represented as a modification)
  has_ofsted_change <- !is.null(ofsted_override) &&
    as.character(school_data$OFSTEDRATING_1) != ofsted_override

  if (length(modifications) == 0 && !has_ofsted_change) {
    return(tibble(
      variable = character(),
      display_name = character(),
      current_value = numeric(),
      scenario_value = numeric(),
      marginal_effect = numeric(),
      pct_of_total = numeric()
    ))
  }

  results <- tibble(
    variable = character(),
    display_name = character(),
    current_value = numeric(),
    scenario_value = numeric(),
    marginal_effect = numeric()
  )

  # Per-variable marginal effects for slider modifications
  if (length(modifications) > 0) {
    slider_results <- purrr::map_dfr(names(modifications), function(var_name) {
      if (!(var_name %in% names(school_data))) return(NULL)

      single_mod <- list()
      single_mod[[var_name]] <- modifications[[var_name]]
      pred <- predict_scenario_slim(slim_model, school_data, single_mod,
                                     include_re, model_name = model_name)

      tibble(
        variable = var_name,
        display_name = variable_display_name(var_name),
        current_value = as.numeric(school_data[[var_name]]),
        scenario_value = as.numeric(school_data[[var_name]] + modifications[[var_name]]),
        marginal_effect = pred$change
      )
    })
    results <- bind_rows(results, slider_results)
  }

  # Ofsted rating change as a separate row
  if (has_ofsted_change) {
    ofsted_pred <- predict_scenario_slim(slim_model, school_data, list(),
                                          include_re, ofsted_override = ofsted_override)
    results <- bind_rows(results, tibble(
      variable = "OFSTEDRATING_1",
      display_name = "Ofsted Rating",
      current_value = NA_real_,
      scenario_value = NA_real_,
      marginal_effect = ofsted_pred$change
    ))
  }

  # Compute percentage of total change
  total <- predict_scenario_slim(slim_model, school_data, modifications,
                                  include_re, ofsted_override = ofsted_override,
                                  model_name = model_name)

  results <- results %>%
    mutate(
      pct_of_total = ifelse(
        abs(total$change) > 0.001,
        round(marginal_effect / total$change * 100, 1),
        0
      )
    )

  results
}


#' Run all three slim models for equity comparison
#'
#' @param slim_models Named list of slim model objects (all, disadvantaged, non_disadvantaged)
#' @param school_data Single-row dataframe
#' @param modifications Named list of changes
#' @param ofsted_override Optional Ofsted rating override
#' @return A tibble comparing the effect across all three models
predict_equity_comparison_slim <- function(slim_models, school_data,
                                            modifications = list(),
                                            ofsted_override = NULL) {

  outcome_labels <- c(
    all = "All Pupils",
    disadvantaged = "Disadvantaged",
    non_disadvantaged = "Non-Disadvantaged"
  )

  results <- purrr::map_dfr(names(slim_models), function(model_name) {
    pred <- predict_scenario_slim(slim_models[[model_name]], school_data,
                                   modifications, ofsted_override = ofsted_override,
                                   model_name = model_name)
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
