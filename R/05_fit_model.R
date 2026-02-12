# 05_fit_model.R - Fit multilevel models (x3 outcomes) and run diagnostics
# -------------------------------------------------------------------------
# This script runs TWO analyses:
#
#   A. PANEL MODELS (all 4 years pooled)
#      - year_label enters as an additional RANDOM effect
#      - Captures between-year variation as a variance component
#      - Formula:
#        log(outcome) ~ log(PTFSM6CLA1A) + log(PERCTOT) + log(PNUMEAL) +
#          PTPRIORLO + ADMPOL_PT + gorard_segregation +
#          log(remained_in_the_same_school) +
#          teachers_on_leadership_pay_range_percent +
#          log(average_number_of_days_taken) +
#          (1 | year_label) + (1 | OFSTEDRATING) + (1 | gor_name/LANAME)
#
#   B. PER-YEAR MODELS (separate model for each academic year)
#      - Same fixed effects but no year term
#      - Allows coefficients to vary freely across years
#      - Formula:
#        log(outcome) ~ log(PTFSM6CLA1A) + log(PERCTOT) + log(PNUMEAL) +
#          PTPRIORLO + ADMPOL_PT + gorard_segregation +
#          log(remained_in_the_same_school) +
#          teachers_on_leadership_pay_range_percent +
#          log(average_number_of_days_taken) +
#          (1 | OFSTEDRATING) + (1 | gor_name/LANAME)
#
# Depends on: R/04_compute_derived.R (produces panel_data.rds)
# Run from project root: source("R/05_fit_model.R")
# -------------------------------------------------------------------------

source(here::here("R", "helpers.R"))
library(lme4)
library(lmerTest)  # For Satterthwaite p-values
library(performance)


# ---- Model configuration ----

# Outcome variables for the three models
OUTCOMES <- list(
  all = "ATT8SCR",
  disadvantaged = "ATT8SCR_FSM6CLA1A",
  non_disadvantaged = "ATT8SCR_NFSM6CLA1A"
)

# Common fixed-effect predictors (log transforms applied in the formula)
FIXED_PREDICTORS <- c(
  "PTFSM6CLA1A", "PERCTOT", "PNUMEAL",
  "PTPRIORLO", "ADMPOL_PT", "gorard_segregation",
  "remained_in_the_same_school",
  "teachers_on_leadership_pay_range_percent",
  "average_number_of_days_taken"
)

# Grouping variables for random effects (shared across both analyses)
MODEL_GROUPING <- c("OFSTEDRATING", "gor_name", "LANAME")

# Fixed-effects portion of the formula (shared by panel and per-year models)
FIXED_FORMULA_RHS <- paste0(
  "log(PTFSM6CLA1A) + log(PERCTOT) + log(PNUMEAL) + ",
  "PTPRIORLO + ADMPOL_PT + gorard_segregation + ",
  "log(remained_in_the_same_school) + ",
  "teachers_on_leadership_pay_range_percent + ",
  "log(average_number_of_days_taken)"
)

# Panel model: year as random effect
PANEL_RANDOM <- "(1 | year_label) + (1 | OFSTEDRATING) + (1 | gor_name/LANAME)"

# Per-year model: no year term
PERYEAR_RANDOM <- "(1 | OFSTEDRATING) + (1 | gor_name/LANAME)"


#' Prepare the modelling dataset
#'
#' Filters to complete cases for all model variables and ensures
#' positive values for log-transformed variables.
prepare_model_data <- function(panel) {

  message("Preparing modelling dataset ...")

  # All variables needed
  all_vars <- c(
    unlist(OUTCOMES),
    FIXED_PREDICTORS,
    MODEL_GROUPING,
    "URN", "SCHNAME", "academic_year", "year_label", "year_numeric"
  )

  # Check which variables are present
  missing_vars <- setdiff(all_vars, names(panel))
  if (length(missing_vars) > 0) {
    warning("Missing variables in panel: ", paste(missing_vars, collapse = ", "))
  }

  # Keep only mainstream school types (academies and maintained schools)
  school_types_keep <- c("Academy", "Maintained school")
  n_before <- nrow(panel)
  panel <- panel %>%
    filter(MINORGROUP %in% school_types_keep)
  message("  School type filter (", paste(school_types_keep, collapse = ", "), "): ",
          n_before, " -> ", nrow(panel), " rows (dropped ",
          n_before - nrow(panel), " non-mainstream schools)")

  # Filter to rows with valid data for the core model
  model_data <- panel %>%
    filter(
      # Outcomes: at least the main outcome must be valid
      !is.na(ATT8SCR), ATT8SCR > 0,
      # Log-transformed predictors must be positive
      PTFSM6CLA1A > 0,
      PERCTOT > 0,
      PNUMEAL > 0,
      # Random effect groupings must be present
      !is.na(OFSTEDRATING),
      !is.na(gor_name),
      !is.na(LANAME)
    )

  message("  After filtering for core model variables: ", nrow(model_data), " rows")

  # Further filter for workforce variables (may reduce sample size)
  model_data_full <- model_data %>%
    filter(
      remained_in_the_same_school > 0,
      !is.na(teachers_on_leadership_pay_range_percent),
      average_number_of_days_taken > 0,
      !is.na(gorard_segregation)
    )

  message("  After filtering for workforce + Gorard: ", nrow(model_data_full), " rows")

  # Ensure factor levels are set
  model_data_full <- model_data_full %>%
    mutate(
      OFSTEDRATING = factor(OFSTEDRATING),
      gor_name = factor(gor_name),
      LANAME = factor(LANAME),
      year_label = factor(year_label)
    ) %>%
    # Drop unused factor levels
    droplevels()

  message("  Ofsted levels: ", paste(levels(model_data_full$OFSTEDRATING), collapse = ", "))
  message("  Regions: ", n_distinct(model_data_full$gor_name))
  message("  LAs: ", n_distinct(model_data_full$LANAME))
  message("  Years: ", paste(sort(unique(as.character(model_data_full$year_label))), collapse = ", "))

  model_data_full
}


#' Fit a single multilevel model
#'
#' @param outcome_var Character name of the outcome variable
#' @param data Prepared modelling dataframe
#' @param random_effects Character string for the random effects portion of the formula
#' @param label Human-readable label for messages
#' @return A fitted lmerMod object
fit_attainment_model <- function(outcome_var, data, random_effects, label = outcome_var) {

  message("\nFitting model for: ", label, " (", outcome_var, ")")

  # Filter to rows where this specific outcome is valid
  model_data <- data %>%
    filter(!is.na(!!sym(outcome_var)), !!sym(outcome_var) > 0) %>%
    droplevels()

  message("  Observations: ", nrow(model_data))
  message("  OFSTEDRATING levels after filter: ",
          paste(levels(model_data$OFSTEDRATING), collapse = ", "))

  # Build formula
  formula_str <- paste0(
    "log(", outcome_var, ") ~ ",
    FIXED_FORMULA_RHS, " + ",
    random_effects
  )

  formula <- as.formula(formula_str)
  message("  Formula: ", formula_str)

  # Also show human-readable version
  fixed_readable <- paste0(
    "log(", var_label(outcome_var), ") ~ ",
    "log(", var_label("PTFSM6CLA1A"), ") + log(", var_label("PERCTOT"), ") + log(", var_label("PNUMEAL"), ") + ",
    var_label("PTPRIORLO"), " + ", var_label("ADMPOL_PT"), " + ", var_label("gorard_segregation"), " + ",
    "log(", var_label("remained_in_the_same_school"), ") + ",
    var_label("teachers_on_leadership_pay_range_percent"), " + ",
    "log(", var_label("average_number_of_days_taken"), ")"
  )
  message("  Readable (fixed): ", fixed_readable)

  # Set contrasts (need at least 2 levels)
  n_ofsted_levels <- nlevels(model_data$OFSTEDRATING)
  if (n_ofsted_levels < 2) {
    stop("OFSTEDRATING has only ", n_ofsted_levels, " level(s) after filtering for ",
         outcome_var, ". Cannot fit random intercept. Check data completeness.")
  }
  contrasts(model_data$OFSTEDRATING) <- contr.treatment(levels(model_data$OFSTEDRATING))

  # Fit model
  model <- lmer(
    formula,
    data = model_data,
    na.action = na.exclude,
    REML = TRUE,
    control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 20000))
  )

  # Check for convergence issues
  if (length(model@optinfo$conv$lme4$messages) > 0) {
    warning("  Convergence messages: ",
            paste(model@optinfo$conv$lme4$messages, collapse = "; "))
  }

  # Check for singular fit
  if (isSingular(model)) {
    warning("  Model has a SINGULAR fit - some variance components are at boundary")
  }

  # Print summary
  message("  Model fitted successfully")
  cat("\n")
  print(summary(model))

  model
}


#' Run model diagnostics and return a summary list
run_diagnostics <- function(model, label) {

  message("\nDiagnostics for: ", label)

  diag <- list()

  # R-squared (marginal and conditional)
  r2_vals <- tryCatch(
    performance::r2(model),
    error = function(e) list(R2_marginal = NA, R2_conditional = NA)
  )
  diag$r2_marginal <- as.numeric(r2_vals$R2_marginal)
  diag$r2_conditional <- as.numeric(r2_vals$R2_conditional)
  message("  R2 marginal (fixed only): ", round(diag$r2_marginal, 4))
  message("  R2 conditional (fixed + random): ", round(diag$r2_conditional, 4))

  # ICC by grouping variable
  icc_vals <- tryCatch(
    performance::icc(model, by_group = TRUE),
    error = function(e) NULL
  )
  diag$icc <- icc_vals
  if (!is.null(icc_vals)) {
    message("  ICC:")
    print(icc_vals)
  }

  # Fixed effects summary with human-readable labels
  fe <- as.data.frame(summary(model)$coefficients)
  fe$Variable <- rownames(fe)

  # Map formula terms to readable descriptions
  fe$Description <- vapply(fe$Variable, function(term) {
    # Strip log() wrapper if present
    raw <- gsub("^log\\((.+)\\)$", "\\1", term)
    lbl <- var_label(raw)
    if (grepl("^log\\(", term)) lbl <- paste0("log(", lbl, ")")
    lbl
  }, character(1))

  diag$fixed_effects <- fe
  message("  Fixed effects: ", nrow(fe), " parameters")

  # Print labelled coefficients
  message("\n  Fixed effect estimates:")
  for (i in seq_len(nrow(fe))) {
    est <- round(fe[["Estimate"]][i], 5)
    pval <- if ("Pr(>|t|)" %in% names(fe)) {
      p <- fe[["Pr(>|t|)"]][i]
      if (is.na(p)) "" else if (p < 0.001) " ***" else if (p < 0.01) " **" else if (p < 0.05) " *" else ""
    } else ""
    message(sprintf("    %-55s  %10.5f%s", fe$Description[i], est, pval))
  }

  # Random effects variance components
  vc <- as.data.frame(VarCorr(model))
  diag$var_components <- vc

  # Sigma (residual SD)
  diag$sigma <- sigma(model)
  message("  Residual sigma: ", round(diag$sigma, 4))

  # Number of observations and groups
  diag$n_obs <- nobs(model)
  diag$n_groups <- sapply(ranef(model), nrow)
  message("  Observations: ", diag$n_obs)
  message("  Groups: ", paste(names(diag$n_groups), "=", diag$n_groups, collapse = ", "))

  diag
}


# ---- Main execution ----

if (sys.nframe() == 0) {

  # Load the final panel
  panel_path <- here::here("data", "panel_data.rds")
  if (!file.exists(panel_path)) {
    stop("panel_data.rds not found. Run 04_compute_derived.R first.")
  }

  panel <- readRDS(panel_path)
  message("Loaded panel: ", nrow(panel), " rows, ", ncol(panel), " cols\n")

  # Prepare modelling dataset (shared by both analyses)
  model_data <- prepare_model_data(panel)

  # Save modelling data for reproducibility
  saveRDS(model_data, here::here("data", "model_data.rds"))


  # =====================================================================
  # ANALYSIS A: FULL PANEL — year_label as random effect
  # =====================================================================

  message("\n", strrep("=", 70))
  message("ANALYSIS A: FULL PANEL MODELS (all years, year as random effect)")
  message(strrep("=", 70))

  panel_models <- list()
  panel_diagnostics <- list()

  for (model_name in names(OUTCOMES)) {
    outcome_var <- OUTCOMES[[model_name]]
    label <- paste0(
      switch(model_name,
             all = "All Pupils",
             disadvantaged = "Disadvantaged Pupils",
             non_disadvantaged = "Non-Disadvantaged Pupils"),
      " [Panel]"
    )

    panel_models[[model_name]] <- fit_attainment_model(
      outcome_var, model_data,
      random_effects = PANEL_RANDOM,
      label = label
    )
    panel_diagnostics[[model_name]] <- run_diagnostics(
      panel_models[[model_name]], label
    )
  }

  # Save panel models
  saveRDS(panel_models, here::here("data", "models.rds"))
  saveRDS(panel_diagnostics, here::here("data", "model_diagnostics.rds"))
  message("\nPanel models saved to: data/models.rds")


  # =====================================================================
  # ANALYSIS B: PER-YEAR MODELS — separate model for each academic year
  # =====================================================================

  message("\n", strrep("=", 70))
  message("ANALYSIS B: PER-YEAR MODELS (separate model for each year)")
  message(strrep("=", 70))

  year_levels <- sort(unique(as.character(model_data$year_label)))
  message("Years to fit: ", paste(year_levels, collapse = ", "))

  yearly_models <- list()
  yearly_diagnostics <- list()

  for (yr in year_levels) {

    message("\n", strrep("-", 60))
    message("  YEAR: ", yr)
    message(strrep("-", 60))

    # Subset to this year and drop unused factor levels
    yr_data <- model_data %>%
      filter(year_label == yr) %>%
      droplevels()

    message("  Rows for ", yr, ": ", nrow(yr_data))

    yearly_models[[yr]] <- list()
    yearly_diagnostics[[yr]] <- list()

    for (model_name in names(OUTCOMES)) {
      outcome_var <- OUTCOMES[[model_name]]
      label <- paste0(
        switch(model_name,
               all = "All Pupils",
               disadvantaged = "Disadvantaged Pupils",
               non_disadvantaged = "Non-Disadvantaged Pupils"),
        " [", yr, "]"
      )

      # Attempt to fit; some year/outcome combos may have too few observations
      yearly_models[[yr]][[model_name]] <- tryCatch({
        fit_attainment_model(
          outcome_var, yr_data,
          random_effects = PERYEAR_RANDOM,
          label = label
        )
      }, error = function(e) {
        warning("  Failed to fit ", label, ": ", e$message)
        NULL
      })

      if (!is.null(yearly_models[[yr]][[model_name]])) {
        yearly_diagnostics[[yr]][[model_name]] <- run_diagnostics(
          yearly_models[[yr]][[model_name]], label
        )
      } else {
        yearly_diagnostics[[yr]][[model_name]] <- list(
          r2_marginal = NA, r2_conditional = NA,
          n_obs = 0, sigma = NA,
          note = paste("Model fitting failed for", yr, model_name)
        )
      }
    }
  }

  # Save per-year models
  saveRDS(yearly_models, here::here("data", "models_yearly.rds"))
  saveRDS(yearly_diagnostics, here::here("data", "model_diagnostics_yearly.rds"))
  message("\nPer-year models saved to: data/models_yearly.rds")


  # =====================================================================
  # PRE-COMPUTE PREDICTIONS (from panel models, for the Shiny app)
  # =====================================================================

  message("\n", strrep("=", 70))
  message("PRE-COMPUTING PREDICTIONS (from panel models)")
  message(strrep("=", 70))

  for (model_name in names(panel_models)) {
    outcome_var <- OUTCOMES[[model_name]]
    model <- panel_models[[model_name]]

    pred_col <- paste0("predicted_", outcome_var)
    resid_col <- paste0("residual_", outcome_var)

    # Predict on log scale then back-transform with bias correction
    bias_factor <- exp(0.5 * sigma(model)^2)

    panel[[pred_col]] <- tryCatch({
      log_pred <- predict(model, newdata = panel, re.form = NULL, allow.new.levels = TRUE)
      exp(log_pred) * bias_factor
    }, error = function(e) {
      message("  Warning: prediction failed for some rows in ", model_name, ": ", e$message)
      NA_real_
    })

    panel[[resid_col]] <- panel[[outcome_var]] - panel[[pred_col]]

    n_pred <- sum(!is.na(panel[[pred_col]]))
    message("  ", model_name, ": ", n_pred, " predictions computed")

    if (n_pred > 0) {
      r2 <- cor(panel[[outcome_var]], panel[[pred_col]], use = "complete.obs")^2
      message("  ", model_name, " R2 (observed vs predicted): ", round(r2, 4))
    }
  }

  # Save panel with predictions
  saveRDS(panel, here::here("data", "panel_data.rds"))
  message("\nUpdated panel_data.rds with predictions")


  # =====================================================================
  # SUMMARY
  # =====================================================================

  message("\n", strrep("=", 70))
  message("MODEL FITTING COMPLETE")
  message(strrep("=", 70))

  message("\n--- Analysis A: Panel Models (year as random effect) ---")
  message("Saved to: data/models.rds, data/model_diagnostics.rds")
  for (mn in names(panel_diagnostics)) {
    d <- panel_diagnostics[[mn]]
    message("\n  ", toupper(mn), ":")
    message("    R2 marginal: ", round(d$r2_marginal, 4))
    message("    R2 conditional: ", round(d$r2_conditional, 4))
    message("    N obs: ", d$n_obs)
    message("    Sigma: ", round(d$sigma, 4))
  }

  message("\n--- Analysis B: Per-Year Models ---")
  message("Saved to: data/models_yearly.rds, data/model_diagnostics_yearly.rds")
  for (yr in names(yearly_diagnostics)) {
    message("\n  Year: ", yr)
    for (mn in names(yearly_diagnostics[[yr]])) {
      d <- yearly_diagnostics[[yr]][[mn]]
      if (is.na(d$r2_marginal)) {
        message("    ", mn, ": FAILED (insufficient data)")
      } else {
        message(sprintf("    %-22s  R2m=%.4f  R2c=%.4f  N=%d  sigma=%.4f",
                        mn, d$r2_marginal, d$r2_conditional, d$n_obs, d$sigma))
      }
    }
  }

  message("\nPredictions (from panel models) saved to: data/panel_data.rds")
}
