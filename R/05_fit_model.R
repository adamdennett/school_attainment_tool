# 05_fit_model.R - Fit multilevel models (x3 outcomes) and run diagnostics
# -------------------------------------------------------------------------
# This script:
#   1. Prepares the modelling dataset (complete cases for model variables)
#   2. Fits three multilevel models: all pupils, disadvantaged, non-disadvantaged
#   3. Runs diagnostics (ICC, R-squared, residual checks)
#   4. Saves model objects and diagnostics
#
# Model specification (extending lme11 from week8_practical.qmd):
#   log(outcome) ~ log(PTFSM6CLA1A) + log(PERCTOT) + log(PNUMEAL) +
#     PTPRIORLO + ADMPOL_PT + gorard_segregation +
#     log(remained_in_the_same_school) +
#     teachers_on_leadership_pay_range_percent +
#     log(average_number_of_days_taken) +
#     year_numeric +
#     (1 | OFSTEDRATING) + (1 | gor_name/LANAME)
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

# Predictor variables used in the model
# (on original scale - log transforms applied in the formula)
MODEL_PREDICTORS <- c(
  "PTFSM6CLA1A", "PERCTOT", "PNUMEAL",
  "PTPRIORLO", "ADMPOL_PT", "gorard_segregation",
  "remained_in_the_same_school",
  "teachers_on_leadership_pay_range_percent",
  "average_number_of_days_taken",
  "year_numeric"
)

# Grouping variables for random effects
MODEL_GROUPING <- c("OFSTEDRATING", "gor_name", "LANAME")


#' Prepare the modelling dataset
#'
#' Filters to complete cases for all model variables and ensures
#' positive values for log-transformed variables.
prepare_model_data <- function(panel) {

  message("Preparing modelling dataset ...")

  # All variables needed
  all_vars <- c(
    unlist(OUTCOMES),
    MODEL_PREDICTORS,
    MODEL_GROUPING,
    "URN", "SCHNAME", "academic_year", "year_label"
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
      LANAME = factor(LANAME)
    ) %>%
    # Drop unused factor levels
    droplevels()

  message("  Ofsted levels: ", paste(levels(model_data_full$OFSTEDRATING), collapse = ", "))
  message("  Regions: ", n_distinct(model_data_full$gor_name))
  message("  LAs: ", n_distinct(model_data_full$LANAME))
  message("  Years: ", paste(sort(unique(model_data_full$year_label)), collapse = ", "))

  model_data_full
}


#' Fit a single multilevel model
#'
#' @param outcome_var Character name of the outcome variable
#' @param data Prepared modelling dataframe
#' @param label Human-readable label for messages
#' @return A fitted lmerMod object
fit_attainment_model <- function(outcome_var, data, label = outcome_var) {

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
    "log(PTFSM6CLA1A) + log(PERCTOT) + log(PNUMEAL) + ",
    "PTPRIORLO + ADMPOL_PT + gorard_segregation + ",
    "log(remained_in_the_same_school) + ",
    "teachers_on_leadership_pay_range_percent + ",
    "log(average_number_of_days_taken) + ",
    "year_numeric + ",
    "(1 | OFSTEDRATING) + (1 | gor_name/LANAME)"
  )

  formula <- as.formula(formula_str)
  message("  Formula: ", formula_str)

  # Also show human-readable version
  formula_readable <- paste0(
    "log(", var_label(outcome_var), ") ~ ",
    "log(", var_label("PTFSM6CLA1A"), ") + log(", var_label("PERCTOT"), ") + log(", var_label("PNUMEAL"), ") + ",
    var_label("PTPRIORLO"), " + ", var_label("ADMPOL_PT"), " + ", var_label("gorard_segregation"), " + ",
    "log(", var_label("remained_in_the_same_school"), ") + ",
    var_label("teachers_on_leadership_pay_range_percent"), " + ",
    "log(", var_label("average_number_of_days_taken"), ") + ",
    var_label("year_numeric"), " + ",
    "(1 | ", var_label("OFSTEDRATING"), ") + (1 | ", var_label("gor_name"), "/", var_label("LANAME"), ")"
  )
  message("  Readable: ", formula_readable)

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

  # Prepare modelling dataset
  model_data <- prepare_model_data(panel)

  # Save modelling data for reproducibility
  saveRDS(model_data, here::here("data", "model_data.rds"))

  # Fit all three models
  models <- list()
  diagnostics <- list()

  for (model_name in names(OUTCOMES)) {
    outcome_var <- OUTCOMES[[model_name]]
    label <- switch(model_name,
                    all = "All Pupils",
                    disadvantaged = "Disadvantaged Pupils",
                    non_disadvantaged = "Non-Disadvantaged Pupils")

    models[[model_name]] <- fit_attainment_model(outcome_var, model_data, label)
    diagnostics[[model_name]] <- run_diagnostics(models[[model_name]], label)
  }

  # Save models and diagnostics
  saveRDS(models, here::here("data", "models.rds"))
  saveRDS(diagnostics, here::here("data", "model_diagnostics.rds"))

  # Pre-compute predictions for the full panel (for the Shiny app)
  message("\n=== Pre-computing predictions ===")

  for (model_name in names(models)) {
    outcome_var <- OUTCOMES[[model_name]]
    model <- models[[model_name]]

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

  # ---- Summary ----
  message("\n=== MODEL FITTING COMPLETE ===")
  message("Models saved to: data/models.rds")
  message("Diagnostics saved to: data/model_diagnostics.rds")
  message("Panel with predictions saved to: data/panel_data.rds")

  for (mn in names(diagnostics)) {
    d <- diagnostics[[mn]]
    message("\n", toupper(mn), ":")
    message("  R2 marginal: ", round(d$r2_marginal, 4))
    message("  R2 conditional: ", round(d$r2_conditional, 4))
    message("  N obs: ", d$n_obs)
    message("  Sigma: ", round(d$sigma, 4))
  }
}
