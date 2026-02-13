# 06_prepare_app_data.R - Create lightweight data bundle for the Shiny app
# --------------------------------------------------------------------------
# The full lme4 model objects (models.rds, models_core.rds) are 100MB+.
# This script pre-extracts everything the Shiny app needs into small,
# deployment-friendly files.
#
# The app uses predict_slim() instead of lme4::predict(), so it only needs
# the extracted coefficients (~11KB) rather than the full model objects.
#
# Depends on: R/05_fit_model.R (produces all model and data RDS files)
# Run from project root: source("R/06_prepare_app_data.R")
# --------------------------------------------------------------------------

source(here::here("R", "helpers.R"))
library(lme4)

app_data_dir <- here::here("app", "data")
dir.create(app_data_dir, showWarnings = FALSE, recursive = TRUE)

message("=== Preparing lightweight app data bundle ===\n")

# ---- 1. panel_data (already the right size, just copy) ----
panel_data <- readRDS(here::here("data", "panel_data.rds"))
saveRDS(panel_data, file.path(app_data_dir, "panel_data.rds"))
message("  panel_data.rds: ", nrow(panel_data), " rows, ",
        round(file.size(file.path(app_data_dir, "panel_data.rds")) / 1e6, 1), " MB")

# ---- 2. school_lookup and la_lookup (tiny) ----
file.copy(here::here("data", "school_lookup.rds"),
          file.path(app_data_dir, "school_lookup.rds"), overwrite = TRUE)
file.copy(here::here("data", "la_lookup.rds"),
          file.path(app_data_dir, "la_lookup.rds"), overwrite = TRUE)
message("  school_lookup.rds + la_lookup.rds copied")

# ---- 3. diagnostics (already small, just copy) ----
file.copy(here::here("data", "model_diagnostics.rds"),
          file.path(app_data_dir, "model_diagnostics.rds"), overwrite = TRUE)
file.copy(here::here("data", "model_diagnostics_core.rds"),
          file.path(app_data_dir, "model_diagnostics_core.rds"), overwrite = TRUE)
message("  model_diagnostics.rds + model_diagnostics_core.rds copied")

# ---- 4. Pre-compute fitted/residuals for the Model Info tab ----
# (This replaces the need for the 45MB+ models.rds at runtime)
models <- readRDS(here::here("data", "models.rds"))
core_models <- readRDS(here::here("data", "models_core.rds"))

model_resid_data <- list()
for (mn in names(models)) {
  m <- models[[mn]]
  model_resid_data[[mn]] <- data.frame(
    fitted = fitted(m),
    residual = residuals(m)
  )
  message("  Residuals for full model '", mn, "': ", nrow(model_resid_data[[mn]]), " obs")
}
saveRDS(model_resid_data, file.path(app_data_dir, "model_resid_data.rds"))
message("  model_resid_data.rds: ",
        round(file.size(file.path(app_data_dir, "model_resid_data.rds")) / 1e6, 2), " MB")

# ---- 5. Slim core models for the simulator ----
# Extract ONLY the fixed-effect coefficients, random effect BLUPs, and
# factor levels needed by predict_slim(). This replaces the 55MB+ lme4
# model objects with an 11KB file.
slim_models <- list()
for (mn in names(core_models)) {
  m <- core_models[[mn]]

  slim_models[[mn]] <- list(
    # Fixed effects
    beta = fixef(m),
    formula_rhs = as.character(formula(m))[3],
    # Random effects (BLUPs) for each grouping factor
    ranef = ranef(m),
    # Sigma for bias correction
    sigma = sigma(m),
    # Model frame levels (needed to identify factor levels)
    ofsted_levels = levels(m@frame$OFSTEDRATING_1),
    gor_levels = levels(m@frame$gor_name),
    la_levels = levels(m@frame$LANAME),
    year_levels = levels(m@frame$year_label),
    # Contrasts used in fitting
    ofsted_contrasts = contrasts(m@frame$OFSTEDRATING_1)
  )

  message("  Slim model '", mn, "': ",
          length(slim_models[[mn]]$beta), " fixed effects, ",
          length(slim_models[[mn]]$ranef), " random effect groups")
}
saveRDS(slim_models, file.path(app_data_dir, "slim_core_models.rds"))
message("  slim_core_models.rds: ",
        round(file.size(file.path(app_data_dir, "slim_core_models.rds")) / 1e6, 2), " MB")


# ---- Summary ----
message("\n=== App data bundle summary ===")
app_files <- list.files(app_data_dir, pattern = "\\.rds$", full.names = TRUE)
total_size <- 0
for (f in sort(app_files)) {
  sz <- file.size(f) / 1e6
  total_size <- total_size + sz
  message(sprintf("  %-40s  %6.2f MB", basename(f), sz))
}
message(sprintf("\n  TOTAL: %.1f MB", total_size))
message("\nCompare with original data/ folder:")
orig_files <- list.files(here::here("data"), pattern = "\\.rds$", full.names = TRUE)
orig_total <- sum(file.size(orig_files)) / 1e6
message(sprintf("  Original data/*.rds: %.1f MB", orig_total))
message(sprintf("  App bundle: %.1f MB (%.0f%% reduction)",
                total_size, (1 - total_size / orig_total) * 100))
