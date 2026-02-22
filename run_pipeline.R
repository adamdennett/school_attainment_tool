# run_pipeline.R — Run the full data pipeline from scratch
# Usage: source("run_pipeline.R")  (from the project root)
# ============================================================
#
# Pipeline steps:
#   01  Extract raw data from DfE APIs        → data/year_data_list.rds
#   02  Build school-level panel               → data/panel_raw.rds
#   03  Add workforce census data              → data/panel_with_workforce.rds
#   04  Compute derived variables              → data/panel_data.rds (final panel)
#   05  Fit prediction models                  → data/models*.rds
#   06  Prepare slim app data bundle           → app/data/*
#   07  LA typology & clustering               → data/la_*.rds, cluster_*.rds
#   08  Standalone cluster map                 → output/la_cluster_map.html
#
# Each step depends on the output of the previous step.
# Set SKIP_MODELS = TRUE below to skip step 05 (slow, ~50MB per model).
# Set START_FROM to skip steps you've already run (e.g. START_FROM = 7).
# ============================================================

SKIP_MODELS <- FALSE   # Set TRUE to skip 05_fit_model.R (saves ~10 min)
START_FROM  <- 1       # Set to a later step number to resume partway

cat("\n========================================\n")
cat("  School Attainment Tool — Full Pipeline\n")
cat("========================================\n\n")

t0 <- Sys.time()

steps <- list(
  list(num = 1, script = "R/01_extract_data.R",   desc = "Extract raw data from DfE"),
  list(num = 2, script = "R/02_build_panel.R",     desc = "Build school-level panel"),
  list(num = 3, script = "R/03_add_workforce.R",   desc = "Add workforce census data"),
  list(num = 4, script = "R/04_compute_derived.R", desc = "Compute derived variables"),
  list(num = 5, script = "R/05_fit_model.R",       desc = "Fit prediction models"),
  list(num = 6, script = "R/06_prepare_app_data.R",desc = "Prepare app data bundle"),
  list(num = 7, script = "R/07_la_typology.R",     desc = "LA typology & clustering"),
  list(num = 8, script = "R/08_la_cluster_map.R",  desc = "Standalone cluster map")
)

for (step in steps) {
  if (step$num < START_FROM) {
    cat(sprintf("  [%d/8] SKIPPED (START_FROM = %d): %s\n", step$num, START_FROM, step$desc))
    next
  }
  if (step$num == 5 && SKIP_MODELS) {
    cat(sprintf("  [%d/8] SKIPPED (SKIP_MODELS = TRUE): %s\n", step$num, step$desc))
    next
  }

  cat(sprintf("\n--- [%d/8] %s ---\n", step$num, step$desc))
  cat(sprintf("    Script: %s\n", step$script))
  t1 <- Sys.time()

  tryCatch({
    source(step$script, local = FALSE)
    elapsed <- round(difftime(Sys.time(), t1, units = "mins"), 1)
    cat(sprintf("    Done in %s min\n", elapsed))
  }, error = function(e) {
    cat(sprintf("\n    ERROR in step %d: %s\n", step$num, conditionMessage(e)))
    cat("    Pipeline halted. Fix the error and re-run with START_FROM =", step$num, "\n")
    stop(e)
  })
}

total <- round(difftime(Sys.time(), t0, units = "mins"), 1)
cat(sprintf("\n========================================\n"))
cat(sprintf("  Pipeline complete in %s min\n", total))
cat(sprintf("========================================\n\n"))
