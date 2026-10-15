# run_pipeline.R — Run the full data pipeline from scratch
# Usage: source("run_pipeline.R")  (from the project root)
# ============================================================
#
# Pipeline steps:
#   01   Extract raw data from DfE APIs        → data/year_data_list.rds
#   02   Build school-level panel               → data/panel_raw.rds
#   03   Add workforce census data              → data/panel_with_workforce.rds
#   03a  Add financial data (CFR/AAR)           → data/panel_with_finance.rds, data/financial_returns.rds
#   04   Compute derived variables              → data/panel_data.rds (final panel)
#   05   Fit prediction models                  → data/models*.rds
#   06   Prepare slim app data bundle           → app/data/*
#   07   LA typology & clustering               → data/la_*.rds, cluster_*.rds
#   08   Standalone cluster map                 → output/la_cluster_map.html
#
# Each step depends on the output of the previous step.
# Set SKIP_MODELS = TRUE below to skip step 05 (slow, ~50MB per model).
# Set START_FROM to skip steps you've already run (e.g. START_FROM = 7).
# Step numbers use the script naming: 1, 2, 3, 3.5 (for 03a), 4, 5, 6, 7, 8.
#
# Each step runs in a FRESH R PROCESS, not via source(). Every pipeline
# script guards its body with `if (sys.nframe() == 0)` so that sourcing it
# only defines its functions - which means source()ing them from here
# executed nothing at all, and every step reported success in 0 minutes
# having done no work. Running them as scripts is also how they are
# documented to be used, and it keeps one step's objects and attached
# packages out of the next one's environment.
# ============================================================

SKIP_MODELS <- FALSE   # Set TRUE to skip 05_fit_model.R (saves ~10 min)
START_FROM  <- 1       # Set to a later step number to resume partway

RSCRIPT <- file.path(R.home("bin"), "Rscript")
stopifnot(file.exists(RSCRIPT) || nzchar(Sys.which("Rscript")))
if (!file.exists(RSCRIPT)) RSCRIPT <- unname(Sys.which("Rscript"))

# htmlwidgets::saveWidget(selfcontained = TRUE) shells out to pandoc, which
# RStudio supplies via RSTUDIO_PANDOC but a bare Rscript does not. Quarto
# ships one, so point at that when nothing else is on the PATH - otherwise
# step 8 dies at the last line having done all its work.
if (!nzchar(Sys.getenv("RSTUDIO_PANDOC")) && !nzchar(Sys.which("pandoc"))) {
  for (cand in c(file.path(Sys.getenv("ProgramFiles"), "Quarto", "bin", "tools"),
                 "/usr/lib/rstudio/bin/quarto/bin/tools",
                 "/Applications/quarto/bin/tools")) {
    if (any(file.exists(file.path(cand, c("pandoc", "pandoc.exe"))))) {
      Sys.setenv(RSTUDIO_PANDOC = cand)
      cat("  pandoc: using", cand, "\n")
      break
    }
  }
}

cat("\n========================================\n")
cat("  School Attainment Tool — Full Pipeline\n")
cat("========================================\n\n")

t0 <- Sys.time()

steps <- list(
  list(num = 1,   script = "R/01_extract_data.R",    desc = "Extract raw data from DfE"),
  list(num = 2,   script = "R/02_build_panel.R",      desc = "Build school-level panel"),
  list(num = 3,   script = "R/03_add_workforce.R",    desc = "Add workforce census data"),
  list(num = 3.5, script = "R/03a_add_finance.R",     desc = "Add financial data (CFR/AAR)"),
  list(num = 4,   script = "R/04_compute_derived.R",  desc = "Compute derived variables"),
  list(num = 5,   script = "R/05_fit_model.R",        desc = "Fit prediction models"),
  list(num = 6,   script = "R/06_prepare_app_data.R", desc = "Prepare app data bundle"),
  list(num = 7,   script = "R/07_la_typology.R",      desc = "LA typology & clustering"),
  list(num = 8,   script = "R/08_la_cluster_map.R",   desc = "Standalone cluster map")
)

n_steps <- length(steps)

for (i in seq_along(steps)) {
  step <- steps[[i]]
  if (step$num < START_FROM) {
    cat(sprintf("  [%d/%d] SKIPPED (START_FROM = %s): %s\n", i, n_steps, START_FROM, step$desc))
    next
  }
  if (step$num == 5 && SKIP_MODELS) {
    cat(sprintf("  [%d/%d] SKIPPED (SKIP_MODELS = TRUE): %s\n", i, n_steps, step$desc))
    next
  }

  cat(sprintf("\n--- [%d/%d] %s ---\n", i, n_steps, step$desc))
  cat(sprintf("    Script: %s\n", step$script))
  t1 <- Sys.time()

  status <- system2(RSCRIPT, shQuote(step$script))
  elapsed <- round(difftime(Sys.time(), t1, units = "mins"), 1)

  if (!identical(status, 0L)) {
    cat(sprintf("\n    ERROR in step %s: %s exited with status %s\n",
                step$num, step$script, status))
    cat("    Pipeline halted. Fix the error and re-run with START_FROM =",
        step$num, "\n")
    stop("Pipeline failed at step ", step$num)
  }
  cat(sprintf("    Done in %s min\n", elapsed))
}

total <- round(difftime(Sys.time(), t0, units = "mins"), 1)
cat(sprintf("\n========================================\n"))
cat(sprintf("  Pipeline complete in %s min\n", total))
cat(sprintf("========================================\n\n"))
