# 00_fetch_data.R - Rebuild the data files that are not in git
# ---------------------------------------------------------------------------
# Most of data/ is git-ignored because every file in it can be rebuilt from
# public sources by a script already in the repo (MACHINE_SYNC_TODO.md,
# step 2). This runs those scripts, in an order that respects what each
# needs, for whichever outputs are missing. On a fresh clone it fetches
# everything; on a machine that has the data it does nothing.
#
#   Rscript R/00_fetch_data.R               # build whatever is missing
#   Rscript R/00_fetch_data.R --dry-run     # say what would run, run nothing
#   Rscript R/00_fetch_data.R --all         # rebuild every step regardless
#   Rscript R/00_fetch_data.R ks2 census    # just these steps (missing or not)
#   Rscript R/00_fetch_data.R --analysis-f  # also render model_experiments.qmd
#
# Each step runs in its own Rscript process, so one script's packages and
# globals cannot leak into the next, and a failure is reported and the
# rest carry on. Every step downloads from DfE, EES, GIAS, ONS or nomis, so
# it needs network access and can take a while.
#
# Not covered here: the core panel and models (R/01 to R/05, which need the
# performance-tables extracts in data/extracted/) and the Sussex area files
# (R/17 onward). This is the layer the decks and destination analyses read.
# ---------------------------------------------------------------------------

# What each step writes, and which steps it needs first. Order matters:
# R/20 uses the GIAS closures from R/10, and R/13 validates against the KS2
# panel from R/20.
STEPS <- list(
  gias = list(script = "R/10_gias_closures.R",
              outputs = c("data/gias/gias_establishments.rds", "data/gias/school_closures.rds")),
  # Ofsted's newest monthly release. Only fetched when missing: to move to a
  # newer month, run it by name (Rscript R/00_fetch_data.R ofsted_latest)
  # or R/15b_ofsted_latest.R --force, then rebuild the area files
  ofsted_latest = list(script = "R/15b_ofsted_latest.R",
              outputs = "data/ofsted/ofsted_latest.rds"),
  ks4_destinations = list(script = "R/09_ks4_destinations.R",
              outputs = "data/ks4_destinations.rds"),
  engagement = list(script = "R/11_ees_engagement.R",
              outputs = c("data/engagement/absence_by_yeargroup.rds",
                          "data/engagement/suspensions_by_yeargroup.rds",
                          "data/engagement/suspensions_school.rds")),
  place = list(script = "R/12_ees_place_demand.R",
              outputs = c("data/place/absence_residence_idaci.rds",
                          "data/place/pupil_projections.rds", "data/place/pupil_yield.rds")),
  early_years = list(script = "R/14_early_years_baselines.R",
              outputs = "data/early_years/la_entry_baselines.rds"),
  ks2 = list(script = "R/20_extract_ks2.R", needs = "gias",
              outputs = "data/ks2/ks2_panel.rds"),
  census = list(script = "R/13_census_context.R", needs = "ks2",
              outputs = c("data/census/school_census_context.rds", "data/cache/oa_pwc_2021.rds")),
  ks5 = list(script = "R/30_extract_ks5.R",
              outputs = "data/ks5/ks5_panel.rds"),
  # Opt-in (--analysis-f): renders the whole of model_experiments.qmd,
  # which fits many models, to produce one small cache. Analysis F is the
  # thing to promote into a script of its own.
  analysis_f = list(render = "output/model_experiments.qmd", opt_in = TRUE,
              outputs = "data/cache/analysis_f_results.rds"))

root <- function(...) here::here(...)
missing_outputs <- function(step) STEPS[[step]]$outputs[!file.exists(root(STEPS[[step]]$outputs))]

#' Run one step in its own process; TRUE if it succeeded and wrote its outputs
run_step <- function(name) {
  s <- STEPS[[name]]
  t0 <- Sys.time()
  status <- if (!is.null(s$script)) {
    system2(file.path(R.home("bin"), "Rscript"), shQuote(root(s$script)))
  } else {
    system2("quarto", c("render", shQuote(root(s$render))))
  }
  still <- missing_outputs(name)
  mins <- round(as.numeric(difftime(Sys.time(), t0, units = "mins")), 1)
  if (status != 0 || length(still)) {
    message(sprintf("  FAILED %s after %s min (exit %s)%s", name, mins, status,
                    if (length(still)) paste0("; still missing: ", paste(still, collapse = ", ")) else ""))
    return(FALSE)
  }
  message(sprintf("  done %s in %s min", name, mins))
  TRUE
}

fetch_data <- function(only = NULL, all = FALSE, analysis_f = FALSE, dry_run = FALSE) {
  unknown <- setdiff(only, names(STEPS))
  if (length(unknown)) stop("Unknown step(s): ", paste(unknown, collapse = ", "),
                            ". Steps: ", paste(names(STEPS), collapse = ", "))
  wanted <- names(STEPS)
  if (length(only)) wanted <- only
  else {
    wanted <- wanted[!vapply(STEPS[wanted], function(s) isTRUE(s$opt_in), logical(1)) |
                       (wanted == "analysis_f" & analysis_f)]
    if (!all) wanted <- wanted[vapply(wanted, function(w) length(missing_outputs(w)) > 0, logical(1))]
  }
  # Pull in anything a wanted step needs whose outputs are missing, and
  # what that needs in turn, until nothing new is added
  repeat {
    add <- unique(unlist(lapply(STEPS[wanted], `[[`, "needs")))
    add <- add[!add %in% wanted & vapply(add, function(n) length(missing_outputs(n)) > 0, logical(1))]
    if (!length(add)) break
    wanted <- c(add, wanted)
  }
  wanted <- names(STEPS)[names(STEPS) %in% wanted]          # keep the dependency order

  if (!length(wanted)) { message("Nothing to fetch: every output is present."); return(invisible(TRUE)) }
  message("Steps to run, in order:")
  for (w in wanted) message(sprintf("  %-17s %s -> %s", w,
    STEPS[[w]]$script %||% STEPS[[w]]$render, paste(STEPS[[w]]$outputs, collapse = ", ")))
  if (dry_run) return(invisible(TRUE))

  ok <- vapply(wanted, function(w) {
    blocked <- STEPS[[w]]$needs[vapply(STEPS[[w]]$needs, function(n) length(missing_outputs(n)) > 0, logical(1))]
    if (length(blocked)) { message("  SKIPPED ", w, ": needs ", paste(blocked, collapse = ", ")); return(FALSE) }
    message("\n== ", w); run_step(w)
  }, logical(1))
  message("\n", sum(ok), " of ", length(ok), " steps succeeded",
          if (any(!ok)) paste0("; failed or skipped: ", paste(names(ok)[!ok], collapse = ", ")) else "")
  invisible(all(ok))
}

`%||%` <- function(a, b) if (is.null(a)) b else a

if (sys.nframe() == 0) {
  args <- commandArgs(trailingOnly = TRUE)
  flags <- args[startsWith(args, "--")]
  bad <- setdiff(flags, c("--all", "--dry-run", "--analysis-f"))
  if (length(bad)) stop("Unknown option(s): ", paste(bad, collapse = ", "))
  ok <- fetch_data(only = setdiff(args, flags), all = "--all" %in% flags,
                   analysis_f = "--analysis-f" %in% flags, dry_run = "--dry-run" %in% flags)
  if (!ok) quit(status = 1)
}
