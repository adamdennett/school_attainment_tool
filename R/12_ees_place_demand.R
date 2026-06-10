# 12_ees_place_demand.R - Place and demand layer: residence-based absence,
#                         pupil projections, pupil yield from housing
# -------------------------------------------------------------------------------
# Cross-phase place/demand datasets from Explore Education Statistics,
# supporting the spatial programme (residence vs school geography) and the
# falling-rolls / closure-risk work (demand side):
#
#   absence_residence_idaci - absence by PUPIL RESIDENCE (not school location):
#                             IDACI decile x rurality, down to LAD level.
#                             Where deprived children live vs where they are
#                             absent - the residence-geography counterpart to
#                             the school-based absence in the panels.
#                             2023-24 onwards only (new DfE breakdown).
#   pupil_projections       - national pupil numbers by single year of age,
#                             actuals + principal projection to 2030 (the
#                             demographic driver of falling rolls).
#   pupil_yield             - pupils generated per new dwelling, by LA/LAD,
#                             tenure, housing type, bedrooms and phase (the
#                             housing-development side of place planning).
#
# Outputs in data/place/. Run from project root:  Rscript R/12_ees_place_demand.R
# -------------------------------------------------------------------------------

source(here::here("R", "helpers.R"))

EES_DATASET_URL <- "https://explore-education-statistics.service.gov.uk/data-catalogue/data-set/%s/csv"

PLACE_DATASETS <- c(
  absence_residence_idaci = "6dd7f70e-440f-4c0f-b8d1-056678c25301",
  pupil_projections       = "bac00d19-5ae1-4262-8de6-bdfa6b6bb801",
  pupil_yield             = "9603b379-5a44-4d3e-b295-2941c378359a"
)

cache_dir <- here::here("data", "cache")
place_dir <- here::here("data", "place")
dir.create(place_dir, showWarnings = FALSE, recursive = TRUE)

read_ees_place <- function(name) {
  csv_path <- file.path(cache_dir, paste0("ees_", name, ".csv"))
  download_cached(sprintf(EES_DATASET_URL, PLACE_DATASETS[[name]]), csv_path)
  df <- read_csv(csv_path, na = NA_CODES, show_col_types = FALSE, guess_max = 50000)
  message("  ", name, ": ", nrow(df), " rows, ", ncol(df), " cols")
  df
}


# ---- 1. Absence by pupil residence (IDACI x rurality) ----

build_absence_residence <- function(save = TRUE) {
  message("Absence by pupil residency (IDACI / rurality) ...")
  out <- read_ees_place("absence_residence_idaci") %>%
    transmute(
      time_period,
      geographic_level,            # National / Regional / LA / LAD
      region_name, la_name, lad_code, lad_name,
      measure,                     # "IDACI" / "Rurality" / "IDACI and rurality" / "Total"
      education_phase,
      idaci = pup_idaci,
      rurality = pup_rurality,
      enrolments = suppressWarnings(as.numeric(enrolments)),
      overall_absence_rate = suppressWarnings(as.numeric(sess_overall_percent)),
      authorised_rate = suppressWarnings(as.numeric(sess_authorised_percent)),
      unauthorised_rate = suppressWarnings(as.numeric(sess_unauthorised_percent))
    )
  if (save) {
    saveRDS(out, file.path(place_dir, "absence_residence_idaci.rds"))
    message("Saved absence_residence_idaci.rds (", nrow(out), " rows)")
  }
  out
}


# ---- 2. National pupil projections ----

build_pupil_projections <- function(save = TRUE) {
  message("National pupil projections ...")
  out <- read_ees_place("pupil_projections") %>%
    mutate(
      pupil_count = suppressWarnings(as.numeric(pupil_count)),
      model_year = suppressWarnings(as.integer(model_year))
    )
  if (save) {
    saveRDS(out, file.path(place_dir, "pupil_projections.rds"))
    message("Saved pupil_projections.rds (", nrow(out), " rows)")
  }
  out
}


# ---- 3. Pupil yield from housing developments ----

build_pupil_yield <- function(save = TRUE) {
  message("Pupil yield from housing developments (large file) ...")
  out <- read_ees_place("pupil_yield") %>%
    transmute(
      time_period,
      geographic_level,
      la_name, lad_code, lad_name,
      tenure, housing, number_of_bedrooms,
      education_phase, education_type,
      number_of_pupils = suppressWarnings(as.numeric(number_of_pupils)),
      completed_properties = suppressWarnings(as.numeric(completed_properties_in_ay)),
      pupil_yield = suppressWarnings(as.numeric(pupil_yield))
    )
  if (save) {
    saveRDS(out, file.path(place_dir, "pupil_yield.rds"))
    message("Saved pupil_yield.rds (", nrow(out), " rows)")
  }
  out
}


# ---- Main execution (Rscript only) ----
if (sys.nframe() == 0) {
  abs_res <- build_absence_residence()
  proj    <- build_pupil_projections()
  yield   <- build_pupil_yield()

  message("\n=== Absence by residence IDACI decile, national, latest year ===")
  abs_res %>%
    filter(geographic_level == "National", measure == "IDACI",
           education_phase == "Total", time_period == max(time_period)) %>%
    select(idaci, overall_absence_rate, unauthorised_rate) %>%
    arrange(idaci) %>%
    print(n = 15)

  # Note structure: Headcount rows exist only for under-5s (part-time effects);
  # all other ages are FTE, and pupil_age == "All" is the total series.
  message("\n=== Pupil projections: state primary & secondary FTE by model year ===")
  proj %>%
    filter(pupil_status == "FTE", pupil_age == "All",
           establishment_type_group %in% c("State primary", "State secondary")) %>%
    select(establishment_type_group, model_year, population_measure, pupil_count) %>%
    tidyr::pivot_wider(names_from = establishment_type_group, values_from = pupil_count) %>%
    arrange(model_year) %>%
    print(n = 25)

  message("\n=== Pupil yield per dwelling by phase (national means) ===")
  yield %>%
    filter(!is.na(pupil_yield)) %>%
    group_by(education_phase) %>%
    summarise(
      mean_yield = round(mean(pupil_yield, na.rm = TRUE), 3),
      areas = n_distinct(lad_name),
      .groups = "drop"
    ) %>%
    print(n = 10)
}
