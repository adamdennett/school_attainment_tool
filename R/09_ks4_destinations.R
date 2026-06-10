# 09_ks4_destinations.R - KS4 pupil destinations (the secondary -> post-16 bridge)
# -------------------------------------------------------------------------------
# Builds a URN x year destinations dataset from the CSCP KS4 destinations files
# (england_ks4-pupdest.csv, auto-downloaded by 00_download_cscp.R for all years).
#
# Destinations measure: activity in the year AFTER completing KS4, for the
# cohort that finished KS4 two academic years before the publication year
# (e.g. the file in the 2023-24 tables tracks the 2021-22 KS4 cohort through
# 2022-23). Variables are % of cohort sustaining each destination, including
# the post-16 route split (FE / school sixth form / sixth-form college) and
# _DIS disadvantaged-cohort versions.
#
# Output: data/ks4_destinations.rds - standalone, keyed by URN + academic_year
# (the year of the performance tables file). Left-join onto panel_data.rds by
# c("URN", "academic_year") when needed; the main KS4 pipeline is untouched.
#
# Run from project root:  Rscript R/09_ks4_destinations.R
# -------------------------------------------------------------------------------

source(here::here("R", "helpers.R"))
source(here::here("R", "00_download_cscp.R"))

KS4_DEST_VARS <- c(
  "COHORT", "OVERALL_DESTPER", "EDUCATIONPER", "FEPER", "SCH_6THPER",
  "SIXTH_COLPER", "OTHER_EDUPER", "APPRENPER", "EMPLOYMENTPER",
  "NOT_SUSTAINEDPER", "UNKNOWNPER",
  "COHORT_DIS", "OVERALL_DESTPER_DIS", "EDUCATIONPER_DIS", "FEPER_DIS",
  "SCH_6THPER_DIS", "SIXTH_COLPER_DIS", "OTHER_EDUPER_DIS", "APPRENPER_DIS",
  "EMPLOYMENTPER_DIS", "NOT_SUSTAINEDPER_DIS", "UNKNOWNPER_DIS"
)

build_ks4_destinations <- function(save = TRUE) {

  cscp_update(CSCP_PHASE_CONFIG["ks4_destinations"])

  dest <- map_dfr(YEAR_CONFIG$academic_year, function(year) {
    path <- file.path(CSCP_EXTRACTED_DIR, year, "england_ks4-pupdest.csv")
    if (!file.exists(path)) {
      warning("KS4 destinations file missing for ", year)
      return(NULL)
    }
    config <- YEAR_CONFIG %>% filter(academic_year == year)

    read_dfe_csv(path) %>%
      filter(if ("RECTYPE" %in% names(.)) RECTYPE == 1 else TRUE) %>%
      select(URN, any_of(KS4_DEST_VARS)) %>%
      mutate(
        across(-URN, ~ suppressWarnings(readr::parse_number(as.character(.)))),
        academic_year = config$academic_year,
        year_label    = config$year_label,
        year_numeric  = config$year_numeric,
        time_period   = config$time_period
      ) %>%
      rename_with(~ paste0("dest_", .), -c(URN, academic_year, year_label,
                                           year_numeric, time_period))
  })

  message("KS4 destinations: ", nrow(dest), " school-year rows across ",
          n_distinct(dest$academic_year), " years")

  if (save) {
    saveRDS(dest, here::here("data", "ks4_destinations.rds"))
    message("Saved data/ks4_destinations.rds")
  }
  dest
}

summarise_ks4_destinations <- function(dest) {
  dest %>%
    group_by(Year = year_label) %>%
    summarise(
      schools = n(),
      `sustained %` = round(mean(dest_OVERALL_DESTPER, na.rm = TRUE), 1),
      `education %` = round(mean(dest_EDUCATIONPER, na.rm = TRUE), 1),
      `apprenticeship %` = round(mean(dest_APPRENPER, na.rm = TRUE), 1),
      `not sustained %` = round(mean(dest_NOT_SUSTAINEDPER, na.rm = TRUE), 1),
      `gap (overall - dis) pp` = round(
        mean(dest_OVERALL_DESTPER - dest_OVERALL_DESTPER_DIS, na.rm = TRUE), 1),
      .groups = "drop"
    )
}

if (sys.nframe() == 0) {
  dest <- build_ks4_destinations()
  print(summarise_ks4_destinations(dest), width = Inf)
}
