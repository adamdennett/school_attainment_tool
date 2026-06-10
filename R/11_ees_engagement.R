# 11_ees_engagement.R - Engagement data: absence by year group, suspensions/exclusions
# -------------------------------------------------------------------------------
# Cross-phase behavioural ("engagement") datasets from Explore Education
# Statistics, motivated by the KS3 measurement gap: with no national attainment
# data between ages 11 and 16, absence and suspensions by national curriculum
# year are the best open-data signals of the early-secondary engagement dip.
#
# Sources (EES data catalogue CSV endpoints, publication: "Pupil absence in
# schools in England" and "Suspensions and permanent exclusions in England"):
#   absence_characteristics  - full-year absence by pupil characteristic
#                              (incl. NC year group), national/regional/LA
#   suspensions_school       - termly school-level suspensions & permanent
#                              exclusions (URN-keyed, joinable to panels)
#   suspensions_characteristics - suspensions by characteristic (incl. NC year),
#                              national/regional/LA
#
# Outputs (data/engagement/):
#   absence_by_yeargroup.rds      - geography x year x NC year absence rates
#   suspensions_school.rds        - school x year suspension/exclusion rates
#                                   (full-year totals computed from termly rows)
#   suspensions_by_yeargroup.rds  - geography x year x NC year suspension rates
#
# Run from project root:  Rscript R/11_ees_engagement.R
# -------------------------------------------------------------------------------

source(here::here("R", "helpers.R"))

EES_DATASET_URL <- "https://explore-education-statistics.service.gov.uk/data-catalogue/data-set/%s/csv"

ENGAGEMENT_DATASETS <- c(
  absence_characteristics     = "e2eb493a-dc95-41f4-96ef-b886c5cd2019",
  suspensions_school          = "660fb83a-1238-48f8-b967-f3fb971ad578",
  suspensions_characteristics = "ed60fee6-bdab-414b-82d9-337f914575de"
)

cache_dir <- here::here("data", "cache")
engagement_dir <- here::here("data", "engagement")
dir.create(engagement_dir, showWarnings = FALSE, recursive = TRUE)


#' Download (cached) and read one EES dataset
read_ees_dataset <- function(name) {
  csv_path <- file.path(cache_dir, paste0("ees_", name, ".csv"))
  download_cached(sprintf(EES_DATASET_URL, ENGAGEMENT_DATASETS[[name]]), csv_path)
  df <- read_csv(csv_path, na = NA_CODES, show_col_types = FALSE, guess_max = 50000)
  message("  ", name, ": ", nrow(df), " rows, ", ncol(df), " cols")
  df
}


# ---- 1. Absence by NC year group ----

build_absence_by_yeargroup <- function(save = TRUE) {
  message("Absence by characteristic ...")
  abs_char <- read_ees_dataset("absence_characteristics")

  # Structure: breakdown_topic ("Year group", "FSM", ...) x breakdown
  # ("Year 1 and below" ... "Year 12 and above"); education_phase = Total /
  # State-funded primary / State-funded secondary / Special; full academic
  # years ("Six half terms"), 2018-19 onwards.
  out <- abs_char %>%
    filter(
      breakdown_topic == "Year group",
      geographic_level %in% c("National", "Regional", "Local authority")
    ) %>%
    transmute(
      time_period,
      geographic_level,
      region_name,
      la_name,
      education_phase,
      year_group = breakdown,
      enrolments = suppressWarnings(as.numeric(enrolments)),
      overall_absence_rate = suppressWarnings(as.numeric(sess_overall_percent)),
      persistent_absence_rate = suppressWarnings(as.numeric(enrolments_pa_10_exact_percent))
    )

  message("  Year-group rows kept: ", nrow(out), " (",
          paste(sort(unique(out$year_group)), collapse = ", "), ")")

  if (save) {
    saveRDS(out, file.path(engagement_dir, "absence_by_yeargroup.rds"))
    message("Saved absence_by_yeargroup.rds")
  }
  out
}


# ---- 2. School-level suspensions / permanent exclusions ----

build_suspensions_school <- function(save = TRUE) {
  message("Suspensions - school level ...")
  susp <- read_ees_dataset("suspensions_school")

  # Termly rows (Autumn/Spring/Summer), 2016-17 onwards; the latest year may
  # lack the summer term (publication lag) - n_terms records coverage.
  out <- susp %>%
    filter(geographic_level == "School") %>%
    mutate(
      school_urn = as.character(school_urn),
      suspension = suppressWarnings(as.numeric(suspension)),
      perm_excl = suppressWarnings(as.numeric(perm_excl)),
      headcount = suppressWarnings(as.numeric(headcount))
    ) %>%
    group_by(time_period, school_urn) %>%
    summarise(
      education_phase = first(education_phase),
      la_name = first(la_name),
      n_terms = n(),
      suspensions = sum(suspension, na.rm = TRUE),
      permanent_exclusions = sum(perm_excl, na.rm = TRUE),
      headcount = suppressWarnings(max(headcount, na.rm = TRUE)),
      .groups = "drop"
    ) %>%
    mutate(
      headcount = ifelse(is.finite(headcount), headcount, NA_real_),
      suspension_rate = 100 * suspensions / headcount,
      perm_exclusion_rate = 100 * permanent_exclusions / headcount
    )

  message("  School-year rows: ", nrow(out), "; time periods: ",
          paste(sort(unique(out$time_period)), collapse = ", "))

  if (save) {
    saveRDS(out, file.path(engagement_dir, "suspensions_school.rds"))
    message("Saved suspensions_school.rds")
  }
  out
}


# ---- 3. Suspensions by NC year group ----

build_suspensions_by_yeargroup <- function(save = TRUE) {
  message("Suspensions by characteristic ...")
  susp_char <- read_ees_dataset("suspensions_characteristics")

  # Structure: characteristic_group ("Year group", "Age", "FSM", ...) x
  # characteristic ("Year 7", ...); annual (no terms) in this dataset
  out <- susp_char %>%
    filter(
      characteristic_group == "Year group",
      geographic_level %in% c("National", "Regional", "Local authority")
    ) %>%
    transmute(
      time_period,
      time_identifier,
      year_group = characteristic,
      geographic_level,
      region_name,
      la_name,
      education_phase,
      headcount = suppressWarnings(as.numeric(headcount)),
      suspensions = suppressWarnings(as.numeric(suspension)),
      suspension_rate = suppressWarnings(as.numeric(susp_rate)),
      permanent_exclusions = suppressWarnings(as.numeric(perm_excl)),
      perm_exclusion_rate = suppressWarnings(as.numeric(perm_excl_rate))
    )

  message("  Year-group rows kept: ", nrow(out))

  if (save) {
    saveRDS(out, file.path(engagement_dir, "suspensions_by_yeargroup.rds"))
    message("Saved suspensions_by_yeargroup.rds")
  }
  out
}


# ---- Main execution (Rscript only) ----
if (sys.nframe() == 0) {
  abs_yg  <- build_absence_by_yeargroup()
  susp_sc <- build_suspensions_school()
  susp_yg <- build_suspensions_by_yeargroup()

  message("\n=== National absence by year group, latest year ===")
  abs_yg %>%
    filter(geographic_level == "National", time_period == max(time_period),
           education_phase == "Total") %>%
    select(year_group, overall_absence_rate, persistent_absence_rate) %>%
    arrange(year_group) %>%
    print(n = 40)

  message("\n=== National suspension rate by year group, latest full year ===")
  susp_yg %>%
    filter(geographic_level == "National") %>%
    filter(time_period == max(time_period)) %>%
    select(time_identifier, year_group, suspension_rate) %>%
    arrange(time_identifier, year_group) %>%
    print(n = 60)
}
