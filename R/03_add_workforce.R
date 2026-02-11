# 03_add_workforce.R - Download and join workforce data from Explore Education Statistics
# ---------------------------------------------------------------------------------------
# This script:
#   1. Downloads teacher turnover, sickness, pay, workforce size, pupil ratios
#   2. Filters to school-level data for relevant time periods
#   3. Selects only the variables needed for the model
#   4. Joins to the panel dataset by URN and time_period
#
# Key variables extracted (used in lme11 model):
#   - remained_in_the_same_school (teacher retention)
#   - teachers_on_leadership_pay_range_percent (leadership pay)
#   - average_number_of_days_taken (teacher sickness days)
#   - teacher_fte_in_census_year (teacher FTEs for computing retention rate)
#
# Depends on: R/02_build_panel.R (produces panel_raw.rds)
# Run from project root: source("R/03_add_workforce.R")
# ---------------------------------------------------------------------------------------

source(here::here("R", "helpers.R"))

cache_dir <- here::here("data", "cache")
dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)

# Time periods we need
NEEDED_TIME_PERIODS <- YEAR_CONFIG$time_period


#' Download a single workforce dataset, filter and select key columns
#' @param name Short name of the dataset
#' @param url Download URL
#' @param select_cols Character vector of columns to keep (besides school_urn, time_period)
#' @return A tibble with school_urn, time_period, and selected columns
download_workforce_dataset <- function(name, url, select_cols = NULL) {
  cache_path <- file.path(cache_dir, paste0("workforce_", name, ".rds"))

  if (file.exists(cache_path)) {
    message("  Loading cached: ", name)
    return(readRDS(cache_path))
  }

  message("  Downloading: ", name, " ...")
  csv_path <- file.path(cache_dir, paste0("workforce_", name, "_raw.csv"))
  download_cached(url, csv_path)

  df <- read_csv(csv_path, na = NA_CODES, show_col_types = FALSE)
  message("    Raw: ", nrow(df), " rows, ", ncol(df), " cols")
  message("    Time periods available: ", paste(sort(unique(df$time_period)), collapse = ", "))

  # Filter to school-level and relevant time periods
  df_filtered <- df %>%
    filter(
      geographic_level == "School",
      time_period %in% NEEDED_TIME_PERIODS
    ) %>%
    mutate(school_urn = as.character(school_urn))

  message("    After filtering: ", nrow(df_filtered), " rows")

  # Select only the columns we need
  if (!is.null(select_cols)) {
    available_cols <- intersect(select_cols, names(df_filtered))
    missing_cols <- setdiff(select_cols, names(df_filtered))
    if (length(missing_cols) > 0) {
      message("    WARNING: Missing columns: ", paste(missing_cols, collapse = ", "))
    }
    df_filtered <- df_filtered %>%
      select(school_urn, time_period, all_of(available_cols))
  } else {
    # Just keep identifiers
    df_filtered <- df_filtered %>%
      select(school_urn, time_period, everything()) %>%
      select(-any_of(c(
        "geographic_level", "country_code", "country_name",
        "region_code", "region_name", "old_la_code", "new_la_code",
        "la_name", "school_laestab", "school_name", "school_type",
        "time_identifier", "number_schools"
      )))
  }

  df_filtered <- df_filtered %>%
    distinct(school_urn, time_period, .keep_all = TRUE)

  message("    Final: ", nrow(df_filtered), " unique school-year rows, ",
          ncol(df_filtered), " cols")

  saveRDS(df_filtered, cache_path)
  df_filtered
}


#' Download all workforce datasets and combine
load_all_workforce <- function(force = FALSE) {
  combined_cache <- file.path(cache_dir, "workforce_combined.rds")

  if (file.exists(combined_cache) && !force) {
    message("Loading cached combined workforce data ...")
    return(readRDS(combined_cache))
  }

  message("Downloading workforce data from Explore Education Statistics ...\n")

  # --- Teacher turnover ---
  turnover <- download_workforce_dataset(
    "turnover",
    WORKFORCE_URLS$turnover,
    select_cols = c(
      "remained_in_the_same_school",
      "entrants_to_the_school",
      "leavers_from_the_school",
      "teacher_fte_in_census_year"
    )
  )

  # --- Teacher pay ---
  pay <- download_workforce_dataset(
    "pay",
    WORKFORCE_URLS$pay,
    select_cols = c(
      "teachers_on_leadership_pay_range_percent",
      "teachers_on_main_pay_range_percent",
      "teachers_on_upper_pay_range_percent"
    )
  )

  # --- Teacher sickness ---
  sickness <- download_workforce_dataset(
    "sickness",
    WORKFORCE_URLS$sickness,
    select_cols = c(
      "average_number_of_days_taken",
      "total_number_of_days_lost",
      "percentage_taking_absence"
    )
  )

  # --- Workforce size ---
  workforce <- download_workforce_dataset(
    "workforce_size",
    WORKFORCE_URLS$workforce_size,
    select_cols = c(
      "fte_workforce",
      "fte_all_teachers",
      "fte_classroom_teachers",
      "fte_leadership_teachers",
      "fte_teaching_assistants"
    )
  )

  # --- Pupil ratios ---
  ratios <- download_workforce_dataset(
    "pupil_ratios",
    WORKFORCE_URLS$pupil_ratios,
    select_cols = c(
      "pupil_to_qual_teacher_ratio",
      "pupil_to_adult_ratio"
    )
  )

  # --- Combine all by school_urn and time_period ---
  message("\nCombining workforce datasets ...")

  combined <- turnover %>%
    full_join(pay, by = c("school_urn", "time_period")) %>%
    full_join(sickness, by = c("school_urn", "time_period")) %>%
    full_join(workforce, by = c("school_urn", "time_period")) %>%
    full_join(ratios, by = c("school_urn", "time_period"))

  # Ensure one row per school-year
  combined <- combined %>%
    distinct(school_urn, time_period, .keep_all = TRUE)

  # Convert numeric columns
  numeric_cols <- setdiff(names(combined), c("school_urn", "time_period"))
  combined <- combined %>%
    mutate(across(all_of(numeric_cols), ~ suppressWarnings(as.numeric(as.character(.)))))

  message("Combined workforce: ", nrow(combined), " school-year rows, ",
          ncol(combined), " variables")
  message("Time periods: ", paste(sort(unique(combined$time_period)), collapse = ", "))

  saveRDS(combined, combined_cache)
  combined
}


#' Join workforce data to the panel
join_workforce_to_panel <- function(panel, workforce) {

  message("\nJoining workforce data to panel ...")

  panel_with_wf <- panel %>%
    left_join(
      workforce,
      by = c("URN" = "school_urn", "time_period" = "time_period")
    )

  # Report match rates
  wf_vars <- c("remained_in_the_same_school", "teachers_on_leadership_pay_range_percent",
                "average_number_of_days_taken")

  for (v in wf_vars) {
    if (v %in% names(panel_with_wf)) {
      n_available <- sum(!is.na(panel_with_wf[[v]]))
      pct <- round(n_available / nrow(panel_with_wf) * 100, 1)
      message("  ", v, ": ", n_available, "/", nrow(panel_with_wf), " (", pct, "%)")
    } else {
      message("  ", v, ": NOT IN DATASET")
    }
  }

  panel_with_wf
}


# ---- Main execution ----

if (sys.nframe() == 0) {

  # Load the raw panel from step 02
  panel_path <- here::here("data", "panel_raw.rds")
  if (!file.exists(panel_path)) {
    stop("panel_raw.rds not found. Run 02_build_panel.R first.")
  }

  panel <- readRDS(panel_path)

  # Download and combine workforce data
  workforce <- load_all_workforce()

  # Join to panel
  panel <- join_workforce_to_panel(panel, workforce)

  # Save
  saveRDS(panel, here::here("data", "panel_with_workforce.rds"))
  message("\nSaved panel_with_workforce.rds (", nrow(panel), " rows, ", ncol(panel), " cols)")
}
