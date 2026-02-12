# 04_compute_derived.R - Compute derived variables: Gorard index, log transforms
# -------------------------------------------------------------------------------
# This script:
#   1. Computes the Gorard segregation index per LA per year
#   2. Creates log-transformed versions of skewed variables
#   3. Creates the school lookup table (URN -> name, LA, region, coords)
#   4. Saves the final panel_data.rds and school_lookup.rds
#
# Depends on: R/03_add_workforce.R (produces panel_with_workforce.rds)
# Run from project root: source("R/04_compute_derived.R")
# -------------------------------------------------------------------------------

source(here::here("R", "helpers.R"))


#' Compute Gorard Segregation Index for each LA in each year
#'
#' Formula: G_s = 0.5 * sum_i |F_i/F_total - T_i/T_total|
#' where F_i = FSM-eligible pupils in school i
#'       F_total = total FSM-eligible pupils in the LA
#'       T_i = total pupils in school i
#'       T_total = total pupils in the LA
#'
#' Source: Gorard & Taylor (2002), replicating the approach in week8_lecture.qmd
#'
#' @param panel Dataframe with TFSM6CLA1A (total FSM pupils), TPUP (total pupils),
#'              LANAME, and academic_year columns
#' @return Dataframe with LANAME, academic_year, gorard_segregation columns
compute_gorard_index <- function(panel) {

  message("Computing Gorard segregation index per LA per year ...")

  # Need: school-level FSM counts and total pupils
  # TFSM6CLA1A = total number of disadvantaged pupils in the school

  # TPUP = total pupils in the school

  gorard <- panel %>%
    filter(!is.na(TFSM6CLA1A), !is.na(TPUP), TPUP > 0) %>%
    group_by(LANAME, academic_year) %>%
    mutate(
      F_total = sum(TFSM6CLA1A, na.rm = TRUE),
      T_total = sum(TPUP, na.rm = TRUE)
    ) %>%
    filter(F_total > 0, T_total > 0) %>%
    summarise(
      gorard_segregation = 0.5 * sum(
        abs(TFSM6CLA1A / F_total - TPUP / T_total),
        na.rm = TRUE
      ),
      n_schools = n(),
      .groups = "drop"
    )

  message("  Gorard index computed for ", n_distinct(gorard$LANAME), " LAs across ",
          n_distinct(gorard$academic_year), " years")
  message("  Index range: ", round(min(gorard$gorard_segregation), 3), " - ",
          round(max(gorard$gorard_segregation), 3))

  gorard %>% select(LANAME, academic_year, gorard_segregation)
}


#' Add log transforms for model variables
#'
#' Guards against zero/negative values by setting to NA before log transform.
#' Uses natural log (base e) consistent with lme11 model specification.
add_log_transforms <- function(panel) {

  message("Adding log transforms ...")

  # Variables that need log transformation in the model
  log_vars <- c(
    "ATT8SCR", "ATT8SCR_FSM6CLA1A", "ATT8SCR_NFSM6CLA1A",
    "PTFSM6CLA1A", "PERCTOT", "PNUMEAL",
    "remained_in_the_same_school", "average_number_of_days_taken"
  )

  for (v in log_vars) {
    if (v %in% names(panel)) {
      new_name <- paste0("log_", v)
      panel[[new_name]] <- safe_log(panel[[v]])
      n_valid <- sum(!is.na(panel[[new_name]]))
      n_total <- nrow(panel)
      message("  log_", v, ": ", n_valid, "/", n_total, " valid")
    } else {
      message("  WARNING: ", v, " not found in panel - skipping log transform")
    }
  }

  panel
}


#' Create a lookup table for schools
create_school_lookup <- function(panel) {

  message("Creating school lookup table ...")

  lookup <- panel %>%
    group_by(URN) %>%
    summarise(
      SCHNAME = first(na.omit(SCHNAME)),
      LANAME = first(na.omit(LANAME)),
      LEA = first(na.omit(LEA)),
      gor_name = first(na.omit(gor_name)),
      TOWN = first(na.omit(TOWN)),
      POSTCODE = first(na.omit(POSTCODE)),
      easting = first(na.omit(easting)),
      northing = first(na.omit(northing)),
      OFSTEDRATING = first(na.omit(OFSTEDRATING)),
      OFSTEDRATING_1 = first(na.omit(OFSTEDRATING_1)),
      MINORGROUP = first(na.omit(MINORGROUP)),
      n_years_present = n(),
      years_present = paste(sort(unique(year_label)), collapse = ", "),
      .groups = "drop"
    )

  message("  Lookup: ", nrow(lookup), " unique schools")
  message("  With coordinates: ", sum(!is.na(lookup$easting)))
  message("  With region: ", sum(!is.na(lookup$gor_name)))

  lookup
}


#' Create a lookup table for LAs
create_la_lookup <- function(panel) {

  message("Creating LA lookup table ...")

  la_lookup <- panel %>%
    group_by(LANAME) %>%
    summarise(
      LEA = first(na.omit(LEA)),
      gor_name = first(na.omit(gor_name)),
      n_schools = n_distinct(URN),
      .groups = "drop"
    ) %>%
    filter(!is.na(LANAME))

  message("  LA lookup: ", nrow(la_lookup), " local authorities")
  la_lookup
}


#' Final cleaning and validation of the panel
clean_panel <- function(panel) {

  message("\nFinal panel cleaning ...")

  # Ensure OFSTEDRATING is a factor
  if ("OFSTEDRATING" %in% names(panel)) {
    panel <- panel %>%
      mutate(OFSTEDRATING = factor(OFSTEDRATING))
  }

  # Ensure OFSTEDRATING_1 retains its ordered factor levels
  if ("OFSTEDRATING_1" %in% names(panel)) {
    panel <- panel %>%
      mutate(OFSTEDRATING_1 = factor(OFSTEDRATING_1,
                                     levels = c("Outstanding", "Good",
                                                "Requires Improvement", "Inadequate"),
                                     ordered = TRUE))
  }

  # Ensure gor_name is a factor
  if ("gor_name" %in% names(panel)) {
    panel <- panel %>%
      mutate(gor_name = factor(gor_name))
  }

  # Ensure LANAME is a factor
  if ("LANAME" %in% names(panel)) {
    panel <- panel %>%
      mutate(LANAME = factor(LANAME))
  }

  # Filter out rows with missing outcome (ATT8SCR) - can't model these
  n_before <- nrow(panel)
  panel <- panel %>% filter(!is.na(ATT8SCR), ATT8SCR > 0)
  n_after <- nrow(panel)
  message("  Dropped ", n_before - n_after, " rows with missing/zero ATT8SCR")

  # Filter out rows with missing key predictors for log transform
  panel <- panel %>%
    filter(PTFSM6CLA1A > 0 | is.na(PTFSM6CLA1A))

  message("  Final panel: ", nrow(panel), " school-year observations")

  panel
}


# ---- Main execution ----

if (sys.nframe() == 0) {

  # Load the panel with workforce data
  panel_path <- here::here("data", "panel_with_workforce.rds")
  if (!file.exists(panel_path)) {
    stop("panel_with_workforce.rds not found. Run 03_add_workforce.R first.")
  }

  panel <- readRDS(panel_path)
  message("Loaded panel: ", nrow(panel), " rows, ", ncol(panel), " cols\n")

  # 1. Compute and join Gorard segregation index
  gorard <- compute_gorard_index(panel)
  panel <- panel %>%
    left_join(gorard, by = c("LANAME", "academic_year"))

  # 2. Add log transforms
  panel <- add_log_transforms(panel)

  # 3. Clean panel
  panel <- clean_panel(panel)

  # 4. Create lookups
  school_lookup <- create_school_lookup(panel)
  la_lookup <- create_la_lookup(panel)

  # 5. Save outputs
  saveRDS(panel, here::here("data", "panel_data.rds"))
  saveRDS(school_lookup, here::here("data", "school_lookup.rds"))
  saveRDS(la_lookup, here::here("data", "la_lookup.rds"))

  message("\n=== Final Panel Summary ===")
  message("panel_data.rds: ", nrow(panel), " rows, ", ncol(panel), " cols")
  message("school_lookup.rds: ", nrow(school_lookup), " schools")
  message("la_lookup.rds: ", nrow(la_lookup), " local authorities")

  message("\nPer-year breakdown:")
  panel %>%
    group_by(year_label) %>%
    summarise(
      n = n(),
      mean_ATT8 = round(mean(ATT8SCR, na.rm = TRUE), 1),
      pct_absence = round(mean(!is.na(PERCTOT)) * 100, 1),
      pct_ofsted = round(mean(!is.na(OFSTEDRATING)) * 100, 1),
      pct_ofsted_1 = round(mean(!is.na(OFSTEDRATING_1)) * 100, 1),
      pct_workforce = round(mean(!is.na(remained_in_the_same_school)) * 100, 1),
      pct_gorard = round(mean(!is.na(gorard_segregation)) * 100, 1),
      .groups = "drop"
    ) %>%
    print()
}
