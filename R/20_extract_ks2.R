# 20_extract_ks2.R - Build the primary-phase (KS2) school panel
# -------------------------------------------------------------------------------
# Primary-phase analogue of 01_extract_data.R + 02_build_panel.R, using the
# automated CSCP downloader (00_download_cscp.R) instead of manual zips.
#
# Pipeline position: first script of the KS2 (primary) phase, numbered 20-29.
# Secondary (KS4) remains scripts 01-08; post-16 (KS5) will take 30-39.
# Outputs live in data/ks2/ to keep phases separated:
#   data/ks2/ks2_year_data_list.rds  - per-year joined dataframes (all columns)
#   data/ks2/ks2_panel.rds           - harmonised panel, selected variables,
#                                      workforce joined (analogue of panel_data.rds)
#
# Coverage note: KS2 school-level results exist for 2022-23, 2023-24 and 2024-25
# only. 2019-20 and 2020-21 assessments were cancelled (COVID) and the 2021-22
# results were not published at school level. year_numeric is aligned with the
# secondary panel (2021-22 = 0), so KS2 years run 1..3.
#
# Run from project root:  Rscript R/20_extract_ks2.R
# or interactively:       source("R/20_extract_ks2.R"); ks2_panel <- build_ks2_panel()
# -------------------------------------------------------------------------------

source(here::here("R", "helpers.R"))
source(here::here("R", "00_download_cscp.R"))
source(here::here("R", "10_gias_closures.R"))  # for add_closure_flags()

KS2_YEAR_CONFIG <- tibble::tribble(
  ~academic_year, ~year_label, ~year_numeric, ~time_period,
  "2022-2023",    "2022-23",   1L,            202223L,
  "2023-2024",    "2023-24",   2L,            202324L,
  "2024-2025",    "2024-25",   3L,            202425L
)

ks2_dir <- here::here("data", "ks2")
dir.create(ks2_dir, showWarnings = FALSE, recursive = TRUE)


# ---- Variables kept in the KS2 panel ----
# Outcome naming follows the KS4 panel conventions: _FSM6CLA1A = disadvantaged
# pupils, _NFSM6CLA1A = non-disadvantaged. Selection uses any_of() throughout:
# columns vary slightly across years (e.g. KS1-baseline progress measures are
# being retired) and missing ones are simply dropped for that year.

ks2_outcomes <- c(
  "PTRWM_EXP", "PTRWM_HIGH",                       # % expected / higher standard (RWM)
  "READ_AVERAGE", "MAT_AVERAGE", "GPS_AVERAGE",    # average scaled scores
  "READPROG", "WRITPROG", "MATPROG"                # KS1-baseline progress (retiring)
)

# Disadvantaged / non-disadvantaged / gender splits of the outcomes
ks2_outcome_splits <- as.vector(outer(
  ks2_outcomes,
  c("_FSM6CLA1A", "_NFSM6CLA1A", "_B", "_G"),
  paste0
))

KS2_PANEL_VARS <- c(
  # Identifiers
  "URN", "SCHNAME", "LEA", "LANAME", "ESTAB", "TOWN", "PCODE",
  "academic_year", "year_label", "year_numeric", "time_period",
  # Outcomes
  ks2_outcomes, ks2_outcome_splits,
  # Cohort / composition (KS2 file)
  "TELIG", "TOTPUPS", "PTFSM6CLA1A", "PTNOTFSM6CLA1A",
  "PTEALGRP2", "PTMOBN",
  # Census (composition detail)
  "PNUMEAL", "PNUMENGFL", "PNUMFSM", "PNUMFSMEVER", "NORB", "NORG", "TPUP",
  # Absence
  "PERCTOT", "PPERSABS10",
  # School characteristics (KS2 file + school information)
  "NFTYPE", "RELDENOM", "AGERANGE",
  "OFSTEDRATING", "OFSTEDLASTINSP", "SCHOOLTYPE", "MINORGROUP",
  "RELCHAR", "ADMPOL", "GENDER", "ISPRIMARY"
)


#' Resolve .x/.y duplicate columns after joins (prefers the KS2-file version)
resolve_join_duplicates <- function(df) {
  for (base_name in c("SCHNAME", "SCHOOLTYPE", "LEA", "ESTAB", "LA", "TOWN", "PCODE")) {
    x_name <- paste0(base_name, ".x")
    y_name <- paste0(base_name, ".y")
    if (x_name %in% names(df) && y_name %in% names(df)) {
      df <- df %>%
        mutate(
          across(all_of(c(x_name, y_name)), as.character),
          !!base_name := coalesce(!!sym(x_name), !!sym(y_name))
        ) %>%
        select(-all_of(c(x_name, y_name)))
    }
  }
  df
}


#' Load and join one year of KS2 + census + absence + school information
load_ks2_year <- function(year) {

  year_dir <- file.path(CSCP_EXTRACTED_DIR, year)
  config <- KS2_YEAR_CONFIG %>% filter(academic_year == year)
  if (nrow(config) == 0) stop("Unknown KS2 year: ", year)

  message("Loading KS2 ", year, " ...")

  # --- KS2 results ---
  ks2_path <- file.path(year_dir, "england_ks2final.csv")
  if (!file.exists(ks2_path)) stop("Missing ", ks2_path, " - run cscp_update() first")

  ks2 <- read_dfe_csv(ks2_path) %>%
    {
      # Coerce everything from TOTPUPS onwards to numeric (mirrors KS4 logic)
      totpups_idx <- which(names(.) == "TOTPUPS")
      if (length(totpups_idx) > 0) {
        mutate(., across(
          all_of(names(.)[totpups_idx:ncol(.)]),
          ~ suppressWarnings(readr::parse_number(as.character(.)))
        ))
      } else .
    }
  message("  KS2: ", nrow(ks2), " rows, ", ncol(ks2), " cols")

  # --- Mainstream schools only (RECTYPE 1; 2 = special, 3 = PRU/other) ---
  if ("RECTYPE" %in% names(ks2)) {
    ks2 <- ks2 %>% filter(RECTYPE == 1)
    message("  After RECTYPE==1 filter: ", nrow(ks2), " rows")
  }

  # --- Census ---
  census_path <- file.path(year_dir, "england_census.csv")
  census <- NULL
  if (file.exists(census_path)) {
    census <- read_dfe_csv(census_path) %>%
      mutate(across(5:ncol(.), ~ suppressWarnings(readr::parse_number(as.character(.)))))
    message("  Census: ", nrow(census), " rows")
  } else warning("  Census file not found for ", year)

  # --- Absence ---
  abs_path <- file.path(year_dir, "england_abs.csv")
  abs_data <- NULL
  if (file.exists(abs_path)) {
    abs_data <- read_dfe_csv(abs_path) %>%
      mutate(across(any_of(c("PERCTOT", "PPERSABS10")),
                    ~ suppressWarnings(as.numeric(as.character(.)))))
    message("  Absence: ", nrow(abs_data), " rows")
  } else warning("  Absence file not found for ", year)

  # --- School information ---
  schinfo_path <- file.path(year_dir, "england_school_information.csv")
  schinfo <- NULL
  if (file.exists(schinfo_path)) {
    schinfo <- read_dfe_csv(schinfo_path)
    if ("OFSTEDLASTINSP" %in% names(schinfo)) {
      schinfo <- schinfo %>%
        mutate(OFSTEDLASTINSP = suppressWarnings(lubridate::dmy(as.character(OFSTEDLASTINSP))))
    }
    message("  School info: ", nrow(schinfo), " rows, ",
            ifelse("OFSTEDRATING" %in% names(schinfo), "HAS", "NO"), " OFSTEDRATING")
  } else warning("  School information file not found for ", year)

  # --- Join by URN ---
  joined <- ks2
  if (!is.null(abs_data)) joined <- joined %>% left_join(abs_data, by = "URN")
  if (!is.null(census))   joined <- joined %>% left_join(census,   by = "URN")
  if (!is.null(schinfo))  joined <- joined %>% left_join(schinfo,  by = "URN")

  joined <- joined %>%
    resolve_join_duplicates() %>%
    mutate(
      academic_year = config$academic_year,
      year_label    = config$year_label,
      year_numeric  = config$year_numeric,
      time_period   = config$time_period
    )

  message("  Final: ", nrow(joined), " rows, ", ncol(joined), " cols\n")
  joined
}


#' Build the harmonised KS2 panel: bind years, select variables, join workforce
build_ks2_panel <- function(save = TRUE) {

  # Ensure raw files are present (no-op if already downloaded)
  cscp_update(CSCP_PHASE_CONFIG["ks2_primary"])

  year_data <- map(KS2_YEAR_CONFIG$academic_year, load_ks2_year) %>%
    set_names(KS2_YEAR_CONFIG$academic_year)

  if (save) {
    saveRDS(year_data, file.path(ks2_dir, "ks2_year_data_list.rds"))
    message("Saved ks2_year_data_list.rds")
  }

  # Harmonise: keep panel variables present in each year, then bind
  panel <- year_data %>%
    map(~ select(.x, any_of(KS2_PANEL_VARS))) %>%
    bind_rows()

  # LA name lookup (shared with the secondary pipeline) if LANAME not in source
  la_lookup_path <- here::here("data", "la_lookup.rds")
  if (!"LANAME" %in% names(panel) && file.exists(la_lookup_path)) {
    la_lookup <- readRDS(la_lookup_path)
    if (all(c("LEA", "LANAME") %in% names(la_lookup))) {
      panel <- panel %>%
        mutate(LEA = as.character(LEA)) %>%
        left_join(
          la_lookup %>% select(LEA, LANAME) %>% mutate(LEA = as.character(LEA)) %>% distinct(),
          by = "LEA"
        )
      message("LANAME joined from la_lookup.rds")
    }
  }

  # Workforce join (same cached EES datasets as the secondary panel; the cache
  # is national school-level so primary rows are already in it)
  workforce_path <- here::here("data", "cache", "workforce_combined.rds")
  if (file.exists(workforce_path)) {
    workforce <- readRDS(workforce_path)
    panel <- panel %>%
      left_join(workforce, by = c("URN" = "school_urn", "time_period" = "time_period"))
    n_wf <- sum(!is.na(panel$remained_in_the_same_school))
    message("Workforce joined: ", n_wf, " of ", nrow(panel), " rows matched (",
            round(100 * n_wf / nrow(panel)), "%)")
  } else {
    message("Workforce cache not found (run 03_add_workforce.R) - skipping join")
  }

  # Ofsted ratings: the school_information file only carries OFSTEDRATING up to
  # 2022-23 (single-word judgements ended Nov 2024). Join the same national
  # Ofsted MI snapshot the secondary pipeline uses (URN -> numeric 1-4, as at
  # 31 Aug 2024, cached by 02_build_panel.R) - it covers primaries too.
  ofsted_path <- here::here("data", "cache", "ofsted_ratings.rds")
  if (file.exists(ofsted_path)) {
    ofsted <- readRDS(ofsted_path)
    panel <- panel %>% left_join(ofsted, by = "URN")
    n_of <- sum(!is.na(panel$OFSTEDRATING_external))
    message("Ofsted MI snapshot joined: ", n_of, " of ", nrow(panel), " rows (",
            round(100 * n_of / nrow(panel)), "%)")
  } else {
    message("Ofsted cache not found (run 02_build_panel.R) - skipping join")
  }

  # GIAS closure flags: has the school since closed, when, why, successor URN
  # (no-op with a message if data/gias/school_closures.rds is absent)
  panel <- add_closure_flags(panel)

  if (save) {
    saveRDS(panel, file.path(ks2_dir, "ks2_panel.rds"))
    message("Saved ks2_panel.rds (", nrow(panel), " rows, ", ncol(panel), " cols)")
  }

  panel
}


#' Quick per-year coverage summary
summarise_ks2_panel <- function(panel) {
  panel %>%
    group_by(year_label) %>%
    summarise(
      schools          = n(),
      mean_rwm_exp     = round(mean(PTRWM_EXP, na.rm = TRUE), 1),
      mean_read_scaled = round(mean(READ_AVERAGE, na.rm = TRUE), 1),
      mean_mat_scaled  = round(mean(MAT_AVERAGE, na.rm = TRUE), 1),
      pct_with_absence = round(100 * mean(!is.na(PERCTOT))),
      pct_with_fsm     = round(100 * mean(!is.na(PTFSM6CLA1A))),
      pct_with_ofsted  = if ("OFSTEDRATING_external" %in% names(panel))
                           round(100 * mean(!is.na(OFSTEDRATING_external))) else NA_real_,
      pct_with_workforce = if ("remained_in_the_same_school" %in% names(panel))
                             round(100 * mean(!is.na(remained_in_the_same_school))) else NA_real_,
      .groups = "drop"
    )
}


# ---- Main execution (Rscript only) ----
if (sys.nframe() == 0) {
  panel <- build_ks2_panel()
  message("\n=== KS2 panel summary ===")
  print(summarise_ks2_panel(panel), width = Inf)
}
