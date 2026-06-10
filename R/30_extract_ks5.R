# 30_extract_ks5.R - Build the post-16 (KS5 / 16-18) provider panel
# -------------------------------------------------------------------------------
# Post-16 analogue of 01_extract_data.R + 02_build_panel.R, using the automated
# CSCP downloader (00_download_cscp.R). Phase numbering: KS4 = 01-08, cross-phase
# GIAS = 10-19, KS2 = 20-29, KS5 = 30-39.
#
# Outputs (data/ks5/):
#   ks5_year_data_list.rds - per-year joined dataframes (all columns)
#   ks5_panel.rds          - harmonised panel: attainment by qualification route,
#                            retention, English/maths resit progress, destinations,
#                            L3VA (2023-24+), provider classification, Ofsted,
#                            workforce (school-based providers only)
#
# Post-16 specifics vs the KS2/KS4 panels:
#   * Providers, not schools: school sixth forms, 16-19 academies, sixth-form
#     colleges, FE colleges, UTCs/studio schools and independents all report.
#     provider_group classifies them; census/absence/workforce joins only cover
#     school-based providers (colleges sit outside the school census).
#   * Outcomes are route-specific (A level / academic / applied general / T level)
#     - there is deliberately no single headline score.
#   * Disadvantage = _DIS suffix (disadvantaged at end of KS4, carried forward).
#   * L3VA (vaqual) exists only from 2023-24; destinations files lag the cohort.
#
# Run from project root:  Rscript R/30_extract_ks5.R
# -------------------------------------------------------------------------------

source(here::here("R", "helpers.R"))
source(here::here("R", "00_download_cscp.R"))

KS5_YEAR_CONFIG <- tibble::tribble(
  ~academic_year, ~year_label, ~year_numeric, ~time_period,
  "2021-2022",    "2021-22",   0L,            202122L,
  "2022-2023",    "2022-23",   1L,            202223L,
  "2023-2024",    "2023-24",   2L,            202324L,
  "2024-2025",    "2024-25",   3L,            202425L
)

ks5_dir <- here::here("data", "ks5")
dir.create(ks5_dir, showWarnings = FALSE, recursive = TRUE)


# ---- Panel variables ----

KS5_PANEL_VARS <- c(
  # Identifiers
  "URN", "SCHNAME", "LEA", "LANAME", "ESTAB", "TOWN", "PCODE",
  "academic_year", "year_label", "year_numeric", "time_period",
  # Provider characteristics
  "NFTYPE", "provider_group", "RELDENOM", "ADMPOL", "GEND1618", "AGERANGE",
  # Cohort sizes (all students / disadvantaged)
  "TPUP1618", "TALLPUP_1618", "TALLPUP_1618_DIS",
  "TALLPUP_ALEV_1618", "TALLPUP_ALEV_1618_DIS",
  "TALLPUP_ACAD_1618", "TALLPUP_ACAD_1618_DIS",
  "TALLPUP_AGEN", "TALLPUP_AGEN_DIS",
  "TALLPUP_TLEV", "TALLPUP_TLEV_DIS",
  # Attainment: average points per entry by route (+ grade equivalents)
  "TALLPPE_ALEV_1618", "TALLPPE_ALEV_1618_DIS", "TALLPPEGRD_ALEV_1618",
  "TALLPPE_ACAD_1618", "TALLPPE_ACAD_1618_DIS", "TALLPPEGRD_ACAD_1618",
  "TALLPPE_AGEN", "TALLPPE_AGEN_DIS", "TALLPPEGRD_AGEN",
  "TALLPPE_TLEV", "TALLPPE_TLEV_DIS", "TALLPPEGRD_TLEV",
  "TALLPPE_TechCert",
  # Top-end measures
  "TB3PTSE", "TB3PTSE_GRD", "PTAAB_2FAC",
  # English/maths resit progress (students without grade 4 at KS4)
  "PROGEX_E", "PROGEX_M", "ENTRY_PER_E", "ENTRY_PER_M",
  # Retention by route
  "PT_Retained_ALEV_Ret", "PT_Retained_ACAD_Ret",
  "PT_Retained_AGEN_Ret", "PT_Retained_TLEV_Ret",
  # L3 value added by route (in the KS5 file from 2023-24; all/disadvantaged/not)
  "VA_INS_ALEV", "VA_INS_ACAD", "VA_INS_AGEN", "VA_INS_TLEV", "VA_INS_TECHCERT",
  "VA_INS_ALEV_DIS", "VA_INS_ACAD_DIS", "VA_INS_AGEN_DIS", "VA_INS_TLEV_DIS",
  "VA_INS_ALEV_NOTDIS", "VA_INS_ACAD_NOTDIS", "VA_INS_AGEN_NOTDIS", "VA_INS_TLEV_NOTDIS"
)

# Destinations (england_ks5-studest.csv), renamed with dest_ prefix
KS5_DEST_VARS <- c(
  "TOT_COHORT", "TOT_OVERALLPER", "TOT_EDUCATIONPER", "TOT_FEPER", "TOT_HEPER",
  "TOT_APPRENPER", "TOT_EMPLOYMENTPER", "TOT_NOT_SUSTAINEDPER",
  "TOT_COHORT_DIS", "TOT_OVERALLPER_DIS", "TOT_EDUCATIONPER_DIS", "TOT_FEPER_DIS",
  "TOT_HEPER_DIS", "TOT_APPRENPER_DIS", "TOT_EMPLOYMENTPER_DIS",
  "TOT_NOT_SUSTAINEDPER_DIS"
)


#' Classify NFTYPE/FESITYPE codes into provider groups
classify_provider <- function(nftype) {
  nftype_clean <- str_trim(as.character(nftype))
  case_when(
    nftype_clean %in% c("ACC1619", "AC1619", "F1619")             ~ "16-19 academy/free school",
    nftype_clean %in% c("UTC", "SS", "CTC")                       ~ "UTC/studio/CTC",
    nftype_clean %in% c("ACC", "AC", "F", "FD")                   ~ "School sixth form (academy/free)",
    nftype_clean %in% c("CY", "CYS", "VA", "VC", "CM")            ~ "School sixth form (LA maintained)",
    nftype_clean == "Sixth Form College"                          ~ "Sixth form college",
    grepl("College", nftype_clean, ignore.case = TRUE)            ~ "FE college",
    nftype_clean == "IND"                                         ~ "Independent",
    TRUE                                                          ~ "Other/unknown"
  )
}


#' Load and join one year of KS5 data
load_ks5_year <- function(year) {

  year_dir <- file.path(CSCP_EXTRACTED_DIR, year)
  config <- KS5_YEAR_CONFIG %>% filter(academic_year == year)
  if (nrow(config) == 0) stop("Unknown KS5 year: ", year)

  message("Loading KS5 ", year, " ...")

  # --- KS5 results ---
  ks5_path <- file.path(year_dir, "england_ks5final.csv")
  if (!file.exists(ks5_path)) stop("Missing ", ks5_path, " - run cscp_update() first")

  ks5 <- read_dfe_csv(ks5_path) %>%
    rename(NFTYPE = any_of(c("NFTYPE/FESITYPE", "NFTYPE"))) %>%
    {
      # Numeric coercion from TPUP1618 onwards, sparing grade columns
      # (TALLPPEGRD_* hold letter grades like "B-") and other text fields
      start_idx <- which(names(.) == "TPUP1618")
      if (length(start_idx) > 0) {
        numeric_cols <- names(.)[start_idx:ncol(.)]
        numeric_cols <- numeric_cols[!grepl("GRD|_GRD|DESCR", numeric_cols)]
        mutate(., across(
          all_of(numeric_cols),
          ~ suppressWarnings(readr::parse_number(as.character(.)))
        ))
      } else .
    } %>%
    mutate(provider_group = classify_provider(NFTYPE))

  message("  KS5: ", nrow(ks5), " rows, ", ncol(ks5), " cols")

  # --- Provider rows only (RECTYPE 1; 4 = LA aggregates, 5/7 = national) ---
  if ("RECTYPE" %in% names(ks5)) {
    ks5 <- ks5 %>% filter(RECTYPE == 1)
    message("  After RECTYPE==1 filter: ", nrow(ks5), " rows")
  }

  # --- Destinations (lagging publication; file may be absent for latest year) ---
  dest_path <- file.path(year_dir, "england_ks5-studest.csv")
  if (file.exists(dest_path)) {
    dest <- read_dfe_csv(dest_path) %>%
      filter(if ("RECTYPE" %in% names(.)) RECTYPE == 1 else TRUE) %>%
      select(URN, any_of(KS5_DEST_VARS)) %>%
      mutate(across(-URN, ~ suppressWarnings(readr::parse_number(as.character(.))))) %>%
      rename_with(~ paste0("dest_", .), -URN)
    ks5 <- ks5 %>% left_join(dest, by = "URN")
    message("  Destinations: ", sum(!is.na(ks5$dest_TOT_OVERALLPER)), " providers matched")
  } else {
    message("  Destinations: file not present for ", year)
  }

  # --- L3 value added ---
  # Route-level VA (VA_INS_ALEV etc., incl. _DIS splits) sits in the KS5 file
  # itself from 2023-24. The separate vaqual/vasubj downloads hold finer
  # qualification- and subject-level VA for later analysis; not joined here.
  if ("VA_INS_ALEV" %in% names(ks5)) {
    message("  L3VA (in-file): ", sum(!is.na(ks5$VA_INS_ALEV)),
            " providers with A level VA")
  } else {
    message("  L3VA: not published for ", year)
  }

  # --- School information (for school-based providers: Ofsted text, type detail) ---
  schinfo_path <- file.path(year_dir, "england_school_information.csv")
  if (file.exists(schinfo_path)) {
    schinfo <- read_dfe_csv(schinfo_path) %>%
      select(URN, any_of(c("LANAME", "MINORGROUP", "OFSTEDRATING", "SCHOOLTYPE")))
    ks5 <- ks5 %>% left_join(schinfo, by = "URN")
  }

  ks5 %>%
    mutate(
      academic_year = config$academic_year,
      year_label    = config$year_label,
      year_numeric  = config$year_numeric,
      time_period   = config$time_period
    )
}


#' Build the harmonised KS5 panel
build_ks5_panel <- function(save = TRUE) {

  cscp_update(CSCP_PHASE_CONFIG[c("ks5_post16", "ks5_value_added")])

  year_data <- map(KS5_YEAR_CONFIG$academic_year, load_ks5_year) %>%
    set_names(KS5_YEAR_CONFIG$academic_year)

  if (save) {
    saveRDS(year_data, file.path(ks5_dir, "ks5_year_data_list.rds"))
    message("Saved ks5_year_data_list.rds")
  }

  panel <- year_data %>%
    map(~ select(.x, any_of(KS5_PANEL_VARS), starts_with("dest_"), starts_with("va_"))) %>%
    bind_rows()

  # Ofsted MI snapshot (numeric 1-4, as at Aug 2024; school-based providers
  # mainly - FE colleges are inspected under a different framework)
  ofsted_path <- here::here("data", "cache", "ofsted_ratings.rds")
  if (file.exists(ofsted_path)) {
    panel <- panel %>% left_join(readRDS(ofsted_path), by = "URN")
    n_of <- sum(!is.na(panel$OFSTEDRATING_external))
    message("Ofsted MI snapshot joined: ", n_of, " of ", nrow(panel), " rows (",
            round(100 * n_of / nrow(panel)), "%)")
  }

  # Workforce (school census based - colleges will not match; whole-school
  # figures for school sixth forms, so interpret with care)
  workforce_path <- here::here("data", "cache", "workforce_combined.rds")
  if (file.exists(workforce_path)) {
    panel <- panel %>%
      left_join(readRDS(workforce_path),
                by = c("URN" = "school_urn", "time_period" = "time_period"))
    n_wf <- sum(!is.na(panel$remained_in_the_same_school))
    message("Workforce joined: ", n_wf, " of ", nrow(panel), " rows (",
            round(100 * n_wf / nrow(panel)), "%)")
  }

  if (save) {
    saveRDS(panel, file.path(ks5_dir, "ks5_panel.rds"))
    message("Saved ks5_panel.rds (", nrow(panel), " rows, ", ncol(panel), " cols)")
  }

  panel
}


#' Per-year, per-provider-group coverage summary
summarise_ks5_panel <- function(panel) {
  panel %>%
    group_by(year_label, provider_group) %>%
    summarise(
      providers     = n(),
      mean_alev_ppe = round(mean(TALLPPE_ALEV_1618, na.rm = TRUE), 1),
      pct_with_alev = round(100 * mean(!is.na(TALLPPE_ALEV_1618))),
      pct_with_dest = round(100 * mean(!is.na(dest_TOT_OVERALLPER))),
      pct_with_va   = if ("VA_INS_ALEV" %in% names(panel))
                        round(100 * mean(!is.na(VA_INS_ALEV))) else NA_real_,
      .groups = "drop"
    ) %>%
    arrange(year_label, desc(providers))
}


# ---- Main execution (Rscript only) ----
if (sys.nframe() == 0) {
  panel <- build_ks5_panel()
  message("\n=== KS5 panel summary ===")
  print(summarise_ks5_panel(panel), n = 50, width = Inf)
}
