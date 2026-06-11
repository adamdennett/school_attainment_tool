# 14_early_years_baselines.R - LA-level early-years entry baselines for KS2 cohorts
# -------------------------------------------------------------------------------
# Builds the "early-years entry conditions" block identified in the inventory's
# KS1 section: for each KS2 panel cohort, the LA-level early-attainment measures
# of that SAME cohort earlier in school. No school-level KS1/phonics/MTC/EYFSP
# data is published, so this is an LA pseudo-cohort resource (typology, LA-level
# models), not a school-level baseline.
#
# Cohort offsets relative to the KS2 year (Year 6):
#   EYFSP    reception  KS2 year - 6   (current EES data starts 2021-22, new
#                                       framework -> populates KS2 2027-28+)
#   Phonics  Year 1     KS2 year - 5   (2011-12 onwards; 2019-20/2020-21 cancelled)
#   KS1      Year 2     KS2 year - 4   (2015-16 -> 2022-23 only; series now dead)
#   MTC      Year 4     KS2 year - 2   (2021-22 onwards)
#
# Sources (EES data catalogue, cached in data/cache/):
#   ees_ks1_la.csv         KS1 attainment by region & LA (superseded publication)
#   ees_phonics_la.csv     Phonics by region & LA (full 2011-12+ history)
#   ees_mtc_la.csv         MTC regional & LA by pupil characteristics
#   ees_eyfsp_headline.csv EYFSP headline measures by child characteristics
#
# Output: data/early_years/la_entry_baselines.rds - LA x KS2-cohort-year rows
# with eyfsp_gld, phonics_y1_met, ks1_read/writ/mat_exp, mtc_mean_score and the
# source years each came from.
#
# Run from project root:  Rscript R/14_early_years_baselines.R
# -------------------------------------------------------------------------------

source(here::here("R", "helpers.R"))

EES_DATASET_URL <- "https://explore-education-statistics.service.gov.uk/data-catalogue/data-set/%s/csv"

EY_DATASETS <- c(
  ks1_la         = "703a7b93-326e-4c59-9df0-4b0045d174cb",
  phonics_la     = "d4eddf3c-d88e-4fe1-9189-d0f894caf642",
  mtc_la         = "477d683c-dc7e-4508-b47c-29ea75816c1b",
  eyfsp_headline = "70737eaa-9f27-47e1-a896-5a1d05ab242d"
)

cache_dir <- here::here("data", "cache")
ey_dir <- here::here("data", "early_years")
dir.create(ey_dir, showWarnings = FALSE, recursive = TRUE)

ey_csv <- function(name) {
  csv_path <- file.path(cache_dir, paste0("ees_", name, ".csv"))
  download_cached(sprintf(EES_DATASET_URL, EY_DATASETS[[name]]), csv_path)
  csv_path
}

# academic-year arithmetic on EES time_period codes (e.g. 202223 - 4 -> 201819)
shift_period <- function(period, years) {
  start <- period %/% 100 + years
  start * 100 + (start + 1) %% 100
}


# ---- 1. Per-source LA extracts (totals only) ----

load_ks1 <- function() {
  read_csv(ey_csv("ks1_la"), na = NA_CODES, show_col_types = FALSE,
           col_select = c(time_period, geographic_level, la_name,
                          breakdown_topic, breakdown, school_type, gender,
                          pt_readta_met_expected_standard,
                          pt_writta_met_expected_standard,
                          pt_matta_met_expected_standard)) %>%
    filter(geographic_level == "Local authority",
           breakdown_topic == "All pupils", breakdown == "Total",
           school_type == "State-funded schools", gender == "Total") %>%
    transmute(time_period, la_name,
              ks1_read_exp = suppressWarnings(as.numeric(pt_readta_met_expected_standard)),
              ks1_writ_exp = suppressWarnings(as.numeric(pt_writta_met_expected_standard)),
              ks1_mat_exp  = suppressWarnings(as.numeric(pt_matta_met_expected_standard))) %>%
    distinct(time_period, la_name, .keep_all = TRUE)
}

load_phonics <- function() {
  read_csv(ey_csv("phonics_la"), na = NA_CODES, show_col_types = FALSE,
           col_select = c(time_period, geographic_level, la_name,
                          establishment_type_group, sex, year_group,
                          disadvantage_status, ethnicity_major, first_language,
                          fsm_status, sen_provision,
                          expected_standard_pupil_percent)) %>%
    filter(geographic_level == "Local authority",
           establishment_type_group == "All state funded", sex == "Total",
           year_group == "Year 1",
           disadvantage_status == "Total", ethnicity_major == "Total",
           first_language == "Total", fsm_status == "Total",
           sen_provision == "Total") %>%
    transmute(time_period, la_name,
              phonics_y1_met = suppressWarnings(as.numeric(expected_standard_pupil_percent))) %>%
    distinct(time_period, la_name, .keep_all = TRUE)
}

load_mtc <- function() {
  read_csv(ey_csv("mtc_la"), na = NA_CODES, show_col_types = FALSE,
           col_select = c(time_period, geographic_level, la_name,
                          establishment_type_group, sex, disadvantage_status,
                          ethnicity_major, first_language, fsm_status,
                          sen_provision, mtc_score_average)) %>%
    filter(geographic_level == "Local authority",
           establishment_type_group == "All state funded", sex == "Total",
           disadvantage_status == "Total", ethnicity_major == "Total",
           first_language == "Total", fsm_status == "Total",
           sen_provision == "Total") %>%
    transmute(time_period, la_name,
              mtc_mean_score = suppressWarnings(as.numeric(mtc_score_average))) %>%
    distinct(time_period, la_name, .keep_all = TRUE)
}

load_eyfsp <- function() {
  df <- read_csv(ey_csv("eyfsp_headline"), na = NA_CODES, show_col_types = FALSE)
  la_col <- if ("la_name" %in% names(df)) "la_name" else "lad_name"
  df %>%
    filter(geographic_level == "Local authority",
           breakdown_topic == "Total",
           if ("sex" %in% names(.)) sex == "Total" else TRUE) %>%
    transmute(time_period, la_name = .data[[la_col]],
              eyfsp_gld = suppressWarnings(as.numeric(gld_children_percent))) %>%
    distinct(time_period, la_name, .keep_all = TRUE)
}


# ---- 2. Align to KS2 cohort years ----

build_la_entry_baselines <- function(ks2_years = NULL, save = TRUE) {

  if (is.null(ks2_years)) {
    ks2_years <- readRDS(here::here("data", "ks2", "ks2_panel.rds")) %>%
      pull(time_period) %>% unique() %>% sort()
  }

  message("Loading source datasets (large CSVs; first run is slow) ...")
  ks1 <- load_ks1();      message("  KS1: ", nrow(ks1), " LA-years")
  ph  <- load_phonics();  message("  Phonics Y1: ", nrow(ph), " LA-years")
  mtc <- load_mtc();      message("  MTC: ", nrow(mtc), " LA-years")
  eyf <- load_eyfsp();    message("  EYFSP (GLD): ", nrow(eyf), " LA-years")

  las <- bind_rows(ks1["la_name"], ph["la_name"], mtc["la_name"], eyf["la_name"]) %>%
    distinct()

  out <- crossing(ks2_period = ks2_years, las) %>%
    mutate(
      eyfsp_year   = shift_period(ks2_period, -6),
      phonics_year = shift_period(ks2_period, -5),
      ks1_year     = shift_period(ks2_period, -4),
      mtc_year     = shift_period(ks2_period, -2)
    ) %>%
    left_join(eyf, by = c("eyfsp_year" = "time_period", "la_name")) %>%
    left_join(ph,  by = c("phonics_year" = "time_period", "la_name")) %>%
    left_join(ks1, by = c("ks1_year" = "time_period", "la_name")) %>%
    left_join(mtc, by = c("mtc_year" = "time_period", "la_name"))

  if (save) {
    saveRDS(out, file.path(ey_dir, "la_entry_baselines.rds"))
    message("Saved la_entry_baselines.rds (", nrow(out), " LA x cohort rows)")
  }
  out
}

summarise_baselines <- function(out) {
  out %>%
    group_by(`KS2 cohort` = ks2_period) %>%
    summarise(
      LAs = n(),
      `% with EYFSP (R, t-6)` = round(100 * mean(!is.na(eyfsp_gld))),
      `% with phonics (Y1, t-5)` = round(100 * mean(!is.na(phonics_y1_met))),
      `% with KS1 (Y2, t-4)` = round(100 * mean(!is.na(ks1_read_exp))),
      `% with MTC (Y4, t-2)` = round(100 * mean(!is.na(mtc_mean_score))),
      .groups = "drop"
    )
}


# ---- Main execution (Rscript only) ----
if (sys.nframe() == 0) {
  out <- build_la_entry_baselines()
  message("\n=== Baseline availability by KS2 cohort ===")
  print(summarise_baselines(out), width = Inf)

  message("\n=== Quick validity check: do LA early baselines correlate with LA KS2 outcomes? ===")
  ks2_la <- readRDS(here::here("data", "ks2", "ks2_panel.rds")) %>%
    group_by(time_period, la_name = LANAME) %>%
    summarise(read_avg = mean(READ_AVERAGE, na.rm = TRUE), .groups = "drop")
  j <- out %>% inner_join(ks2_la, by = c("ks2_period" = "time_period", "la_name"))
  cat("LA-years matched to KS2 panel:", sum(!is.na(j$read_avg)), "of", nrow(j), "\n")
  for (v in c("phonics_y1_met", "ks1_read_exp", "mtc_mean_score")) {
    cat(sprintf("  cor(LA mean KS2 reading, %s): %.2f\n", v,
                cor(j$read_avg, j[[v]], use = "pairwise.complete.obs")))
  }
}
