# 07_la_typology.R - Local Authority Typology via K-Means Clustering
# -------------------------------------------------------------------
# This script:
#   1. Constructs hundreds of LA-level indicators from school-level panel data
#   2. Applies k-means clustering (with optimal k selection via silhouette/elbow)
#   3. Produces cluster pen portraits
#   4. Saves la_typology.rds and la_cluster_summary.rds for use in the app
#
# Depends on: panel_data.rds (from 04_compute_derived.R)
# Run from project root: source("R/07_la_typology.R")
# -------------------------------------------------------------------

source(here::here("R", "helpers.R"))
library(cluster)      # silhouette
suppressWarnings(suppressPackageStartupMessages(
  try(library(factoextra), silent = TRUE)
))

set.seed(42)

# ============================================================
# 1. LOAD DATA
# ============================================================

panel <- readRDS(here::here("data", "panel_data.rds"))
la_lookup_base <- readRDS(here::here("data", "la_lookup.rds"))

message("Panel loaded: ", nrow(panel), " rows, ", ncol(panel), " cols")
message("Local authorities: ", n_distinct(panel$LANAME))

# Helper: proportion helper for categorical variables
prop_of <- function(x, val) mean(x == val, na.rm = TRUE)

# Ensure optional columns exist (filled with NA if not in panel)
ensure_col <- function(df, col, type = NA_real_) {
  if (!col %in% names(df)) df[[col]] <- type
  df
}
optional_cols <- c(
  "PSENELSE", "PSENELK", "PNUMFSMEVER",
  "ATT8SCR_GIRLS", "ATT8SCR_BOYS",
  "ATT8SCRENG", "ATT8SCRMAT",
  "P8MEA", "P8MEA_FSM6CLA1A", "P8MEA_NFSM6CLA1A",
  "PTPRIORHI", "PPERSABS10",
  "fte_workforce", "fte_all_teachers", "fte_classroom_teachers",
  "fte_leadership_teachers", "fte_teaching_assistants",
  "pupil_to_qual_teacher_ratio", "pupil_to_adult_ratio",
  "teachers_on_main_pay_range_percent", "teachers_on_upper_pay_range_percent",
  "percentage_taking_absence", "total_number_of_days_lost",
  "entrants_to_the_school", "leavers_from_the_school"
)
for (col in optional_cols) panel <- ensure_col(panel, col)

# ============================================================
# 2. BUILD LA-LEVEL INDICATORS
# ============================================================

message("\n=== Building LA-level indicators ===\n")

# ---- 2a. School counts and composition ----
la_school_counts <- panel %>%
  group_by(LANAME, academic_year) %>%
  summarise(n_schools = n_distinct(URN), .groups = "drop") %>%
  group_by(LANAME) %>%
  summarise(
    n_schools_mean = mean(n_schools, na.rm = TRUE),
    n_schools_max  = max(n_schools, na.rm = TRUE),
    n_schools_min  = min(n_schools, na.rm = TRUE),
    .groups = "drop"
  )

# ---- 2b. Attainment indicators (ATT8 all / FSM / non-FSM) ----
la_attainment <- panel %>%
  group_by(LANAME) %>%
  summarise(
    # Overall ATT8
    att8_mean           = mean(ATT8SCR, na.rm = TRUE),
    att8_median         = median(ATT8SCR, na.rm = TRUE),
    att8_sd             = sd(ATT8SCR, na.rm = TRUE),
    att8_min            = min(ATT8SCR, na.rm = TRUE),
    att8_max            = max(ATT8SCR, na.rm = TRUE),
    att8_range          = max(ATT8SCR, na.rm = TRUE) - min(ATT8SCR, na.rm = TRUE),
    att8_cv             = sd(ATT8SCR, na.rm = TRUE) / mean(ATT8SCR, na.rm = TRUE),
    att8_iqr            = IQR(ATT8SCR, na.rm = TRUE),
    att8_p25            = quantile(ATT8SCR, 0.25, na.rm = TRUE),
    att8_p75            = quantile(ATT8SCR, 0.75, na.rm = TRUE),

    # Disadvantaged ATT8
    att8_fsm_mean       = mean(ATT8SCR_FSM6CLA1A, na.rm = TRUE),
    att8_fsm_median     = median(ATT8SCR_FSM6CLA1A, na.rm = TRUE),
    att8_fsm_sd         = sd(ATT8SCR_FSM6CLA1A, na.rm = TRUE),
    att8_fsm_min        = min(ATT8SCR_FSM6CLA1A, na.rm = TRUE),
    att8_fsm_max        = max(ATT8SCR_FSM6CLA1A, na.rm = TRUE),
    att8_fsm_range      = max(ATT8SCR_FSM6CLA1A, na.rm = TRUE) - min(ATT8SCR_FSM6CLA1A, na.rm = TRUE),
    att8_fsm_cv         = sd(ATT8SCR_FSM6CLA1A, na.rm = TRUE) / mean(ATT8SCR_FSM6CLA1A, na.rm = TRUE),

    # Non-disadvantaged ATT8
    att8_nfsm_mean      = mean(ATT8SCR_NFSM6CLA1A, na.rm = TRUE),
    att8_nfsm_median    = median(ATT8SCR_NFSM6CLA1A, na.rm = TRUE),
    att8_nfsm_sd        = sd(ATT8SCR_NFSM6CLA1A, na.rm = TRUE),
    att8_nfsm_min       = min(ATT8SCR_NFSM6CLA1A, na.rm = TRUE),
    att8_nfsm_max       = max(ATT8SCR_NFSM6CLA1A, na.rm = TRUE),
    att8_nfsm_cv        = sd(ATT8SCR_NFSM6CLA1A, na.rm = TRUE) / mean(ATT8SCR_NFSM6CLA1A, na.rm = TRUE),

    # Disadvantaged gap
    att8_gap_mean       = mean(ATT8SCR_NFSM6CLA1A - ATT8SCR_FSM6CLA1A, na.rm = TRUE),
    att8_gap_median     = median(ATT8SCR_NFSM6CLA1A - ATT8SCR_FSM6CLA1A, na.rm = TRUE),
    att8_gap_sd         = sd(ATT8SCR_NFSM6CLA1A - ATT8SCR_FSM6CLA1A, na.rm = TRUE),

    # English/Maths subscores
    att8_eng_mean       = mean(ATT8SCRENG, na.rm = TRUE),
    att8_eng_sd         = sd(ATT8SCRENG, na.rm = TRUE),
    att8_mat_mean       = mean(ATT8SCRMAT, na.rm = TRUE),
    att8_mat_sd         = sd(ATT8SCRMAT, na.rm = TRUE),
    att8_eng_mat_diff   = mean(ATT8SCRENG - ATT8SCRMAT, na.rm = TRUE),

    # Gender gap
    att8_girls_mean     = mean(ATT8SCR_GIRLS, na.rm = TRUE),
    att8_boys_mean      = mean(ATT8SCR_BOYS, na.rm = TRUE),
    att8_gender_gap     = mean(ATT8SCR_GIRLS - ATT8SCR_BOYS, na.rm = TRUE),

    # Progress 8
    p8_mean             = mean(P8MEA, na.rm = TRUE),
    p8_median           = median(P8MEA, na.rm = TRUE),
    p8_sd               = sd(P8MEA, na.rm = TRUE),
    p8_fsm_mean         = mean(P8MEA_FSM6CLA1A, na.rm = TRUE),
    p8_nfsm_mean        = mean(P8MEA_NFSM6CLA1A, na.rm = TRUE),
    p8_gap_mean         = mean(P8MEA_NFSM6CLA1A - P8MEA_FSM6CLA1A, na.rm = TRUE),

    .groups = "drop"
  )

# ---- 2c. Temporal trends in attainment (slope across year_numeric) ----
la_trends <- panel %>%
  filter(!is.na(ATT8SCR)) %>%
  group_by(LANAME) %>%
  do({
    df <- .
    n_yrs <- n_distinct(df$year_numeric)
    if (n_yrs >= 2) {
      tibble(
        att8_trend_slope      = coef(lm(ATT8SCR ~ year_numeric, data = df))[2],
        att8_fsm_trend_slope  = if (sum(!is.na(df$ATT8SCR_FSM6CLA1A)) >= 2)
          coef(lm(ATT8SCR_FSM6CLA1A ~ year_numeric, data = df))[2] else NA_real_,
        att8_nfsm_trend_slope = if (sum(!is.na(df$ATT8SCR_NFSM6CLA1A)) >= 2)
          coef(lm(ATT8SCR_NFSM6CLA1A ~ year_numeric, data = df))[2] else NA_real_,
        p8_trend_slope        = if (sum(!is.na(df$P8MEA)) >= 2)
          coef(lm(P8MEA ~ year_numeric, data = df))[2] else NA_real_
      )
    } else {
      tibble(att8_trend_slope = NA_real_, att8_fsm_trend_slope = NA_real_,
             att8_nfsm_trend_slope = NA_real_, p8_trend_slope = NA_real_)
    }
  }) %>%
  ungroup()

# ---- 2d. Pupil composition ----
la_pupils <- panel %>%
  group_by(LANAME) %>%
  summarise(
    # FSM / disadvantage
    pct_fsm_mean        = mean(PTFSM6CLA1A, na.rm = TRUE),
    pct_fsm_median      = median(PTFSM6CLA1A, na.rm = TRUE),
    pct_fsm_sd          = sd(PTFSM6CLA1A, na.rm = TRUE),
    pct_fsm_max         = max(PTFSM6CLA1A, na.rm = TRUE),
    pct_fsm_min         = min(PTFSM6CLA1A, na.rm = TRUE),
    pct_fsm_cv          = sd(PTFSM6CLA1A, na.rm = TRUE) / mean(PTFSM6CLA1A, na.rm = TRUE),

    # EAL (English not first language)
    pct_eal_mean        = mean(PNUMEAL, na.rm = TRUE),
    pct_eal_median      = median(PNUMEAL, na.rm = TRUE),
    pct_eal_sd          = sd(PNUMEAL, na.rm = TRUE),
    pct_eal_max         = max(PNUMEAL, na.rm = TRUE),
    pct_eal_cv          = sd(PNUMEAL, na.rm = TRUE) / mean(PNUMEAL, na.rm = TRUE),

    # Prior attainment
    pct_prior_lo_mean   = mean(PTPRIORLO, na.rm = TRUE),
    pct_prior_lo_sd     = sd(PTPRIORLO, na.rm = TRUE),
    pct_prior_hi_mean   = mean(PTPRIORHI, na.rm = TRUE),
    pct_prior_hi_sd     = sd(PTPRIORHI, na.rm = TRUE),
    prior_lo_hi_ratio   = mean(PTPRIORLO / PTPRIORHI, na.rm = TRUE),

    # SEN
    pct_sen_ehcp_mean   = mean(PSENELSE, na.rm = TRUE),
    pct_sen_ehcp_sd     = sd(PSENELSE, na.rm = TRUE),
    pct_sen_support_mean = mean(PSENELK, na.rm = TRUE),
    pct_sen_support_sd  = sd(PSENELK, na.rm = TRUE),

    # FSM (ever)
    pct_fsm_ever_mean   = mean(PNUMFSMEVER, na.rm = TRUE),
    pct_fsm_ever_sd     = sd(PNUMFSMEVER, na.rm = TRUE),

    # School size
    totpups_mean        = mean(TOTPUPS, na.rm = TRUE),
    totpups_median      = median(TOTPUPS, na.rm = TRUE),
    totpups_sd          = sd(TOTPUPS, na.rm = TRUE),
    totpups_max         = max(TOTPUPS, na.rm = TRUE),
    totpups_min         = min(TOTPUPS, na.rm = TRUE),
    totpups_cv          = sd(TOTPUPS, na.rm = TRUE) / mean(TOTPUPS, na.rm = TRUE),

    .groups = "drop"
  )

# Add gender columns if available
if ("gender_name" %in% names(panel)) {
  la_pupils_gender <- panel %>%
    group_by(LANAME) %>%
    summarise(
      pct_schools_mixed = mean(gender_name == "Mixed", na.rm = TRUE),
      pct_schools_boys  = mean(gender_name == "Boys",  na.rm = TRUE),
      pct_schools_girls = mean(gender_name == "Girls", na.rm = TRUE),
      .groups = "drop"
    )
  la_pupils <- la_pupils %>% left_join(la_pupils_gender, by = "LANAME")
} else {
  la_pupils <- la_pupils %>%
    mutate(pct_schools_mixed = NA_real_, pct_schools_boys = NA_real_,
           pct_schools_girls = NA_real_)
}

# ---- 2e. Absence ----
la_absence <- panel %>%
  group_by(LANAME) %>%
  summarise(
    abs_overall_mean    = mean(PERCTOT, na.rm = TRUE),
    abs_overall_median  = median(PERCTOT, na.rm = TRUE),
    abs_overall_sd      = sd(PERCTOT, na.rm = TRUE),
    abs_overall_max     = max(PERCTOT, na.rm = TRUE),
    abs_overall_cv      = sd(PERCTOT, na.rm = TRUE) / mean(PERCTOT, na.rm = TRUE),
    abs_persist_mean    = mean(PPERSABS10, na.rm = TRUE),
    abs_persist_median  = median(PPERSABS10, na.rm = TRUE),
    abs_persist_sd      = sd(PPERSABS10, na.rm = TRUE),
    abs_persist_max     = max(PPERSABS10, na.rm = TRUE),
    abs_persist_cv      = sd(PPERSABS10, na.rm = TRUE) / mean(PPERSABS10, na.rm = TRUE),
    abs_persist_to_overall_ratio = mean(PPERSABS10 / PERCTOT, na.rm = TRUE),
    .groups = "drop"
  )

# ---- 2f. Absence temporal trends ----
la_abs_trends <- panel %>%
  filter(!is.na(PERCTOT)) %>%
  group_by(LANAME) %>%
  do({
    df <- .
    if (n_distinct(df$year_numeric) >= 2) {
      tibble(
        abs_trend_slope       = coef(lm(PERCTOT ~ year_numeric, data = df))[2],
        abs_persist_trend     = if (sum(!is.na(df$PPERSABS10)) >= 2)
          coef(lm(PPERSABS10 ~ year_numeric, data = df))[2] else NA_real_
      )
    } else {
      tibble(abs_trend_slope = NA_real_, abs_persist_trend = NA_real_)
    }
  }) %>%
  ungroup()

# ---- 2g. Ofsted ratings ----
la_ofsted <- panel %>%
  filter(!is.na(OFSTEDRATING_1)) %>%
  group_by(LANAME) %>%
  summarise(
    ofsted_outstanding_pct  = mean(OFSTEDRATING_1 == "Outstanding", na.rm = TRUE),
    ofsted_good_pct         = mean(OFSTEDRATING_1 == "Good", na.rm = TRUE),
    ofsted_ri_pct           = mean(OFSTEDRATING_1 == "Requires Improvement", na.rm = TRUE),
    ofsted_inadequate_pct   = mean(OFSTEDRATING_1 == "Inadequate", na.rm = TRUE),
    ofsted_good_plus_pct    = mean(OFSTEDRATING_1 %in% c("Outstanding", "Good"), na.rm = TRUE),
    ofsted_below_good_pct   = mean(OFSTEDRATING_1 %in% c("Requires Improvement", "Inadequate"), na.rm = TRUE),
    ofsted_numeric_mean     = mean(as.numeric(OFSTEDRATING_1), na.rm = TRUE),
    ofsted_numeric_sd       = sd(as.numeric(OFSTEDRATING_1), na.rm = TRUE),
    .groups = "drop"
  )

# ---- 2h. Segregation ----
la_segregation <- panel %>%
  filter(!is.na(gorard_segregation)) %>%
  group_by(LANAME) %>%
  summarise(
    gorard_mean     = mean(gorard_segregation, na.rm = TRUE),
    gorard_median   = median(gorard_segregation, na.rm = TRUE),
    gorard_sd       = sd(gorard_segregation, na.rm = TRUE),
    gorard_max      = max(gorard_segregation, na.rm = TRUE),
    gorard_min      = min(gorard_segregation, na.rm = TRUE),
    gorard_trend    = if (n_distinct(academic_year) >= 2)
      tryCatch(coef(lm(gorard_segregation ~ as.numeric(factor(academic_year))))[2],
               error = function(e) NA_real_)
    else NA_real_,
    .groups = "drop"
  )

# ---- 2i. School type composition ----
la_school_types <- panel %>%
  group_by(LANAME) %>%
  summarise(
    # Admissions policy
    pct_selective        = mean(ADMPOL_PT == "Selective", na.rm = TRUE),
    pct_comprehensive    = mean(ADMPOL_PT == "Comprehensive", na.rm = TRUE),
    pct_modern          = mean(ADMPOL_PT == "Modern", na.rm = TRUE),

    # School type
    pct_academy         = mean(grepl("cademy", MINORGROUP, ignore.case = TRUE), na.rm = TRUE),
    pct_converter_acad  = mean(grepl("converter", MINORGROUP, ignore.case = TRUE), na.rm = TRUE),
    pct_sponsored_acad  = mean(grepl("sponsored", MINORGROUP, ignore.case = TRUE), na.rm = TRUE),
    pct_free_school     = mean(grepl("free school", MINORGROUP, ignore.case = TRUE), na.rm = TRUE),
    pct_va_school       = mean(grepl("oluntary aided", SCHOOLTYPE, ignore.case = TRUE), na.rm = TRUE),
    pct_vc_school       = mean(grepl("oluntary controlled", SCHOOLTYPE, ignore.case = TRUE), na.rm = TRUE),
    pct_community       = mean(grepl("community", SCHOOLTYPE, ignore.case = TRUE), na.rm = TRUE),
    pct_foundation      = mean(grepl("foundation", SCHOOLTYPE, ignore.case = TRUE), na.rm = TRUE),

    # Religious character
    pct_religious       = mean(!grepl("none|does not apply|^$", RELCHAR,
                                      ignore.case = TRUE) & !is.na(RELCHAR), na.rm = TRUE),
    pct_ce              = mean(grepl("church of england|C of E", RELCHAR,
                                     ignore.case = TRUE), na.rm = TRUE),
    pct_rc              = mean(grepl("roman catholic|catholic", RELCHAR,
                                     ignore.case = TRUE), na.rm = TRUE),

    .groups = "drop"
  )

# ---- 2j. Workforce indicators ----
la_workforce <- panel %>%
  group_by(LANAME) %>%
  summarise(
    # Teacher retention
    retention_mean          = mean(remained_in_the_same_school, na.rm = TRUE),
    retention_median        = median(remained_in_the_same_school, na.rm = TRUE),
    retention_sd            = sd(remained_in_the_same_school, na.rm = TRUE),
    retention_min           = min(remained_in_the_same_school, na.rm = TRUE),
    retention_max           = max(remained_in_the_same_school, na.rm = TRUE),
    retention_cv            = sd(remained_in_the_same_school, na.rm = TRUE) /
                              mean(remained_in_the_same_school, na.rm = TRUE),

    # Sickness
    sickness_mean           = mean(average_number_of_days_taken, na.rm = TRUE),
    sickness_median         = median(average_number_of_days_taken, na.rm = TRUE),
    sickness_sd             = sd(average_number_of_days_taken, na.rm = TRUE),
    sickness_max            = max(average_number_of_days_taken, na.rm = TRUE),
    sickness_cv             = sd(average_number_of_days_taken, na.rm = TRUE) /
                              mean(average_number_of_days_taken, na.rm = TRUE),

    # Leadership pay
    leadership_pay_mean     = mean(teachers_on_leadership_pay_range_percent, na.rm = TRUE),
    leadership_pay_median   = median(teachers_on_leadership_pay_range_percent, na.rm = TRUE),
    leadership_pay_sd       = sd(teachers_on_leadership_pay_range_percent, na.rm = TRUE),

    # Teacher FTE
    fte_teachers_mean       = mean(teacher_fte_in_census_year, na.rm = TRUE),
    fte_teachers_sd         = sd(teacher_fte_in_census_year, na.rm = TRUE),
    fte_teachers_cv         = sd(teacher_fte_in_census_year, na.rm = TRUE) /
                              mean(teacher_fte_in_census_year, na.rm = TRUE),

    # Full workforce
    fte_workforce_mean      = mean(fte_workforce, na.rm = TRUE),
    fte_workforce_sd        = sd(fte_workforce, na.rm = TRUE),
    fte_classroom_mean      = mean(fte_classroom_teachers, na.rm = TRUE),
    fte_leadership_mean     = mean(fte_leadership_teachers, na.rm = TRUE),
    fte_ta_mean             = mean(fte_teaching_assistants, na.rm = TRUE),
    fte_ta_sd               = sd(fte_teaching_assistants, na.rm = TRUE),
    fte_leadership_prop     = mean(fte_leadership_teachers / fte_all_teachers, na.rm = TRUE),
    fte_ta_to_teacher_ratio = mean(fte_teaching_assistants / fte_all_teachers, na.rm = TRUE),

    # Pupil-teacher ratios
    ptr_qual_mean           = mean(pupil_to_qual_teacher_ratio, na.rm = TRUE),
    ptr_qual_sd             = sd(pupil_to_qual_teacher_ratio, na.rm = TRUE),
    ptr_qual_max            = max(pupil_to_qual_teacher_ratio, na.rm = TRUE),
    ptr_adult_mean          = mean(pupil_to_adult_ratio, na.rm = TRUE),
    ptr_adult_sd            = sd(pupil_to_adult_ratio, na.rm = TRUE),

    # Pay ranges
    pay_main_mean           = mean(teachers_on_main_pay_range_percent, na.rm = TRUE),
    pay_upper_mean          = mean(teachers_on_upper_pay_range_percent, na.rm = TRUE),
    pay_main_to_upper_ratio = mean(teachers_on_main_pay_range_percent /
                                   teachers_on_upper_pay_range_percent, na.rm = TRUE),

    # Sickness & absence taking
    pct_taking_sickness_mean = mean(percentage_taking_absence, na.rm = TRUE),
    total_days_lost_mean     = mean(total_number_of_days_lost, na.rm = TRUE),

    .groups = "drop"
  )

# ---- 2k. Workforce temporal trends ----
la_wf_trends <- panel %>%
  filter(!is.na(remained_in_the_same_school)) %>%
  group_by(LANAME) %>%
  do({
    df <- .
    if (n_distinct(df$year_numeric) >= 2) {
      tibble(
        retention_trend   = coef(lm(remained_in_the_same_school ~ year_numeric, data = df))[2],
        sickness_trend    = if (sum(!is.na(df$average_number_of_days_taken)) >= 2)
          coef(lm(average_number_of_days_taken ~ year_numeric, data = df))[2] else NA_real_,
        ptr_trend         = if (sum(!is.na(df$pupil_to_qual_teacher_ratio)) >= 2)
          coef(lm(pupil_to_qual_teacher_ratio ~ year_numeric, data = df))[2] else NA_real_
      )
    } else {
      tibble(retention_trend = NA_real_, sickness_trend = NA_real_, ptr_trend = NA_real_)
    }
  }) %>%
  ungroup()

# Safe correlation: returns NA instead of erroring when <2 complete pairs
safe_cor <- function(x, y) {
  complete <- complete.cases(x, y)
  if (sum(complete) < 2) return(NA_real_)
  suppressWarnings(cor(x[complete], y[complete]))
}

# ---- 2l. Composite / derived LA-level metrics ----
la_derived <- panel %>%
  group_by(LANAME) %>%
  summarise(
    # Inequality within LA
    att8_between_school_sd   = sd(ATT8SCR, na.rm = TRUE),
    fsm_between_school_sd    = sd(PTFSM6CLA1A, na.rm = TRUE),
    eal_between_school_sd    = sd(PNUMEAL, na.rm = TRUE),
    abs_between_school_sd    = sd(PERCTOT, na.rm = TRUE),

    # School churn (proportion of school-years present across all 4 years)
    n_school_years           = n(),
    n_unique_schools         = n_distinct(URN),
    avg_years_per_school     = n() / n_distinct(URN),

    # Correlation within-LA: FSM vs ATT8 (NA for LAs with <2 complete pairs)
    fsm_att8_cor             = safe_cor(PTFSM6CLA1A, ATT8SCR),
    eal_att8_cor             = safe_cor(PNUMEAL, ATT8SCR),
    abs_att8_cor             = safe_cor(PERCTOT, ATT8SCR),
    retention_att8_cor       = safe_cor(remained_in_the_same_school, ATT8SCR),
    .groups = "drop"
  )

# ---- 2m. Geographic / contextual (from la_lookup) ----
la_geo <- la_lookup_base %>%
  select(LANAME, gor_name, n_schools)

# ============================================================
# 3. JOIN ALL INDICATORS INTO ONE LA-LEVEL DATAFRAME
# ============================================================

message("\n=== Joining all LA indicators ===\n")

la_indicators <- la_geo %>%
  left_join(la_school_counts,  by = "LANAME") %>%
  left_join(la_attainment,     by = "LANAME") %>%
  left_join(la_trends,         by = "LANAME") %>%
  left_join(la_pupils,         by = "LANAME") %>%
  left_join(la_absence,        by = "LANAME") %>%
  left_join(la_abs_trends,     by = "LANAME") %>%
  left_join(la_ofsted,         by = "LANAME") %>%
  left_join(la_segregation,    by = "LANAME") %>%
  left_join(la_school_types,   by = "LANAME") %>%
  left_join(la_workforce,      by = "LANAME") %>%
  left_join(la_wf_trends,      by = "LANAME") %>%
  left_join(la_derived,        by = "LANAME")

message("LA indicators dataframe: ", nrow(la_indicators), " LAs, ",
        ncol(la_indicators), " variables")

# ---- 3a. Derived gap decomposition indicator ----
# gap_driver_index = z(att8_fsm_mean) - z(att8_nfsm_mean)
# Negative → gap driven by low FSM attainment (FSM pupils below avg; policy concern)
# Positive → gap driven by high non-FSM attainment (non-FSM above avg; less concerning)
# Near zero → both groups contribute symmetrically to the gap
fsm_mu  <- mean(la_indicators$att8_fsm_mean,  na.rm = TRUE)
fsm_s   <- sd(la_indicators$att8_fsm_mean,    na.rm = TRUE)
nfsm_mu <- mean(la_indicators$att8_nfsm_mean, na.rm = TRUE)
nfsm_s  <- sd(la_indicators$att8_nfsm_mean,   na.rm = TRUE)

la_indicators <- la_indicators %>%
  mutate(
    gap_driver_index = ((att8_fsm_mean - fsm_mu) / fsm_s) -
                       ((att8_nfsm_mean - nfsm_mu) / nfsm_s)
  )
message("  Added gap_driver_index (range: ",
        round(min(la_indicators$gap_driver_index, na.rm = TRUE), 2), " to ",
        round(max(la_indicators$gap_driver_index, na.rm = TRUE), 2), ")")

# Save the full indicators table
saveRDS(la_indicators, here::here("data", "la_indicators.rds"))
message("Saved la_indicators.rds")

# ============================================================
# 4. K-MEANS CLUSTERING
# ============================================================

message("\n=== Preparing data for clustering ===\n")

# ---- 4a. Identify tiny/atypical LAs to exclude from initial clustering ----
# LAs with very few schools are extreme outliers (e.g. City of London = 1 school)
# and will dominate the k-means solution. We cluster the main group first,
# then assign outlier LAs to the nearest centroid post-hoc.

n_schools_per_la <- la_derived %>% select(LANAME, n_unique_schools)

# Threshold: LAs with fewer than 5 unique schools are treated as outliers
OUTLIER_THRESHOLD <- 5
la_outliers  <- n_schools_per_la %>% filter(n_unique_schools < OUTLIER_THRESHOLD)
la_main_names <- n_schools_per_la %>%
  filter(n_unique_schools >= OUTLIER_THRESHOLD) %>%
  pull(LANAME)

if (nrow(la_outliers) > 0) {
  message("Excluding ", nrow(la_outliers), " tiny LA(s) from initial clustering: ",
          paste(la_outliers$LANAME, collapse = ", "))
} else {
  message("No tiny LAs to exclude.")
}

# ---- 4b. Prepare scaled feature matrix ----

# Select only numeric columns for clustering (exclude identifiers and categorical)
cluster_input_all <- la_indicators %>%
  select(-LANAME, -gor_name) %>%
  select(where(is.numeric))

message("Numeric variables available: ", ncol(cluster_input_all))

# Remove columns with >30% missing
miss_pct <- colMeans(is.na(cluster_input_all))
cluster_input_all <- cluster_input_all[, miss_pct <= 0.30]
message("After removing >30% missing: ", ncol(cluster_input_all), " variables")

# Remove near-zero variance columns
col_sds <- sapply(cluster_input_all, function(x) sd(x, na.rm = TRUE))
cluster_input_all <- cluster_input_all[, !is.na(col_sds) & col_sds > 1e-6]
message("After removing near-zero variance: ", ncol(cluster_input_all), " variables")

# Remove highly correlated columns (|r| > 0.97)
cor_mat <- cor(cluster_input_all, use = "pairwise.complete.obs")
cor_mat[is.na(cor_mat)] <- 0
high_cor <- which(abs(cor_mat) > 0.97 & upper.tri(cor_mat), arr.ind = TRUE)
if (nrow(high_cor) > 0) {
  cols_to_drop <- unique(high_cor[, 2])
  cluster_input_all <- cluster_input_all[, -cols_to_drop]
  message("After removing highly correlated (|r|>0.97): ", ncol(cluster_input_all), " variables")
}

# Impute NAs with column median
cluster_imputed_all <- cluster_input_all %>%
  mutate(across(everything(), ~ ifelse(is.na(.), median(., na.rm = TRUE), .)))

# Standardise to z-scores using the full LA set (for centroid calculation)
# Scale parameters from ALL LAs (so outlier assignment uses the same scale)
scale_center <- colMeans(cluster_imputed_all, na.rm = TRUE)
scale_sd     <- apply(cluster_imputed_all, 2, sd, na.rm = TRUE)
scale_sd[scale_sd == 0] <- 1  # avoid division by zero

cluster_scaled_all <- scale(cluster_imputed_all,
                             center = scale_center,
                             scale  = scale_sd)
rownames(cluster_scaled_all) <- la_indicators$LANAME

# Split into main and outlier sets
main_idx    <- which(la_indicators$LANAME %in% la_main_names)
outlier_idx <- which(!la_indicators$LANAME %in% la_main_names)

cluster_scaled_main <- cluster_scaled_all[main_idx, , drop = FALSE]

message("Main clustering set: ", nrow(cluster_scaled_main), " LAs x ",
        ncol(cluster_scaled_main), " variables")

# ============================================================
# 5. SELECT OPTIMAL k ON THE MAIN SET
# ============================================================

message("\n=== Selecting optimal number of clusters ===\n")

# Use a broader k range (5..10) — we know k=4 was too coarse
K_MIN <- 5
K_MAX <- 10
k_range <- K_MIN:K_MAX

# Helper for CH index
ch_index <- function(km, n) {
  k   <- length(km$size)
  bss <- km$betweenss
  wss <- km$tot.withinss
  if (k <= 1 || wss == 0) return(NA_real_)
  (bss / (k - 1)) / (wss / (n - k))
}

set.seed(42)
wss_vals  <- numeric(length(k_range))
sil_vals  <- numeric(length(k_range))
ch_vals   <- numeric(length(k_range))

n_main <- nrow(cluster_scaled_main)

for (i in seq_along(k_range)) {
  k  <- k_range[i]
  km <- kmeans(cluster_scaled_main, centers = k, nstart = 100, iter.max = 300)
  wss_vals[i] <- km$tot.withinss
  sil_obj     <- silhouette(km$cluster, dist(cluster_scaled_main))
  sil_vals[i] <- mean(sil_obj[, 3])
  ch_vals[i]  <- ch_index(km, n_main)
}

cluster_metrics <- tibble(
  k                 = k_range,
  wss               = wss_vals,
  silhouette        = sil_vals,
  calinski_harabasz = ch_vals,
  wss_improvement   = c(NA, -diff(wss_vals) / wss_vals[-length(wss_vals)])
)

message("\nCluster metrics (main LAs only):")
print(cluster_metrics, n = Inf)

saveRDS(cluster_metrics, here::here("data", "cluster_metrics.rds"))

# --- Choose k ---
# Strategy:
#   1. Find k with highest silhouette (best separation)
#   2. Find k with highest CH index
#   3. Find WSS elbow (largest 2nd derivative)
#   4. Pick the median of the three, clamped to [K_MIN, K_MAX]
#   5. But also check: if silhouette improves meaningfully beyond min, prefer that

best_sil_k <- k_range[which.max(sil_vals)]
best_ch_k  <- k_range[which.max(ch_vals)]

wss_d2  <- c(NA, NA, diff(diff(wss_vals)))
elbow_k <- k_range[which.max(wss_d2[-1]) + 1]

message("Best silhouette k=", best_sil_k,
        " | Best CH k=", best_ch_k,
        " | Elbow k=", elbow_k)

chosen_k <- as.integer(median(c(best_sil_k, best_ch_k, elbow_k)))
chosen_k <- max(K_MIN, min(K_MAX, chosen_k))
message("Chosen k = ", chosen_k)

# ============================================================
# 6. FINAL K-MEANS ON MAIN LAs
# ============================================================

message("\n=== Running final k-means with k = ", chosen_k, " ===\n")

set.seed(42)
km_final <- kmeans(cluster_scaled_main, centers = chosen_k,
                   nstart = 200, iter.max = 500)

message("Cluster sizes (main set): ", paste(table(km_final$cluster), collapse = ", "))
message("Between-SS / Total-SS: ",
        round(km_final$betweenss / km_final$totss * 100, 1), "%")

# ---- Assign clusters to all LAs (including outliers via nearest centroid) ----
la_indicators$cluster <- NA_integer_

# Main set: direct from kmeans
la_indicators$cluster[main_idx] <- km_final$cluster

# Outlier LAs: assign to nearest centroid by Euclidean distance
if (length(outlier_idx) > 0) {
  centroids <- km_final$centers  # chosen_k x p matrix

  for (idx in outlier_idx) {
    la_vec  <- cluster_scaled_all[idx, , drop = FALSE]
    dists   <- apply(centroids, 1, function(c) sqrt(sum((la_vec - c)^2, na.rm = TRUE)))
    nearest <- which.min(dists)
    la_indicators$cluster[idx] <- nearest
    message("  Outlier LA '", la_indicators$LANAME[idx],
            "' assigned to nearest cluster ", nearest,
            " (dist = ", round(min(dists), 2), ")")
  }
}

message("\nFinal cluster sizes (all LAs): ",
        paste(table(la_indicators$cluster), collapse = ", "))

# ============================================================
# 7. CLUSTER PROFILING, LABELS & PEN PORTRAITS
# ============================================================

message("\n=== Profiling clusters ===\n")

# Key variables for profiling (human-readable subset)
profile_vars <- c(
  "att8_mean", "att8_gap_mean", "gap_driver_index", "p8_mean",
  "pct_fsm_mean", "pct_eal_mean", "pct_prior_lo_mean", "pct_prior_hi_mean",
  "abs_overall_mean", "abs_persist_mean",
  "gorard_mean",
  "ofsted_good_plus_pct", "ofsted_outstanding_pct", "ofsted_ri_pct",
  "retention_mean", "sickness_mean", "ptr_qual_mean",
  "pct_selective", "pct_academy", "pct_religious", "pct_sponsored_acad",
  "totpups_mean", "n_schools_mean",
  "att8_trend_slope", "abs_trend_slope",
  "fte_ta_to_teacher_ratio", "leadership_pay_mean",
  "att8_gender_gap", "att8_eng_mat_diff",
  "pct_sen_ehcp_mean", "pct_fsm_cv",
  "retention_cv", "sickness_cv"
)
profile_vars <- intersect(profile_vars, names(la_indicators))

# Cluster means for profiling variables
cluster_profiles <- la_indicators %>%
  group_by(cluster) %>%
  summarise(
    n_las = n(),
    across(all_of(profile_vars), ~ mean(.x, na.rm = TRUE), .names = "mean_{.col}"),
    .groups = "drop"
  ) %>%
  arrange(cluster)

# Grand means + SDs for z-score computation
grand_stats <- la_indicators %>%
  summarise(
    across(all_of(profile_vars), list(
      m  = ~ mean(.x, na.rm = TRUE),
      sd = ~ sd(.x, na.rm = TRUE)
    ), .names = "{.col}__{.fn}")
  )

# Z-scores of cluster means relative to grand mean
cluster_z <- cluster_profiles %>%
  select(cluster, n_las) %>%
  bind_cols(
    map_dfc(profile_vars, function(v) {
      col_name <- paste0("mean_", v)
      grand_m  <- grand_stats[[paste0(v, "__m")]]
      grand_sd <- grand_stats[[paste0(v, "__sd")]]
      vals     <- cluster_profiles[[col_name]]
      z_vals   <- if (!is.na(grand_sd) && grand_sd > 0)
                    (vals - grand_m) / grand_sd
                  else
                    rep(0, nrow(cluster_profiles))
      tibble(!!paste0("z_", v) := z_vals)
    })
  )

message("Cluster z-score profiles computed")

# -----------------------------------------------------------------------
# 7a. CLUSTER LABELLING
# Assign a descriptive 2-3 word label by combining the top distinguishing
# z-score features. We use multiple features to avoid generic labels.
# -----------------------------------------------------------------------

# Descriptive atoms: variable -> short positive/negative word
var_atoms <- list(
  att8_mean             = list(hi = "High Attainment",    lo = "Low Attainment"),
  pct_fsm_mean          = list(hi = "Deprived",           lo = "Affluent"),
  abs_overall_mean      = list(hi = "High Absence",       lo = "Low Absence"),
  abs_persist_mean      = list(hi = "Persistent Absence", lo = "Low Pers. Absence"),
  pct_eal_mean          = list(hi = "High EAL",           lo = "Low EAL"),
  pct_selective         = list(hi = "Selective",          lo = "Non-Selective"),
  gorard_mean           = list(hi = "Segregated",         lo = "Integrated"),
  ofsted_good_plus_pct  = list(hi = "Strong Ofsted",      lo = "Weak Ofsted"),
  retention_mean        = list(hi = "Stable Workforce",   lo = "High Turnover"),
  sickness_mean         = list(hi = "High Sickness",      lo = "Low Sickness"),
  pct_academy           = list(hi = "Academy-Heavy",      lo = "LA-Maintained"),
  pct_religious         = list(hi = "Faith Schools",      lo = "Secular"),
  att8_gap_mean         = list(hi = "Wide Gap",           lo = "Narrow Gap"),
  gap_driver_index      = list(hi = "Non-FSM Driven Gap", lo = "FSM-Driven Gap"),
  ptr_qual_mean         = list(hi = "High PTR",           lo = "Low PTR"),
  pct_sponsored_acad    = list(hi = "Sponsored Academies",lo = "Few Sponsored")
)

# Priority order for labelling (most policy-relevant first)
label_priority <- c(
  "pct_selective", "pct_eal_mean", "att8_mean",
  "abs_overall_mean", "abs_persist_mean",
  "pct_fsm_mean", "gorard_mean",
  "ofsted_good_plus_pct", "retention_mean", "sickness_mean",
  "pct_academy", "pct_religious", "att8_gap_mean", "gap_driver_index",
  "ptr_qual_mean", "pct_sponsored_acad"
)

assign_cluster_label <- function(k, z_row) {
  # Build data frame of (var, |z|, z) for priority vars only
  scored <- map_dfr(label_priority, function(v) {
    z_col <- paste0("z_", v)
    if (!z_col %in% names(z_row)) return(NULL)
    z <- z_row[[z_col]]
    if (is.na(z)) return(NULL)
    tibble(var = v, abs_z = abs(z), z = z)
  }) %>%
    arrange(desc(abs_z))

  if (nrow(scored) == 0) return(paste("Cluster", k))

  # Take the top 1-2 features with |z| > 0.5
  top <- scored %>% filter(abs_z > 0.5) %>% slice_head(n = 2)

  if (nrow(top) == 0) {
    # No strong signal — use top feature regardless
    top <- scored %>% slice_head(n = 1)
  }

  # Build label parts
  parts <- purrr::map_chr(seq_len(nrow(top)), function(i) {
    v   <- top$var[i]
    z   <- top$z[i]
    atm <- var_atoms[[v]]
    if (is.null(atm)) return(paste("Cluster", k))
    if (z >= 0) atm$hi else atm$lo
  })

  # Collapse parts
  paste(parts, collapse = " / ")
}

cluster_labels <- map_chr(1:chosen_k, function(k) {
  z_row <- cluster_z %>% filter(cluster == k)
  assign_cluster_label(k, z_row)
})

# De-duplicate: if two clusters get identical labels, append a number
dups <- which(duplicated(cluster_labels))
for (d in dups) {
  cluster_labels[d] <- paste0(cluster_labels[d], " (", d, ")")
}

message("\nCluster labels: ", paste(cluster_labels, collapse = " | "))

# -----------------------------------------------------------------------
# 7b. PEN PORTRAITS
# -----------------------------------------------------------------------

generate_pen_portrait <- function(cluster_num, profile_row, z_row) {
  n <- profile_row$n_las

  # Describe a variable relative to England average
  desc <- function(var, threshold = 0.35) {
    z_col <- paste0("z_", var)
    if (!z_col %in% names(z_row)) return(list(dir = "average", z = 0))
    z <- z_row[[z_col]]
    if (is.na(z)) return(list(dir = "average", z = 0))
    dir <- if (z >  threshold) "above" else
           if (z < -threshold) "below"  else "average"
    list(dir = dir, z = z)
  }

  # Helper to get formatted mean value
  gm <- function(var, digits = 1, pct = FALSE) {
    col <- paste0("mean_", var)
    if (!col %in% names(profile_row)) return("N/A")
    val <- profile_row[[col]]
    if (is.na(val)) return("N/A")
    if (pct) return(paste0(round(val * 100, 0), "%"))
    as.character(round(val, digits))
  }

  # Key values
  att8_v    <- gm("att8_mean")
  gap_v     <- gm("att8_gap_mean")
  fsm_att8  <- gm("att8_fsm_mean")
  nfsm_att8 <- gm("att8_nfsm_mean")
  fsm_v     <- gm("pct_fsm_mean", 1)
  eal_v     <- gm("pct_eal_mean", 1)
  abs_v     <- gm("abs_overall_mean", 1)
  persabs_v <- gm("abs_persist_mean", 1)
  ofsted_v  <- gm("ofsted_good_plus_pct", pct = TRUE)
  gorard_v  <- gm("gorard_mean", 3)
  ret_v     <- gm("retention_mean")
  sick_v    <- gm("sickness_mean")
  ptr_v     <- gm("ptr_qual_mean")
  sel_v     <- gm("pct_selective", pct = TRUE)
  acad_v    <- gm("pct_academy", pct = TRUE)

  # Directional descriptors
  att8_d   <- desc("att8_mean")
  gap_d    <- desc("att8_gap_mean")
  gapdr_d  <- desc("gap_driver_index")
  fsm_d    <- desc("pct_fsm_mean")
  eal_d    <- desc("pct_eal_mean")
  abs_d    <- desc("abs_overall_mean")
  pabs_d   <- desc("abs_persist_mean")
  ofst_d   <- desc("ofsted_good_plus_pct")
  gor_d    <- desc("gorard_mean")
  ret_d    <- desc("retention_mean")
  sick_d   <- desc("sickness_mean")
  sel_d    <- desc("pct_selective")
  acad_d   <- desc("pct_academy")

  # Header line
  header <- paste0(
    "Cluster ", cluster_num, " (", n, " local authorit", if(n==1) "y" else "ies", ")\n",
    "ATT8: ", att8_v, " | Gap: ", gap_v, " (FSM: ", fsm_att8, " / non-FSM: ", nfsm_att8,
    ") | FSM: ", fsm_v, "% | Absence: ", abs_v, "% | Good+ Ofsted: ", ofsted_v
  )

  # Narrative sentences
  sentences <- c()

  # Attainment
  if (att8_d$dir != "average") {
    sentences <- c(sentences,
      paste0("Attainment is ", att8_d$dir, " average (ATT8 = ", att8_v, "), ",
             if (gap_d$dir == "above") paste0("with a wider than average disadvantage gap (", gap_v, " points)")
             else if (gap_d$dir == "below") paste0("with a narrower than average disadvantage gap (", gap_v, " points)")
             else paste0("with a broadly average disadvantage gap (", gap_v, " points)"),
             "."))
  } else {
    sentences <- c(sentences,
      paste0("Attainment is broadly average (ATT8 = ", att8_v, ")."))
  }

  # Gap driver decomposition
  if (gap_d$dir == "above" && gapdr_d$dir != "average") {
    if (gapdr_d$dir == "below") {
      sentences <- c(sentences,
        paste0("The gap is primarily driven by lower-than-average disadvantaged pupil attainment ",
               "(FSM ATT8: ", fsm_att8, ", non-FSM ATT8: ", nfsm_att8, ")."))
    } else {
      sentences <- c(sentences,
        paste0("The gap is primarily driven by higher-than-average non-disadvantaged pupil attainment, ",
               "while FSM pupils perform closer to the national average ",
               "(FSM ATT8: ", fsm_att8, ", non-FSM ATT8: ", nfsm_att8, ")."))
    }
  } else if (gap_d$dir == "below" && gapdr_d$dir != "average") {
    if (gapdr_d$dir == "above") {
      sentences <- c(sentences,
        paste0("The narrower gap reflects non-disadvantaged pupils attaining closer to the national average ",
               "(FSM ATT8: ", fsm_att8, ", non-FSM ATT8: ", nfsm_att8, ")."))
    } else {
      sentences <- c(sentences,
        paste0("The narrower gap reflects relatively stronger disadvantaged pupil attainment ",
               "(FSM ATT8: ", fsm_att8, ", non-FSM ATT8: ", nfsm_att8, ")."))
    }
  }

  # Pupil composition
  fsm_sent <- if (fsm_d$dir != "average")
    paste0("Schools serve ", if(fsm_d$dir=="above") "higher" else "lower",
           " levels of disadvantage than average (", fsm_v, "% FSM)") else NULL
  eal_sent <- if (eal_d$dir != "average")
    paste0(if(eal_d$dir=="above") "substantially" else "relatively few", " EAL pupils (", eal_v, "%)") else NULL

  if (!is.null(fsm_sent) && !is.null(eal_sent)) {
    sentences <- c(sentences, paste0(fsm_sent, ", with ", eal_sent, "."))
  } else if (!is.null(fsm_sent)) {
    sentences <- c(sentences, paste0(fsm_sent, "."))
  } else if (!is.null(eal_sent)) {
    sentences <- c(sentences, paste0("There are ", eal_sent, "."))
  }

  # Absence
  if (abs_d$dir != "average") {
    sentences <- c(sentences,
      paste0("Overall absence is ", abs_d$dir, " the national average (",
             abs_v, "%), with persistent absence at ", persabs_v, "%."))
  }

  # Ofsted
  sentences <- c(sentences,
    paste0(if (ofst_d$dir == "above") "Ofsted ratings are strong: "
           else if (ofst_d$dir == "below") "Ofsted ratings are weaker than average: "
           else "Ofsted ratings are broadly typical: ",
           ofsted_v, " of schools are rated Good or Outstanding."))

  # Segregation
  if (gor_d$dir != "average") {
    sentences <- c(sentences,
      paste0("There is ", if(gor_d$dir=="above") "higher" else "lower",
             " than average FSM-based school segregation within LAs in this group (Gorard index: ", gorard_v, ")."))
  }

  # Selective admission
  if (sel_d$dir == "above") {
    sentences <- c(sentences,
      paste0("The cluster is notable for a high proportion of selective schools (", sel_v, ")."))
  }

  # Academisation
  if (acad_d$dir != "average") {
    sentences <- c(sentences,
      paste0("Academy schools make up ", acad_v, " of schools — ",
             if(acad_d$dir=="above") "higher" else "lower", " than the national average."))
  }

  # Workforce
  wf_parts <- c()
  if (ret_d$dir != "average")
    wf_parts <- c(wf_parts,
      paste0(if(ret_d$dir=="above") "strong" else "weaker",
             " teacher retention (", ret_v, " FTE)"))
  if (sick_d$dir != "average")
    wf_parts <- c(wf_parts,
      paste0(if(sick_d$dir=="above") "higher" else "lower",
             " teacher sickness absence (", sick_v, " days)"))

  if (length(wf_parts) > 0) {
    sentences <- c(sentences,
      paste0("Workforce characteristics include ", paste(wf_parts, collapse = " and "), "."))
  }

  paste0(header, "\n\n", paste(sentences, collapse = " "))
}

pen_portraits <- map_chr(1:chosen_k, function(k) {
  profile_row <- cluster_profiles %>% filter(cluster == k)
  z_row       <- cluster_z       %>% filter(cluster == k)
  generate_pen_portrait(k, profile_row, z_row)
})

message("\n=== Pen Portraits ===\n")
cat(paste(pen_portraits, collapse = "\n\n---\n\n"))

# ============================================================
# 8. BUILD SUMMARY TABLE & SAVE OUTPUTS
# ============================================================

message("\n=== Saving outputs ===\n")

cluster_summary <- cluster_profiles %>%
  mutate(
    label        = cluster_labels,
    pen_portrait = pen_portraits
  ) %>%
  select(cluster, label, n_las, everything()) %>%
  arrange(cluster)

# Add cluster label to la_indicators
la_indicators <- la_indicators %>%
  left_join(
    cluster_summary %>% select(cluster, cluster_label = label),
    by = "cluster"
  )

# Build a slim la_typology dataframe for the app
la_typology <- la_indicators %>%
  select(LANAME, gor_name, cluster, cluster_label,
         # Key summary stats
         att8_mean, att8_fsm_mean, att8_nfsm_mean,
         att8_gap_mean, gap_driver_index,
         pct_fsm_mean, pct_eal_mean,
         abs_overall_mean, abs_persist_mean,
         ofsted_good_plus_pct, gorard_mean,
         retention_mean, sickness_mean, ptr_qual_mean,
         pct_selective, pct_academy,
         n_schools_mean, totpups_mean, att8_trend_slope,
         p8_mean, pct_religious, pct_sen_ehcp_mean)

# Save all outputs
saveRDS(la_indicators,   here::here("data", "la_indicators.rds"))
saveRDS(la_typology,     here::here("data", "la_typology.rds"))
saveRDS(cluster_summary, here::here("data", "la_cluster_summary.rds"))
saveRDS(cluster_metrics, here::here("data", "cluster_metrics.rds"))

# Save cluster selection metadata
cluster_meta <- list(
  chosen_k           = chosen_k,
  best_sil_k         = best_sil_k,
  best_ch_k          = best_ch_k,
  n_vars_used        = ncol(cluster_scaled_main),
  n_las              = nrow(la_indicators),
  n_las_main         = nrow(cluster_scaled_main),
  n_las_outlier      = length(outlier_idx),
  outlier_las        = if (nrow(la_outliers) > 0) la_outliers$LANAME else character(0),
  variance_explained = round(km_final$betweenss / km_final$totss * 100, 1),
  cluster_sizes      = as.vector(table(la_indicators$cluster)[as.character(1:chosen_k)]),
  cluster_labels     = cluster_labels,
  # Profile variable z-scores for radar charts
  profile_vars       = profile_vars,
  cluster_z_scores   = cluster_z
)

saveRDS(cluster_meta, here::here("data", "cluster_meta.rds"))

message("Saved: la_indicators.rds   (", nrow(la_indicators), " LAs, ", ncol(la_indicators), " vars)")
message("Saved: la_typology.rds     (", nrow(la_typology), " LAs)")
message("Saved: la_cluster_summary.rds (", chosen_k, " clusters)")
message("Saved: cluster_metrics.rds")
message("Saved: cluster_meta.rds")
message("\n=== Clustering complete ===")
message("k = ", chosen_k, " | Variance explained: ", cluster_meta$variance_explained, "%")
message("Cluster sizes: ", paste(cluster_meta$cluster_sizes, collapse = ", "))
message("Labels: ", paste(cluster_labels, collapse = " | "))
