# 03a_add_finance.R - Download and join school financial data (CFR + AAR)
# -----------------------------------------------------------------------
# This script:
#   1. Downloads CFR (LA maintained schools) and AAR (academies) Excel files
#      from the DfE Financial Benchmarking and Insights Tool
#   2. Reads and harmonises column names across file formats and years
#   3. Stacks CFR + AAR into a single financial_returns dataset
#   4. Saves standalone data/financial_returns.rds
#   5. Joins key financial variables to the panel by URN + academic_year
#   6. Saves data/panel_with_finance.rds
#
# Data source: https://financial-benchmarking-and-insights-tool.education.gov.uk/data-sources
#
# CFR = Consistent Financial Reporting (LA maintained schools)
# AAR = Academy Accounts Return (academies)
#
# Depends on: R/03_add_workforce.R (produces panel_with_workforce.rds)
# Run from project root: source("R/03a_add_finance.R")
# -----------------------------------------------------------------------

source(here::here("R", "helpers.R"))

if (!requireNamespace("readxl", quietly = TRUE)) {
  stop("Package 'readxl' is required. Install with: install.packages('readxl')")
}
library(readxl)

cache_dir <- here::here("data", "cache")
dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)

FINANCE_BASE_URL <- "https://financial-benchmarking-and-insights-tool.education.gov.uk/files"

# Financial years aligned to the panel academic years
FINANCE_YEARS <- c("2021-22", "2022-23", "2023-24", "2024-25")


# ---- CFR (LA Maintained Schools) -----------------------------------------

#' Find the data sheet name in a CFR workbook
#' Sheet name varies across years: "CFRData", "CFR_Data", "CFR Data"
find_cfr_data_sheet <- function(path) {
  sheets <- excel_sheets(path)
  match <- sheets[grepl("^CFR[_ ]?Data$", sheets, ignore.case = TRUE)]
  if (length(match) == 0) stop("No CFR data sheet found in: ", basename(path))
  match[1]
}

#' Detect the header row in a CFR workbook
#' Older years (2021-22, 2022-23) have a title row before the header
find_cfr_skip <- function(path, sheet) {
  raw <- read_excel(path, sheet = sheet, col_names = FALSE, n_max = 5,
                    .name_repair = "minimal")
  for (r in 1:nrow(raw)) {
    if ("URN" %in% as.character(raw[r, ])) return(r - 1)
  }
  0
}

#' Read and standardise a single CFR file
read_cfr <- function(year) {
  url <- paste0(FINANCE_BASE_URL, "/CFR_", year, "_Full_Data_Workbook.xlsx")
  path <- file.path(cache_dir, paste0("cfr_", year, ".xlsx"))

  tryCatch({
    download_cached(url, path)
    sheet <- find_cfr_data_sheet(path)
    skip <- find_cfr_skip(path, sheet)
    message("  CFR ", year, ": sheet='", sheet, "', skip=", skip)

    df <- read_excel(path, sheet = sheet, skip = skip, .name_repair = "minimal")
    names(df) <- trimws(names(df))

    # Find columns by pattern matching (names vary across years)
    find_col <- function(patterns, df) {
      nms <- names(df)
      for (p in patterns) {
        m <- grep(p, nms, ignore.case = TRUE, value = TRUE)
        if (length(m) > 0) return(m[1])
      }
      NA_character_
    }

    col_map <- list(
      URN                 = find_col(c("^URN$"), df),
      school_name         = find_col(c("^School Name$"), df),
      la_code             = find_col(c("^LA$"), df),
      la_name             = find_col(c("^LA Name$"), df),
      phase               = find_col(c("^Overall Phase$"), df),
      num_pupils          = find_col(c("^No pupils$", "^Full time equivalent number of pupils in school$"), df),
      num_teachers        = find_col(c("^FTE Number of teachers$", "FTE.*teachers"), df),
      pct_fsm             = find_col(c("^% of pupils eligible for FSM$"), df),
      total_income        = find_col(c("^Total Income"), df),
      total_expenditure   = find_col(c("^Total Expenditure"), df),
      in_year_balance     = find_col(c("^In-year Balance", "^In year balance"), df),
      revenue_reserve     = find_col(c("^Revenue Reserve"), df),
      grant_funding       = find_col(c("^Grant Funding"), df),
      self_generated      = find_col(c("^Self Generated Funding"), df),
      staff_costs         = find_col(c("^Staff Total"), df),
      teaching_staff      = find_col(c("^E01 Teaching staff$", "^Teaching Staff E01$"), df),
      premises_costs      = find_col(c("^Premises:"), df),
      energy              = find_col(c("^E16 Energy$"), df),
      learning_resources  = find_col(c("^E19 Learning resources"), df),
      supply_staff        = find_col(c("^Supply Staff:"), df),
      catering_costs      = find_col(c("^Catering Expenses:"), df),
      educational_supplies = find_col(c("^Educational Supplies:"), df),
      bought_in_professional = find_col(c("^Brought in Professional Services:"), df),
      pupil_premium       = find_col(c("^I05 Pupil premium$"), df)
    )

    # Select and rename columns that were found
    valid_cols <- col_map[!is.na(col_map)]
    out <- df %>%
      select(all_of(unname(unlist(valid_cols)))) %>%
      setNames(names(valid_cols))

    # Coerce financials to numeric
    financial_cols <- setdiff(names(out), c("URN", "school_name", "la_code", "la_name", "phase"))
    for (col in financial_cols) {
      out[[col]] <- suppressWarnings(as.numeric(out[[col]]))
    }

    out$URN <- as.character(out$URN)
    # Convert "2021-22" → "2021-2022" to match panel academic_year format
    out$academic_year <- paste0(substr(year, 1, 4), "-20", substr(year, 6, 7))
    out$source <- "CFR"

    # Remove rows with no URN or where Did Not Supply
    out <- out %>% filter(!is.na(URN), URN != "")

    message("    CFR ", year, ": ", nrow(out), " schools")
    out
  }, error = function(e) {
    warning("Failed to read CFR ", year, ": ", e$message)
    NULL
  })
}


# ---- AAR (Academies) -----------------------------------------------------

#' Detect the header row in an AAR file
#' Older years have headers in row 3, newer years in row 2
find_aar_skip <- function(path) {
  raw <- read_excel(path, sheet = "Academies", col_names = FALSE, n_max = 5,
                    .name_repair = "minimal")
  for (r in 1:nrow(raw)) {
    if ("URN" %in% as.character(raw[r, ])) return(r - 1)
  }
  1  # default: skip 1 row
}

#' Read and standardise a single AAR file
read_aar <- function(year) {
  url <- paste0(FINANCE_BASE_URL, "/AAR_", year, "_download.xlsx")
  path <- file.path(cache_dir, paste0("aar_", year, ".xlsx"))

  tryCatch({
    download_cached(url, path)
    skip <- find_aar_skip(path)
    message("  AAR ", year, ": skip=", skip)

    df <- read_excel(path, sheet = "Academies", skip = skip, .name_repair = "minimal")
    names(df) <- trimws(names(df))

    # Deduplicate column names (AAR files sometimes have duplicate col names)
    dups <- duplicated(names(df))
    if (any(dups)) {
      names(df)[dups] <- paste0(names(df)[dups], "_dup")
    }

    find_col <- function(patterns, df) {
      nms <- names(df)
      for (p in patterns) {
        m <- grep(p, nms, ignore.case = TRUE, value = TRUE)
        # Prefer non-duplicate columns
        m_clean <- m[!grepl("_dup$", m)]
        if (length(m_clean) > 0) return(m_clean[1])
        if (length(m) > 0) return(m[1])
      }
      NA_character_
    }

    col_map <- list(
      URN                 = find_col(c("^URN$"), df),
      school_name         = find_col(c("^School Name$"), df),
      la_code             = find_col(c("^LA$"), df),
      phase               = find_col(c("^Overall Phase$"), df),
      num_pupils          = find_col(c("^Number of pupils in academy"), df),
      num_teachers        = find_col(c("^Number of teachers in academy"), df),
      pct_fsm             = find_col(c("^% of pupils eligible for FSM$"), df),
      total_income        = find_col(c("^Total Income$"), df),
      total_expenditure   = find_col(c("^Total Expenditure$"), df),
      in_year_balance     = find_col(c("^In year balance$", "^In-year balance$"), df),
      revenue_reserve     = find_col(c("^Revenue [Rr]eserve$"), df),
      grant_funding       = find_col(c("^Total Grant Funding$"), df),
      self_generated      = find_col(c("^Total Self Generated Funding$"), df),
      staff_costs         = find_col(c("^Total Staff Costs$"), df),
      teaching_staff      = find_col(c("^Teaching staff$"), df),
      premises_costs      = find_col(c("^Premises Costs$"), df),
      energy              = find_col(c("^Energy$"), df),
      learning_resources  = find_col(c("^Learning resources"), df),
      supply_staff        = find_col(c("^Supply Staff Costs$"), df),
      catering_costs      = find_col(c("^Catering Expenses$"), df),
      educational_supplies = find_col(c("^Total Costs of Educational Supplies$"), df),
      bought_in_professional = find_col(c("^Costs of Brought in Professional Services$"), df)
    )

    valid_cols <- col_map[!is.na(col_map)]
    out <- df %>%
      select(all_of(unname(unlist(valid_cols)))) %>%
      setNames(names(valid_cols))

    # Coerce financials to numeric
    financial_cols <- setdiff(names(out), c("URN", "school_name", "la_code", "la_name", "phase"))
    for (col in financial_cols) {
      out[[col]] <- suppressWarnings(as.numeric(out[[col]]))
    }

    out$URN <- as.character(out$URN)
    # Convert "2021-22" → "2021-2022" to match panel academic_year format
    out$academic_year <- paste0(substr(year, 1, 4), "-20", substr(year, 6, 7))
    out$source <- "AAR"

    out <- out %>% filter(!is.na(URN), URN != "")

    message("    AAR ", year, ": ", nrow(out), " schools")
    out
  }, error = function(e) {
    warning("Failed to read AAR ", year, ": ", e$message)
    NULL
  })
}


# ---- Combine and compute per-pupil metrics --------------------------------

#' Download all financial data, combine CFR + AAR, compute per-pupil metrics
build_financial_returns <- function() {
  message("\n=== Building financial returns dataset ===\n")

  # Read all CFR files
  message("Reading CFR files (LA maintained schools)...")
  cfr_list <- lapply(FINANCE_YEARS, read_cfr)

  # Read all AAR files (2024-25 may not exist yet)
  message("\nReading AAR files (academies)...")
  aar_list <- lapply(FINANCE_YEARS, read_aar)

  # Combine all non-NULL results
  all_dfs <- c(cfr_list, aar_list)
  all_dfs <- all_dfs[!sapply(all_dfs, is.null)]

  if (length(all_dfs) == 0) stop("No financial data files could be read")

  # Ensure all columns present (fill missing with NA)
  all_cols <- unique(unlist(lapply(all_dfs, names)))
  for (i in seq_along(all_dfs)) {
    missing <- setdiff(all_cols, names(all_dfs[[i]]))
    for (col in missing) all_dfs[[i]][[col]] <- NA
  }

  finance <- bind_rows(all_dfs)
  message("\nCombined: ", nrow(finance), " school-year records (",
          sum(finance$source == "CFR", na.rm = TRUE), " CFR, ",
          sum(finance$source == "AAR", na.rm = TRUE), " AAR)")

  # Check for and remove duplicates (same URN + year in both CFR and AAR)
  dupes <- finance %>%
    group_by(URN, academic_year) %>%
    filter(n() > 1)
  if (nrow(dupes) > 0) {
    message("  Removing ", nrow(dupes) - n_distinct(dupes$URN, dupes$academic_year),
            " duplicate URN-year records (keeping first)")
    finance <- finance %>%
      group_by(URN, academic_year) %>%
      slice(1) %>%
      ungroup()
  }

  # Compute per-pupil metrics where pupils > 0
  finance <- finance %>%
    mutate(
      total_income_pp     = ifelse(num_pupils > 0, total_income / num_pupils, NA_real_),
      total_expenditure_pp = ifelse(num_pupils > 0, total_expenditure / num_pupils, NA_real_),
      staff_costs_pp      = ifelse(num_pupils > 0, staff_costs / num_pupils, NA_real_),
      teaching_staff_pp   = ifelse(num_pupils > 0, teaching_staff / num_pupils, NA_real_),
      premises_costs_pp   = ifelse(num_pupils > 0, premises_costs / num_pupils, NA_real_),
      grant_funding_pp    = ifelse(num_pupils > 0, grant_funding / num_pupils, NA_real_),
      # Spending ratios
      staff_income_ratio  = ifelse(total_income > 0, staff_costs / total_income, NA_real_),
      teaching_expenditure_ratio = ifelse(total_expenditure > 0,
                                          teaching_staff / total_expenditure, NA_real_),
      balance_income_ratio = ifelse(total_income > 0,
                                    in_year_balance / total_income, NA_real_)
    )

  message("Final financial returns: ", nrow(finance), " rows, ", ncol(finance), " cols")
  message("Years: ", paste(sort(unique(finance$academic_year)), collapse = ", "))
  message("URNs: ", n_distinct(finance$URN))

  finance
}


# ---- Main execution -------------------------------------------------------

if (sys.nframe() == 0) {

  # Build the full financial returns dataset
  finance <- build_financial_returns()

  # Save standalone financial returns file
  out_path <- here::here("data", "financial_returns.rds")
  saveRDS(finance, out_path)
  message("\nSaved: ", out_path, " (", round(file.size(out_path) / 1e6, 1), " MB)")

  # Save a cached RDS for faster re-use
  saveRDS(finance, file.path(cache_dir, "financial_returns.rds"))

  # --- Join key financial variables to the panel ---
  panel_path <- here::here("data", "panel_with_workforce.rds")
  if (!file.exists(panel_path)) {
    stop("panel_with_workforce.rds not found. Run 03_add_workforce.R first.")
  }

  panel <- readRDS(panel_path)
  message("\nLoaded panel: ", nrow(panel), " rows, ", ncol(panel), " cols")

  # Select the key financial columns to add to the panel
  finance_for_panel <- finance %>%
    select(
      URN, academic_year,
      fin_total_income          = total_income,
      fin_total_expenditure     = total_expenditure,
      fin_in_year_balance       = in_year_balance,
      fin_revenue_reserve       = revenue_reserve,
      fin_grant_funding         = grant_funding,
      fin_self_generated        = self_generated,
      fin_staff_costs           = staff_costs,
      fin_teaching_staff        = teaching_staff,
      fin_premises_costs        = premises_costs,
      fin_energy                = energy,
      fin_pupil_premium         = pupil_premium,
      fin_total_income_pp       = total_income_pp,
      fin_total_expenditure_pp  = total_expenditure_pp,
      fin_staff_costs_pp        = staff_costs_pp,
      fin_teaching_staff_pp     = teaching_staff_pp,
      fin_grant_funding_pp      = grant_funding_pp,
      fin_staff_income_ratio    = staff_income_ratio,
      fin_teaching_expenditure_ratio = teaching_expenditure_ratio,
      fin_balance_income_ratio  = balance_income_ratio,
      fin_source                = source
    )

  panel <- panel %>%
    left_join(finance_for_panel, by = c("URN", "academic_year"))

  matched <- sum(!is.na(panel$fin_total_income))
  message("Financial data matched: ", matched, " / ", nrow(panel), " panel rows (",
          round(100 * matched / nrow(panel), 1), "%)")

  out_panel <- here::here("data", "panel_with_finance.rds")
  saveRDS(panel, out_panel)
  message("Saved: ", out_panel, " (", round(file.size(out_panel) / 1e6, 1), " MB)")

  message("\nDone! Financial data added to pipeline.")
}
