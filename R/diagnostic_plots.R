# diagnostic_plots.R - Descriptive statistics and visual diagnostics for panel data
# ---------------------------------------------------------------------------------
# Generates:
#   1. Summary table of all numeric variables (N, NA%, mean, sd, min, Q1, med, Q3, max)
#   2. Summary of all factor/character variables (levels, NA%)
#   3. Histograms for continuous variables
#   4. Boxplots by year for key outcomes and predictors
#   5. Boxplots by Ofsted rating for outcomes
#   6. Missing-data heatmap by variable and year
#   7. Scatter matrix for key model variables (log-scale)
#   8. Outlier flags printed to console
#
# Outputs saved to: output/diagnostics/
# Run from project root: source("R/diagnostic_plots.R")
# ---------------------------------------------------------------------------------

library(tidyverse)
library(patchwork)
library(scales)
library(here)

source(here::here("R", "helpers.R"))

output_dir <- here::here("output", "diagnostics")
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)


# ---- Load data ----
# Use the most complete panel file available:
#   panel_data.rds  = final (after derived variables, step 04)
#   panel_with_workforce.rds = after workforce join (step 03)
#   panel_raw.rds   = after stacking years + external joins (step 02)
# Pick the most recently modified of those that exist.

panel_candidates <- c(
  here::here("data", "panel_data.rds"),
  here::here("data", "panel_with_workforce.rds"),
  here::here("data", "panel_raw.rds")
)
panel_exists <- panel_candidates[file.exists(panel_candidates)]

if (length(panel_exists) == 0) {
  stop("No panel data file found. Run the pipeline (01-04) first.")
}

# Pick the one modified most recently
panel_path <- panel_exists[which.max(file.mtime(panel_exists))]
message("Loading panel from: ", basename(panel_path))

panel <- readRDS(panel_path)
message("Panel loaded: ", nrow(panel), " rows, ", ncol(panel), " cols")
message("Years in panel: ", paste(sort(unique(panel$year_label)), collapse = ", "))


# ===========================================================================
# 1. NUMERIC SUMMARY TABLE
# ===========================================================================

numeric_vars <- panel %>%
  select(where(is.numeric)) %>%
  names()

numeric_summary <- map_dfr(numeric_vars, function(v) {
  x <- panel[[v]]
  tibble(
    variable    = v,
    description = var_label(v),
    n           = sum(!is.na(x)),
    n_missing   = sum(is.na(x)),
    pct_missing = round(mean(is.na(x)) * 100, 1),
    mean        = round(mean(x, na.rm = TRUE), 4),
    sd          = round(sd(x, na.rm = TRUE), 4),
    min         = round(min(x, na.rm = TRUE), 4),
    q1          = round(quantile(x, 0.25, na.rm = TRUE), 4),
    median      = round(median(x, na.rm = TRUE), 4),
    q3          = round(quantile(x, 0.75, na.rm = TRUE), 4),
    max         = round(max(x, na.rm = TRUE), 4),
    n_zero      = sum(x == 0, na.rm = TRUE),
    n_negative  = sum(x < 0, na.rm = TRUE)
  )
})

write_csv(numeric_summary, file.path(output_dir, "numeric_summary.csv"))
message("\nNumeric summary saved to numeric_summary.csv")
print(numeric_summary, n = 100)


# ===========================================================================
# 2. FACTOR / CHARACTER SUMMARY
# ===========================================================================

factor_vars <- panel %>%
  select(where(~ is.factor(.) | is.character(.))) %>%
  names()

factor_summary <- map_dfr(factor_vars, function(v) {
  x <- panel[[v]]
  levels_str <- if (is.factor(x)) {
    paste(levels(x), collapse = ", ")
  } else {
    vals <- sort(unique(na.omit(x)))
    if (length(vals) <= 20) paste(vals, collapse = ", ") else paste0(length(vals), " unique values")
  }
  tibble(
    variable    = v,
    description = var_label(v),
    type        = class(x)[1],
    n_distinct  = n_distinct(x, na.rm = TRUE),
    n_missing   = sum(is.na(x)),
    pct_missing = round(mean(is.na(x)) * 100, 1),
    levels      = levels_str
  )
})

write_csv(factor_summary, file.path(output_dir, "factor_summary.csv"))
message("\nFactor summary saved to factor_summary.csv")
print(factor_summary, n = 50)


# ===========================================================================
# 3. MISSING DATA HEATMAP BY VARIABLE AND YEAR
# ===========================================================================

# Focus on variables used in the model and key descriptors
key_vars <- c(
  "ATT8SCR", "ATT8SCR_FSM6CLA1A", "ATT8SCR_NFSM6CLA1A",
  "PTFSM6CLA1A", "PERCTOT", "PNUMEAL",
  "PTPRIORLO", "ADMPOL_PT",
  "gorard_segregation",
  "remained_in_the_same_school",
  "teachers_on_leadership_pay_range_percent",
  "average_number_of_days_taken",
  "OFSTEDRATING", "gor_name", "LANAME"
)
key_vars <- intersect(key_vars, names(panel))

missing_by_year <- panel %>%
  group_by(year_label) %>%
  summarise(
    across(all_of(key_vars), ~ round(mean(is.na(.)) * 100, 1)),
    .groups = "drop"
  ) %>%
  pivot_longer(-year_label, names_to = "variable", values_to = "pct_missing") %>%
  mutate(var_desc = var_label(variable, wrap_width = 40))

p_missing <- ggplot(missing_by_year, aes(x = year_label, y = var_desc, fill = pct_missing)) +
  geom_tile(colour = "white", linewidth = 0.5) +
  geom_text(aes(label = paste0(pct_missing, "%")), size = 3) +
  scale_fill_gradient(low = "#e8f5e9", high = "#c62828", name = "% Missing") +
  labs(
    title = "Missing Data by Variable and Year",
    subtitle = "Key model variables",
    x = "Academic Year", y = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(axis.text.y = element_text(size = 9))

ggsave(file.path(output_dir, "missing_heatmap.png"), p_missing,
       width = 10, height = 7, dpi = 150)
message("Missing heatmap saved")


# ===========================================================================
# 4. HISTOGRAMS OF CONTINUOUS VARIABLES
# ===========================================================================

#' Smart distribution plot: histogram for truly continuous variables,
#' bar chart for discrete / low-cardinality variables.
#' Coerces factors and characters to numeric where possible.
smart_distplot <- function(data, v, fill = "#1565c0", show_median = FALSE) {
  x <- data[[v]]
  vlabel <- var_label(v)

  # Coerce factor/character to numeric if the values are numeric-like
  if (is.factor(x) || is.character(x)) {
    x_num <- suppressWarnings(as.numeric(as.character(x)))
    if (sum(!is.na(x_num)) > 0) {
      data[[v]] <- x_num
      x <- x_num
    } else {
      # Truly categorical — use a bar chart
      return(
        ggplot(data %>% filter(!is.na(!!sym(v))), aes(x = !!sym(v))) +
          geom_bar(fill = fill, alpha = 0.7) +
          labs(title = vlabel, x = NULL, y = "Count") +
          theme_minimal(base_size = 9) +
          theme(axis.text.x = element_text(angle = 45, hjust = 1))
      )
    }
  }

  # For numeric data: use bar chart if few distinct values, histogram otherwise
  n_unique <- n_distinct(na.omit(x))

  if (n_unique <= 15) {
    p <- ggplot(data %>% filter(!is.na(!!sym(v))),
                aes(x = factor(!!sym(v)))) +
      geom_bar(fill = fill, alpha = 0.7) +
      labs(title = paste0(vlabel, "  (", n_unique, " distinct)"), x = NULL, y = "Count") +
      theme_minimal(base_size = 9)
  } else {
    p <- ggplot(data %>% filter(!is.na(!!sym(v))), aes(x = !!sym(v))) +
      geom_histogram(bins = 50, fill = fill, alpha = 0.7, colour = "white") +
      labs(title = vlabel, x = NULL, y = "Count") +
      theme_minimal(base_size = 9)

    if (show_median) {
      med_val <- median(x, na.rm = TRUE)
      p <- p + geom_vline(xintercept = med_val,
                           colour = "red", linetype = "dashed", linewidth = 0.8)
    }
  }

  p
}


# Outcome variables
outcome_vars <- c("ATT8SCR", "ATT8SCR_FSM6CLA1A", "ATT8SCR_NFSM6CLA1A")
outcome_vars <- intersect(outcome_vars, names(panel))

p_hist_outcomes <- map(outcome_vars, function(v) {
  smart_distplot(panel, v, fill = "#1565c0", show_median = TRUE)
}) %>%
  wrap_plots(ncol = 1) +
  plot_annotation(title = "Distributions of Outcome Variables",
                  subtitle = "Red dashed line = median (for continuous variables)")

ggsave(file.path(output_dir, "hist_outcomes.png"), p_hist_outcomes,
       width = 8, height = 9, dpi = 150)
message("Outcome histograms saved")


# Predictor variables (original scale)
pred_vars <- c(
  "PTFSM6CLA1A", "PERCTOT", "PNUMEAL", "PTPRIORLO", "ADMPOL_PT",
  "gorard_segregation", "remained_in_the_same_school",
  "teachers_on_leadership_pay_range_percent", "average_number_of_days_taken"
)
pred_vars <- intersect(pred_vars, names(panel))

p_hist_preds <- map(pred_vars, function(v) {
  smart_distplot(panel, v, fill = "#2e7d32")
}) %>%
  wrap_plots(ncol = 3) +
  plot_annotation(title = "Distributions of Predictor Variables")

ggsave(file.path(output_dir, "hist_predictors.png"), p_hist_preds,
       width = 14, height = 10, dpi = 150)
message("Predictor histograms saved")


# Log-transformed versions (what the model actually sees)
log_pred_vars <- c("PTFSM6CLA1A", "PERCTOT", "PNUMEAL",
                   "remained_in_the_same_school", "average_number_of_days_taken")
log_pred_vars <- intersect(log_pred_vars, names(panel))

p_hist_log <- map(log_pred_vars, function(v) {
  d <- panel %>%
    mutate(across(all_of(v), ~ suppressWarnings(as.numeric(as.character(.))))) %>%
    filter(!is.na(!!sym(v)), !!sym(v) > 0) %>%
    mutate(log_val = log(!!sym(v)))

  ggplot(d, aes(x = log_val)) +
    geom_histogram(bins = 50, fill = "#6a1b9a", alpha = 0.7, colour = "white") +
    labs(title = paste0("log(", var_label(v), ")"), x = NULL, y = "Count") +
    theme_minimal(base_size = 9)
}) %>%
  wrap_plots(ncol = 3) +
  plot_annotation(title = "Distributions of Log-Transformed Predictors",
                  subtitle = "These are what the model sees")

ggsave(file.path(output_dir, "hist_log_predictors.png"), p_hist_log,
       width = 14, height = 7, dpi = 150)
message("Log predictor histograms saved")


# ===========================================================================
# 5. BOXPLOTS BY YEAR
# ===========================================================================

boxplot_vars <- c(outcome_vars, "PTFSM6CLA1A", "PERCTOT", "PNUMEAL",
                  "gorard_segregation", "remained_in_the_same_school",
                  "average_number_of_days_taken")
boxplot_vars <- intersect(boxplot_vars, names(panel))

p_box_year <- map(boxplot_vars, function(v) {
  d <- panel %>%
    mutate(across(all_of(v), ~ suppressWarnings(as.numeric(as.character(.))))) %>%
    filter(!is.na(!!sym(v)))

  ggplot(d, aes(x = year_label, y = !!sym(v), fill = year_label)) +
    geom_boxplot(outlier.size = 0.5, outlier.alpha = 0.3) +
    labs(title = var_label(v), x = NULL, y = NULL) +
    theme_minimal(base_size = 9) +
    theme(legend.position = "none",
          axis.text.x = element_text(angle = 45, hjust = 1))
}) %>%
  wrap_plots(ncol = 3) +
  plot_annotation(title = "Variable Distributions by Academic Year",
                  subtitle = "Look for shifts in level or spread across years")

ggsave(file.path(output_dir, "boxplots_by_year.png"), p_box_year,
       width = 14, height = 12, dpi = 150)
message("Year boxplots saved")


# ===========================================================================
# 6. BOXPLOTS BY OFSTED RATING
# ===========================================================================

p_box_ofsted <- map(outcome_vars, function(v) {
  d <- panel %>%
    mutate(across(all_of(v), ~ suppressWarnings(as.numeric(as.character(.))))) %>%
    filter(!is.na(!!sym(v)), !is.na(OFSTEDRATING))

  ggplot(d, aes(x = OFSTEDRATING, y = !!sym(v), fill = OFSTEDRATING)) +
    geom_boxplot(outlier.size = 0.5, outlier.alpha = 0.3) +
    labs(title = var_label(v), x = "Ofsted Rating", y = NULL) +
    theme_minimal(base_size = 10) +
    theme(legend.position = "none")
}) %>%
  wrap_plots(ncol = 1) +
  plot_annotation(title = "Attainment by Ofsted Rating",
                  subtitle = "Ratings: 1 = Outstanding, 2 = Good, 3 = Requires Improvement, 4 = Inadequate")

ggsave(file.path(output_dir, "boxplots_by_ofsted.png"), p_box_ofsted,
       width = 8, height = 10, dpi = 150)
message("Ofsted boxplots saved")


# ===========================================================================
# 7. OFSTED RATING BREAKDOWN
# ===========================================================================

ofsted_counts <- panel %>%
  count(year_label, OFSTEDRATING) %>%
  filter(!is.na(OFSTEDRATING))

p_ofsted_bar <- ggplot(ofsted_counts, aes(x = year_label, y = n, fill = OFSTEDRATING)) +
  geom_col(position = "fill") +
  scale_y_continuous(labels = percent_format()) +
  labs(
    title = "Ofsted Rating Distribution by Year",
    subtitle = "Check for years with very few schools in certain categories",
    x = "Academic Year", y = "Proportion of Schools", fill = "Rating"
  ) +
  theme_minimal(base_size = 12)

ggsave(file.path(output_dir, "ofsted_distribution.png"), p_ofsted_bar,
       width = 9, height = 6, dpi = 150)
message("Ofsted distribution saved")


# ===========================================================================
# 8. SCATTER PAIRS FOR KEY MODEL RELATIONSHIPS
# ===========================================================================

# Sample for speed (scatter with full panel can be slow)
# Coerce scatter variables to numeric first
scatter_vars_all <- c("ATT8SCR", "PTFSM6CLA1A", "PERCTOT", "PNUMEAL",
                      "gorard_segregation", "remained_in_the_same_school",
                      "average_number_of_days_taken")
scatter_vars_all <- intersect(scatter_vars_all, names(panel))

set.seed(42)
sample_n_rows <- min(5000, nrow(panel))
panel_sample <- panel %>%
  mutate(across(all_of(scatter_vars_all),
                ~ suppressWarnings(as.numeric(as.character(.))))) %>%
  filter(!is.na(ATT8SCR), ATT8SCR > 0, PTFSM6CLA1A > 0, PERCTOT > 0) %>%
  slice_sample(n = sample_n_rows)

scatter_pairs <- list(
  c("PTFSM6CLA1A", "ATT8SCR"),
  c("PERCTOT", "ATT8SCR"),
  c("PNUMEAL", "ATT8SCR"),
  c("gorard_segregation", "ATT8SCR"),
  c("remained_in_the_same_school", "ATT8SCR"),
  c("average_number_of_days_taken", "ATT8SCR")
)

p_scatter <- map(scatter_pairs, function(pair) {
  xv <- pair[1]; yv <- pair[2]
  if (!all(c(xv, yv) %in% names(panel_sample))) return(NULL)

  ggplot(panel_sample %>% filter(!is.na(!!sym(xv)), !is.na(!!sym(yv))),
         aes(x = !!sym(xv), y = !!sym(yv))) +
    geom_point(alpha = 0.15, size = 0.8, colour = "#1565c0") +
    geom_smooth(method = "loess", se = FALSE, colour = "red", linewidth = 0.8) +
    labs(title = paste(var_label(yv), "~", var_label(xv)),
         x = var_label(xv), y = var_label(yv)) +
    theme_minimal(base_size = 9)
}) %>%
  compact() %>%
  wrap_plots(ncol = 3) +
  plot_annotation(title = "Bivariate Relationships with ATT8SCR",
                  subtitle = paste0("Random sample of ", sample_n_rows, " observations; red = loess smooth"))

ggsave(file.path(output_dir, "scatter_pairs.png"), p_scatter,
       width = 14, height = 8, dpi = 150)
message("Scatter plots saved")


# ===========================================================================
# 9. OUTLIER FLAGS
# ===========================================================================

message("\n=== OUTLIER FLAGS ===\n")

flag_outliers <- function(data, var_name, lower_q = 0.001, upper_q = 0.999) {
  x <- suppressWarnings(as.numeric(as.character(data[[var_name]])))
  if (all(is.na(x))) return(NULL)
  # Skip variables with very few distinct values (likely categorical)
  if (n_distinct(na.omit(x)) <= 10) return(NULL)
  data[[var_name]] <- x
  lo <- quantile(x, lower_q, na.rm = TRUE)
  hi <- quantile(x, upper_q, na.rm = TRUE)
  extreme <- data %>%
    filter(!!sym(var_name) < lo | !!sym(var_name) > hi) %>%
    select(URN, SCHNAME, LANAME, year_label, !!sym(var_name))
  if (nrow(extreme) > 0) {
    message(var_label(var_name), " [", var_name, "]: ",
            nrow(extreme), " extreme values (outside 0.1%-99.9% range)")
    message("  Range of extremes: ",
            round(min(extreme[[var_name]], na.rm = TRUE), 2), " to ",
            round(max(extreme[[var_name]], na.rm = TRUE), 2))
    message("  Normal range: ", round(lo, 2), " to ", round(hi, 2))
  }
  extreme
}

outlier_vars <- c(outcome_vars, pred_vars)
outliers <- map(outlier_vars, ~ flag_outliers(panel, .x))
names(outliers) <- outlier_vars

# Collect all flagged schools
all_outliers <- map2_dfr(outliers, names(outliers), function(df, v) {
  if (is.null(df) || nrow(df) == 0) return(NULL)
  df %>% mutate(flagged_variable = v)
})

if (nrow(all_outliers) > 0) {
  write_csv(all_outliers, file.path(output_dir, "outlier_flags.csv"))
  message("\nOutlier flags saved to outlier_flags.csv (", nrow(all_outliers), " rows)")
} else {
  message("\nNo extreme outliers flagged.")
}


# ===========================================================================
# 10. ZERO AND NEGATIVE VALUE CHECK (for log-transform safety)
# ===========================================================================

message("\n=== ZERO / NEGATIVE VALUE CHECK (log-transform variables) ===\n")

log_check_vars <- c("ATT8SCR", "ATT8SCR_FSM6CLA1A", "ATT8SCR_NFSM6CLA1A",
                     "PTFSM6CLA1A", "PERCTOT", "PNUMEAL",
                     "remained_in_the_same_school", "average_number_of_days_taken")
log_check_vars <- intersect(log_check_vars, names(panel))

for (v in log_check_vars) {
  x <- panel[[v]]
  n_zero <- sum(x == 0, na.rm = TRUE)
  n_neg  <- sum(x < 0, na.rm = TRUE)
  n_na   <- sum(is.na(x))
  n_ok   <- sum(x > 0, na.rm = TRUE)
  desc   <- var_label(v)
  message(sprintf("  %-50s  OK: %5d  Zero: %4d  Neg: %4d  NA: %5d",
                  paste0(desc, " [", v, "]"), n_ok, n_zero, n_neg, n_na))
}


# ===========================================================================
# DONE
# ===========================================================================

message("\n=== DIAGNOSTICS COMPLETE ===")
message("All outputs saved to: ", output_dir)
message("Files:")
message("  numeric_summary.csv       - Full descriptive statistics for all numeric variables")
message("  factor_summary.csv        - Summary of categorical variables")
message("  missing_heatmap.png       - Missing data by variable and year")
message("  hist_outcomes.png         - Histograms of ATT8 outcome variables")
message("  hist_predictors.png       - Histograms of predictor variables")
message("  hist_log_predictors.png   - Histograms of log-transformed predictors")
message("  boxplots_by_year.png      - Boxplots across academic years")
message("  boxplots_by_ofsted.png    - Attainment by Ofsted rating")
message("  ofsted_distribution.png   - Ofsted rating proportions by year")
message("  scatter_pairs.png         - Bivariate relationships with ATT8SCR")
message("  outlier_flags.csv         - Schools with extreme values")
