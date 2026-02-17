# global.R - Load data and models at Shiny app startup
# ----------------------------------------------------
# This runs once when the app starts and makes objects
# available to all sessions.
#
# Data is loaded from the lightweight data/ bundle
# created by R/06_prepare_app_data.R. This avoids loading
# the 55MB+ lme4 model objects — the simulator uses
# predict_slim() with extracted coefficients instead.
#
# Uses the imputed full model (Analysis E, 9 predictors)
# which covers all 4 years including 2024-25 (with
# carry-forward imputed workforce and prior attainment).
#
# All helper functions are in R/app_helpers.R so the app
# is fully self-contained for deployment (shinyapps.io).
# ----------------------------------------------------

library(tidyverse)
library(shiny)
library(bslib)
library(leaflet)
library(plotly)
library(DT)
library(sf)
library(scales)

# Source self-contained helper functions (predict_slim, slider config, etc.)
source("R/app_helpers.R")

# ---- Load pre-computed data ----

message("Loading lightweight app data bundle ...")

panel_data      <- readRDS("data/panel_data.rds")
school_lookup   <- readRDS("data/school_lookup.rds")
la_lookup       <- readRDS("data/la_lookup.rds")
diagnostics     <- readRDS("data/model_diagnostics_imputed.rds")
model_resid     <- readRDS("data/model_resid_data.rds")
slim_models     <- readRDS("data/slim_imputed_models.rds")

message("  Panel: ", nrow(panel_data), " school-year rows")
message("  Schools: ", nrow(school_lookup))
message("  Slim models: ", paste(names(slim_models), collapse = ", "))

# ---- Outcome configuration ----

OUTCOME_CONFIG <- list(
  all = list(
    var = "ATT8SCR",
    pred_var = "predicted_ATT8SCR_imputed",
    resid_var = "residual_ATT8SCR_imputed",
    label = "All Pupils",
    description = "Average Attainment 8 score per pupil"
  ),
  disadvantaged = list(
    var = "ATT8SCR_FSM6CLA1A",
    pred_var = "predicted_ATT8SCR_FSM6CLA1A_imputed",
    resid_var = "residual_ATT8SCR_FSM6CLA1A_imputed",
    label = "Disadvantaged Pupils",
    description = "Average Attainment 8 score per disadvantaged pupil"
  ),
  non_disadvantaged = list(
    var = "ATT8SCR_NFSM6CLA1A",
    pred_var = "predicted_ATT8SCR_NFSM6CLA1A_imputed",
    resid_var = "residual_ATT8SCR_NFSM6CLA1A_imputed",
    label = "Non-Disadvantaged Pupils",
    description = "Average Attainment 8 score per non-disadvantaged pupil"
  )
)

# ---- Pre-compute useful subsets ----

# Latest year data for each school (for default display)
latest_year <- max(panel_data$year_label, na.rm = TRUE)

latest_data <- panel_data %>%
  filter(year_label == latest_year)

# Schools with coordinates for mapping
schools_spatial <- school_lookup %>%
  filter(!is.na(easting), !is.na(northing)) %>%
  st_as_sf(
    coords = c("easting", "northing"),
    crs = 27700  # British National Grid
  ) %>%
  st_transform(crs = 4326)  # WGS84 for leaflet

# LA list for dropdowns (as character so selectInput shows names, not factor codes)
la_choices <- sort(unique(na.omit(as.character(la_lookup$LANAME))))

# School name list for autocomplete
school_choices <- school_lookup %>%
  filter(!is.na(SCHNAME)) %>%
  arrange(SCHNAME) %>%
  select(URN, SCHNAME, LANAME) %>%
  mutate(label = paste0(SCHNAME, " (", LANAME, ")"))

# Year choices
year_choices <- sort(unique(panel_data$year_label))

# Slider configuration
slider_config <- get_slider_config()

# Pre-compute England and LA averages for slider context markers
slider_var_names <- names(slider_config)

england_avgs <- panel_data %>%
  select(year_label, all_of(slider_var_names)) %>%
  pivot_longer(-year_label, names_to = "variable", values_to = "value") %>%
  filter(!is.na(value)) %>%
  group_by(year_label, variable) %>%
  summarise(mean_val = mean(value, na.rm = TRUE), .groups = "drop")

la_avgs <- panel_data %>%
  select(year_label, LANAME, all_of(slider_var_names)) %>%
  pivot_longer(-c(year_label, LANAME), names_to = "variable", values_to = "value") %>%
  filter(!is.na(value)) %>%
  group_by(year_label, LANAME, variable) %>%
  summarise(mean_val = mean(value, na.rm = TRUE), .groups = "drop")

message("  Slider context: ", nrow(england_avgs), " England avgs, ",
        nrow(la_avgs), " LA avgs")

message("App data loaded successfully.")
