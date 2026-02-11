# global.R - Load data and models at Shiny app startup
# ----------------------------------------------------
# This runs once when the app starts and makes objects
# available to all sessions.
# ----------------------------------------------------

library(tidyverse)
library(shiny)
library(bslib)
library(leaflet)
library(plotly)
library(DT)
library(sf)
library(lme4)
library(scales)

# Source prediction functions
source(here::here("R", "predict_scenario.R"))

# ---- Load pre-computed data ----

message("Loading data and models ...")

panel_data    <- readRDS(here::here("data", "panel_data.rds"))
models        <- readRDS(here::here("data", "models.rds"))
school_lookup <- readRDS(here::here("data", "school_lookup.rds"))
la_lookup     <- readRDS(here::here("data", "la_lookup.rds"))
diagnostics   <- readRDS(here::here("data", "model_diagnostics.rds"))

message("  Panel: ", nrow(panel_data), " school-year rows")
message("  Schools: ", nrow(school_lookup))
message("  Models: ", paste(names(models), collapse = ", "))

# ---- Outcome configuration ----

OUTCOME_CONFIG <- list(
  all = list(
    var = "ATT8SCR",
    pred_var = "predicted_ATT8SCR",
    resid_var = "residual_ATT8SCR",
    label = "All Pupils",
    description = "Average Attainment 8 score per pupil"
  ),
  disadvantaged = list(
    var = "ATT8SCR_FSM6CLA1A",
    pred_var = "predicted_ATT8SCR_FSM6CLA1A",
    resid_var = "residual_ATT8SCR_FSM6CLA1A",
    label = "Disadvantaged Pupils",
    description = "Average Attainment 8 score per disadvantaged pupil"
  ),
  non_disadvantaged = list(
    var = "ATT8SCR_NFSM6CLA1A",
    pred_var = "predicted_ATT8SCR_NFSM6CLA1A",
    resid_var = "residual_ATT8SCR_NFSM6CLA1A",
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

# LA list for dropdowns
la_choices <- sort(unique(na.omit(la_lookup$LANAME)))

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

message("App data loaded successfully.")
