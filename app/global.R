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
library(stringr)
library(purrr)
library(glue)
library(cluster)   # Gower distance for school twin matching

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


# ============================================================
# ---- LA Typology data (from 07_la_typology.R) ----
# ============================================================

message("Loading LA typology data ...")

# Core typology: one row per LA with cluster assignment and key indicators
la_typology_path <- "data/la_typology.rds"
la_typology <- if (file.exists(la_typology_path)) {
  readRDS(la_typology_path)
} else {
  message("  WARNING: la_typology.rds not found. Run R/07_la_typology.R first.")
  NULL
}

# Full indicator table
la_indicators_path <- "data/la_indicators.rds"
la_indicators_data <- if (file.exists(la_indicators_path)) {
  readRDS(la_indicators_path)
} else {
  message("  WARNING: la_indicators.rds not found.")
  NULL
}

# Cluster summary (pen portraits, labels, profiles)
cluster_summary_path <- "data/la_cluster_summary.rds"
la_cluster_summary <- if (file.exists(cluster_summary_path)) {
  readRDS(cluster_summary_path)
} else {
  message("  WARNING: la_cluster_summary.rds not found.")
  NULL
}

# Cluster metadata (k, sizes, labels, colours)
cluster_meta_path <- "data/cluster_meta.rds"
cluster_meta_data <- if (file.exists(cluster_meta_path)) {
  cm <- readRDS(cluster_meta_path)

  # Attach a consistent colour palette
  palette_cols <- c(
    "#1f77b4", "#ff7f0e", "#2ca02c", "#d62728",
    "#9467bd", "#8c564b", "#e377c2", "#7f7f7f",
    "#bcbd22", "#17becf", "#aec7e8", "#ffbb78"
  )
  cm$cluster_colours <- palette_cols[seq_len(cm$chosen_k)]
  cm
} else {
  message("  WARNING: cluster_meta.rds not found.")
  NULL
}

# ---- LA boundary polygons (for choropleth map) ----
la_boundary_cache <- "data/la_boundaries.rds"   # lives in app/data/ alongside other app data

load_la_boundaries_app <- function() {
  if (file.exists(la_boundary_cache)) {
    message("  Loading cached LA boundaries ...")
    boundaries <- readRDS(la_boundary_cache)
    # If the cache is unsimplified (>2MB), simplify now and re-cache
    cache_mb <- file.info(la_boundary_cache)$size / 1024^2
    if (cache_mb > 2) {
      message(sprintf("  Cached boundaries are large (%.1f MB) — simplifying ...", cache_mb))
      tryCatch({
        if (requireNamespace("rmapshaper", quietly = TRUE)) {
          boundaries <- rmapshaper::ms_simplify(boundaries, keep = 0.05,
                                                 keep_shapes = TRUE)
        } else {
          b_proj <- sf::st_transform(boundaries, 27700)
          b_proj <- sf::st_simplify(b_proj, dTolerance = 200,
                                     preserveTopology = TRUE)
          boundaries <- sf::st_transform(b_proj, 4326)
          if (any(!sf::st_is_valid(boundaries)))
            boundaries <- sf::st_make_valid(boundaries)
        }
        saveRDS(boundaries, la_boundary_cache)
        message(sprintf("  Re-cached simplified boundaries (%.1f MB)",
                        file.info(la_boundary_cache)$size / 1024^2))
      }, error = function(e) {
        message("  Simplification failed (using full-res): ", e$message)
      })
    }
  } else {
    message("  Downloading LA boundary data from ONS ...")
    # Counties and Unitary Authorities (upper-tier) — matches education LEA geography
    boundary_url <- paste0(
      "https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services/",
      "Counties_and_Unitary_Authorities_December_2023_Boundaries_UK_BGC/FeatureServer/0/",
      "query?where=1%3D1&outFields=CTYUA23CD,CTYUA23NM&outSR=4326&f=geojson"
    )
    boundaries <- tryCatch(
      st_read(boundary_url, quiet = TRUE),
      error = function(e) {
        message("  Boundary download failed: ", e$message)
        NULL
      }
    )
    if (!is.null(boundaries)) {
      # Simplify before caching — ~467K coords → ~25K for fast map rendering
      tryCatch({
        if (requireNamespace("rmapshaper", quietly = TRUE)) {
          boundaries <- rmapshaper::ms_simplify(boundaries, keep = 0.05,
                                                 keep_shapes = TRUE)
          message("  Pre-simplified boundaries (rmapshaper, keep = 5%)")
        } else {
          b_proj <- sf::st_transform(boundaries, 27700)
          b_proj <- sf::st_simplify(b_proj, dTolerance = 200,
                                     preserveTopology = TRUE)
          boundaries <- sf::st_transform(b_proj, 4326)
          if (any(!sf::st_is_valid(boundaries)))
            boundaries <- sf::st_make_valid(boundaries)
          message("  Pre-simplified boundaries (st_simplify, 200m)")
        }
      }, error = function(e) {
        message("  Pre-simplification skipped: ", e$message)
      })
      saveRDS(boundaries, la_boundary_cache)
    }
  }

  if (is.null(boundaries) || is.null(la_typology)) return(NULL)

  # Identify name column (CTYUA23NM for UTLA, LAD23NM for LAD fallback)
  name_col <- names(boundaries)[grepl("NM$|name", names(boundaries), ignore.case = TRUE)][1]
  message("  Using boundary name column: ", name_col)

  clean_name <- function(x) {
    x %>%
      tolower() %>%
      str_replace_all("&", "and") %>%
      str_remove_all("[^a-z0-9 ]") %>%
      str_squish()
  }

  boundaries_join <- boundaries %>%
    mutate(la_name_clean = clean_name(get(name_col)))

  typology_join <- la_typology %>%
    mutate(la_name_clean = clean_name(LANAME))

  joined <- boundaries_join %>%
    inner_join(typology_join, by = "la_name_clean")

  message("  Boundaries joined: ", nrow(joined), "/", nrow(la_typology), " LAs matched")

  joined
}

la_boundaries_sf <- load_la_boundaries_app()

if (!is.null(la_boundaries_sf)) {
  message("  LA boundaries ready: ", nrow(la_boundaries_sf), " polygons")
} else {
  message("  LA boundaries unavailable — typology map will be limited")
}
