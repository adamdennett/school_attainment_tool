# 08_la_cluster_map.R - Standalone interactive LA cluster typology map
# -------------------------------------------------------------------
# Produces a self-contained HTML map using leaflet + LA boundary data.
# Can be run standalone or sourced from the Shiny app.
#
# Depends on: la_typology.rds, la_cluster_summary.rds (from 07_la_typology.R)
# Run from project root: source("R/08_la_cluster_map.R")
# Output: output/la_cluster_map.html
# -------------------------------------------------------------------

source(here::here("R", "helpers.R"))

library(leaflet)
library(leaflet.extras)
library(sf)
library(htmlwidgets)
library(htmltools)
library(scales)
library(glue)

# ============================================================
# 1. LOAD DATA
# ============================================================

la_typology     <- readRDS(here::here("data", "la_typology.rds"))
cluster_summary <- readRDS(here::here("data", "la_cluster_summary.rds"))
cluster_meta    <- readRDS(here::here("data", "cluster_meta.rds"))
la_indicators   <- readRDS(here::here("data", "la_indicators.rds"))

message("LA typology: ", nrow(la_typology), " LAs, ", cluster_meta$chosen_k, " clusters")

# ============================================================
# 2. DOWNLOAD / LOAD LA BOUNDARY DATA
# ============================================================

# ONS LA boundaries (simplified, WGS84 GeoJSON) for England
# We use the ONS Open Geography Portal - Local Authority Districts (Dec 2023)
la_boundary_cache <- here::here("data", "cache", "la_boundaries.rds")

load_la_boundaries <- function() {
  if (file.exists(la_boundary_cache)) {
    message("Loading cached LA boundaries ...")
    return(readRDS(la_boundary_cache))
  }

  message("Downloading LA boundary data from ONS Open Geography Portal ...")

  # ONS Counties and Unitary Authorities (Dec 2023) - upper-tier geography matching LEAs
  boundary_url <- paste0(
    "https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services/",
    "Counties_and_Unitary_Authorities_December_2023_Boundaries_UK_BGC/FeatureServer/0/",
    "query?where=1%3D1&outFields=CTYUA23CD,CTYUA23NM&outSR=4326&f=geojson"
  )

  tryCatch({
    boundaries <- st_read(boundary_url, quiet = TRUE)
    message("  Downloaded: ", nrow(boundaries), " UTLA boundaries")
    saveRDS(boundaries, la_boundary_cache)
    boundaries
  }, error = function(e) {
    message("  Download failed: ", e$message)
    NULL
  })
}

boundaries <- load_la_boundaries()

# ============================================================
# 3. JOIN TYPOLOGY TO BOUNDARIES
# ============================================================

# Standardise LA names for joining (& -> and, strip punctuation)
clean_la_name <- function(x) {
  x %>%
    tolower() %>%
    str_replace_all("&", "and") %>%
    str_remove_all("[^a-z0-9 ]") %>%
    str_squish()
}

la_typology_join <- la_typology %>%
  mutate(la_name_clean = clean_la_name(LANAME))

if (!is.null(boundaries)) {
  # Detect name column
  name_col <- names(boundaries)[grepl("NM$|name", names(boundaries), ignore.case = TRUE)][1]
  code_col <- names(boundaries)[grepl("CD$|code", names(boundaries), ignore.case = TRUE)][1]

  message("  Boundary name column: ", name_col)

  boundaries_join <- boundaries %>%
    mutate(la_name_clean = clean_la_name(get(name_col)))

  # Merge
  la_map_sf <- boundaries_join %>%
    inner_join(la_typology_join, by = "la_name_clean")

  n_matched <- nrow(la_map_sf)
  n_total   <- nrow(la_typology)
  message("  Boundary join: ", n_matched, "/", n_total, " LAs matched")

  # Try fuzzy match for unmatched LAs
  if (n_matched < n_total * 0.85) {
    message("  Warning: low match rate. Checking for name differences...")
    unmatched <- setdiff(la_typology_join$la_name_clean,
                         la_map_sf$la_name_clean)
    message("  Unmatched: ", paste(head(unmatched, 5), collapse = ", "))
  }
} else {
  la_map_sf <- NULL
}

# ============================================================
# 4. COLOUR PALETTE
# ============================================================

chosen_k <- cluster_meta$chosen_k
cluster_labels <- cluster_meta$cluster_labels

# Distinct palette for clusters (colourblind-friendly)
cluster_palette <- c(
  "#1f77b4", "#ff7f0e", "#2ca02c", "#d62728",
  "#9467bd", "#8c564b", "#e377c2", "#7f7f7f",
  "#bcbd22", "#17becf", "#aec7e8", "#ffbb78"
)[1:chosen_k]

names(cluster_palette) <- as.character(1:chosen_k)

pal_cluster <- colorFactor(
  palette = cluster_palette,
  domain  = as.character(1:chosen_k)
)

# ============================================================
# 5. BUILD POPUP HTML
# ============================================================

make_popup <- function(row) {
  cl_label <- row$cluster_label
  cl_num   <- row$cluster
  cl_colour <- cluster_palette[as.character(cl_num)]

  # Cluster summary
  cs_row <- cluster_summary %>% filter(cluster == cl_num)
  portrait <- cs_row$pen_portrait[1]

  # Format values
  fmt <- function(x, digits = 1, suffix = "") {
    if (is.na(x)) "N/A" else paste0(round(x, digits), suffix)
  }
  fmt_pct <- function(x) fmt(x * 100, 1, "%")

  glue::glue('
    <div style="font-family: Arial, sans-serif; max-width: 360px;">
      <div style="background:{cl_colour}; color:white; padding:8px 12px;
                  border-radius:4px 4px 0 0; margin-bottom:0;">
        <strong style="font-size:14px;">{htmltools::htmlEscape(row$LANAME)}</strong><br>
        <span style="font-size:12px;">{htmltools::htmlEscape(cl_label)}</span>
      </div>
      <div style="border:1px solid {cl_colour}; border-top:none;
                  padding:10px 12px; border-radius:0 0 4px 4px;">
        <table style="width:100%; font-size:12px; border-collapse:collapse;">
          <tr style="border-bottom:1px solid #eee;">
            <td style="padding:3px 0; color:#555;">Region</td>
            <td style="padding:3px 0; font-weight:bold; text-align:right;">
              {htmltools::htmlEscape(row$gor_name)}</td>
          </tr>
          <tr style="border-bottom:1px solid #eee;">
            <td style="padding:3px 0; color:#555;">Mean ATT8 score</td>
            <td style="padding:3px 0; font-weight:bold; text-align:right;">
              {fmt(row$att8_mean)}</td>
          </tr>
          <tr style="border-bottom:1px solid #eee;">
            <td style="padding:3px 0; color:#555;">Disadvantage gap</td>
            <td style="padding:3px 0; font-weight:bold; text-align:right;">
              {fmt(row$att8_gap_mean)} pts</td>
          </tr>
          <tr style="border-bottom:1px solid #eee;">
            <td style="padding:3px 0; color:#555;">% Disadvantaged (FSM)</td>
            <td style="padding:3px 0; font-weight:bold; text-align:right;">
              {fmt(row$pct_fsm_mean)}%</td>
          </tr>
          <tr style="border-bottom:1px solid #eee;">
            <td style="padding:3px 0; color:#555;">% EAL pupils</td>
            <td style="padding:3px 0; font-weight:bold; text-align:right;">
              {fmt(row$pct_eal_mean)}%</td>
          </tr>
          <tr style="border-bottom:1px solid #eee;">
            <td style="padding:3px 0; color:#555;">Overall absence</td>
            <td style="padding:3px 0; font-weight:bold; text-align:right;">
              {fmt(row$abs_overall_mean)}%</td>
          </tr>
          <tr style="border-bottom:1px solid #eee;">
            <td style="padding:3px 0; color:#555;">Good/Outstanding Ofsted</td>
            <td style="padding:3px 0; font-weight:bold; text-align:right;">
              {fmt_pct(row$ofsted_good_plus_pct)}</td>
          </tr>
          <tr style="border-bottom:1px solid #eee;">
            <td style="padding:3px 0; color:#555;">Gorard segregation index</td>
            <td style="padding:3px 0; font-weight:bold; text-align:right;">
              {fmt(row$gorard_mean, 3)}</td>
          </tr>
          <tr style="border-bottom:1px solid #eee;">
            <td style="padding:3px 0; color:#555;">Teacher retention (FTE)</td>
            <td style="padding:3px 0; font-weight:bold; text-align:right;">
              {fmt(row$retention_mean)}</td>
          </tr>
          <tr style="border-bottom:1px solid #eee;">
            <td style="padding:3px 0; color:#555;">Teacher sickness (days)</td>
            <td style="padding:3px 0; font-weight:bold; text-align:right;">
              {fmt(row$sickness_mean)}</td>
          </tr>
          <tr style="border-bottom:1px solid #eee;">
            <td style="padding:3px 0; color:#555;">Pupil:teacher ratio</td>
            <td style="padding:3px 0; font-weight:bold; text-align:right;">
              {fmt(row$ptr_qual_mean, 1)}</td>
          </tr>
          <tr>
            <td style="padding:3px 0; color:#555;">No. of schools (mean)</td>
            <td style="padding:3px 0; font-weight:bold; text-align:right;">
              {fmt(row$n_schools_mean, 0)}</td>
          </tr>
        </table>
      </div>
    </div>
  ')
}

# ============================================================
# 6. BUILD LEGEND HTML
# ============================================================

build_legend <- function() {
  items <- map(1:chosen_k, function(k) {
    cs_row <- cluster_summary %>% filter(cluster == k)
    label  <- cs_row$label
    n_las  <- cs_row$n_las
    colour <- cluster_palette[as.character(k)]
    glue::glue(
      '<div style="display:flex; align-items:center; margin-bottom:6px;">',
      '<div style="width:18px; height:18px; background:{colour};',
      ' border-radius:3px; margin-right:8px; flex-shrink:0;"></div>',
      '<span style="font-size:12px;">{htmltools::htmlEscape(label)} ({n_las} LAs)</span>',
      '</div>'
    )
  })
  paste(items, collapse = "")
}

# ============================================================
# 7. BUILD LEAFLET MAP
# ============================================================

message("\nBuilding interactive leaflet map ...")

# Prepare data rows as a list for popup generation
if (!is.null(la_map_sf)) {
  la_map_sf <- la_map_sf %>%
    mutate(cluster_chr = as.character(cluster))

  popups <- map_chr(seq_len(nrow(la_map_sf)), function(i) {
    make_popup(as.list(st_drop_geometry(la_map_sf[i, ])))
  })

  map_obj <- leaflet(la_map_sf) %>%
    addProviderTiles(
      providers$CartoDB.Positron,
      options = tileOptions(minZoom = 5, maxZoom = 14)
    ) %>%
    setView(lng = -1.5, lat = 52.8, zoom = 6) %>%

    # Cluster polygons
    addPolygons(
      fillColor   = ~pal_cluster(cluster_chr),
      fillOpacity = 0.75,
      color       = "white",
      weight      = 1.2,
      opacity     = 0.8,
      highlightOptions = highlightOptions(
        weight      = 3,
        color       = "#333",
        fillOpacity = 0.9,
        bringToFront = TRUE
      ),
      popup       = popups,
      label       = ~paste0(LANAME, ": ", cluster_label),
      labelOptions = labelOptions(
        style     = list("font-weight" = "bold", "font-size" = "13px"),
        textsize  = "13px",
        direction = "auto"
      ),
      group = "Cluster boundaries"
    ) %>%

    # Custom legend
    addControl(
      html = paste0(
        '<div style="background:white; padding:12px 16px; border-radius:6px;',
        ' box-shadow:0 1px 6px rgba(0,0,0,0.3); max-width:280px;">',
        '<strong style="font-size:13px; display:block; margin-bottom:8px;">',
        'LA Typology (', chosen_k, ' clusters)</strong>',
        build_legend(),
        '<div style="font-size:10px; color:#888; margin-top:8px;">',
        'Based on k-means clustering of ', cluster_meta$n_vars_used,
        ' school-level indicators<br>',
        'Variance explained: ', cluster_meta$variance_explained, '%</div>',
        '</div>'
      ),
      position = "topright"
    ) %>%

    # Scale bar
    addScaleBar(position = "bottomleft") %>%

    # Layer control
    addLayersControl(
      overlayGroups = "Cluster boundaries",
      options = layersControlOptions(collapsed = FALSE)
    )

} else {
  # Fallback: use centroid points if no boundary data
  message("  Using point markers (no boundary data available)")

  # Approximate centroids for England LAs
  la_map_sf <- la_typology %>%
    mutate(cluster_chr = as.character(cluster))

  map_obj <- leaflet() %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    setView(lng = -1.5, lat = 52.8, zoom = 6) %>%
    addControl(
      html = '<div style="background:white; padding:10px; border-radius:4px;">
              <strong>Note:</strong> LA boundaries unavailable. Data loaded.</div>',
      position = "topright"
    )
}

# ============================================================
# 8. SAVE HTML
# ============================================================

output_dir <- here::here("output")
dir.create(output_dir, showWarnings = FALSE)

out_path <- file.path(output_dir, "la_cluster_map.html")

saveWidget(
  map_obj,
  file          = out_path,
  selfcontained = TRUE,
  title         = "LA School Typology - Interactive Map"
)

message("\nSaved interactive map: ", out_path)
message("Open in browser: file://", gsub("\\\\", "/", out_path))

invisible(map_obj)
