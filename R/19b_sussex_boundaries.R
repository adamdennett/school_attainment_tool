# 19b_sussex_boundaries.R - District boundaries for the Sussex documents
# ---------------------------------------------------------------------------
# The 13 local authority districts of East Sussex, West Sussex and
# Brighton and Hove, from the ONS Open Geography Portal (Local Authority
# Districts, December 2024, ultra-generalised clipped boundaries; Open
# Government Licence v3.0, contains OS data (c) Crown copyright and
# database right 2024). Ultra-generalised is plenty for an interactive map
# at county scale and keeps the self-contained pages small.
#
# Writes data/sussex_lads.rds (sf, WGS84).
#
# Run from project root.
# ---------------------------------------------------------------------------

suppressPackageStartupMessages({library(sf); library(dplyr)})

OUT <- here::here("data", "sussex_lads.rds")
SERVICE <- paste0("https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services/",
                  "Local_Authority_Districts_December_2024_Boundaries_UK_BUC/FeatureServer/0/query")

SUSSEX_LADS <- c(
  # East Sussex
  "Eastbourne" = "East Sussex", "Hastings" = "East Sussex", "Lewes" = "East Sussex",
  "Rother" = "East Sussex", "Wealden" = "East Sussex",
  # West Sussex
  "Adur" = "West Sussex", "Arun" = "West Sussex", "Chichester" = "West Sussex",
  "Crawley" = "West Sussex", "Horsham" = "West Sussex", "Mid Sussex" = "West Sussex",
  "Worthing" = "West Sussex",
  # Unitary
  "Brighton and Hove" = "Brighton and Hove")

build_sussex_lads <- function() {
  where <- sprintf("LAD24NM IN (%s)", paste0("'", names(SUSSEX_LADS), "'", collapse = ","))
  url <- paste0(SERVICE, "?where=", utils::URLencode(where, reserved = TRUE),
                "&outFields=LAD24CD,LAD24NM&outSR=4326&f=geojson")
  lads <- st_read(url, quiet = TRUE) |>
    transmute(lad_cd = LAD24CD, lad = LAD24NM, la = unname(SUSSEX_LADS[LAD24NM]))
  stopifnot(nrow(lads) == length(SUSSEX_LADS), !anyNA(lads$la))
  saveRDS(lads, OUT)
  message("Wrote ", OUT, " (", nrow(lads), " districts)")
  invisible(lads)
}

if (sys.nframe() == 0) build_sussex_lads()
