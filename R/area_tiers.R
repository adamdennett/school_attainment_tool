# area_tiers.R - Which areas get a document, at which tier
# ---------------------------------------------------------------------------
# The Sussex documents come in three tiers (AREA_REPORTS.md):
#   top    Sussex as a whole                       sussex_review.qmd
#   upper  the authorities that run schools        east_sussex.qmd, west_sussex.qmd
#          (Brighton and Hove is covered by the Lever report itself)
#   lower  the districts of the two counties       templates/district_report.qmd,
#                                                  one copy per district, plus
#                                                  sussex_districts.qmd as a summary
# R/17_area_analysis.R builds a data file for each area named here, and
# R/20_district_reports.R writes and renders a document for each district.
# To cover another county, add it to DISTRICTS (and its boundaries to
# R/19b_sussex_boundaries.R).
# ---------------------------------------------------------------------------

DISTRICTS <- list(
  "East Sussex" = c("Eastbourne", "Hastings", "Lewes", "Rother", "Wealden"),
  "West Sussex" = c("Adur", "Arun", "Chichester", "Crawley", "Horsham", "Mid Sussex", "Worthing"))

# Districts with a hand-written document instead of the template's
HAND_WRITTEN <- c("Mid Sussex")

slugify <- function(x) gsub("[^a-z]+", "_", tolower(x))
