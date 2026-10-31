# 20_district_reports.R - One document per district, from one template
# ---------------------------------------------------------------------------
# Writes district_<slug>.qmd from templates/district_report.qmd for every
# district in R/area_tiers.R (except the hand-written ones), renders it,
# makes it mobile-safe and copies it to docs/. Then renders the summary of
# all districts, sussex_districts.qmd. A change to the template is a
# change to every district: edit it and run this again.
#
#   Rscript R/20_district_reports.R              # every district + summary
#   Rscript R/20_district_reports.R hastings     # just these
#
# Needs data/area_<slug>.rds for each district (R/17_area_analysis.R) and
# quarto on the PATH. The generated .qmd files are not committed: the
# template is the source.
# ---------------------------------------------------------------------------

source(here::here("R", "area_tiers.R"))
source(here::here("R", "16_mobile_safe_html.R"))

TEMPLATE <- here::here("templates", "district_report.qmd")

render_one <- function(qmd) {
  status <- system2("quarto", c("render", shQuote(qmd)), stdout = FALSE, stderr = "")
  if (status != 0) stop("quarto render failed for ", qmd)
  html <- sub("\\.qmd$", ".html", qmd)
  # a template copy names its output district_<slug>.html
  out <- yaml_output(qmd)
  if (!is.null(out)) html <- here::here(paste0(out, ".html"))
  mobile_safe(html)
  file.copy(html, here::here("docs", basename(html)), overwrite = TRUE)
  message("  ", basename(html))
}

yaml_output <- function(qmd) {
  l <- grep("^\\s*output-file:", readLines(qmd, n = 40), value = TRUE)
  if (length(l)) trimws(sub("^\\s*output-file:", "", l[1])) else NULL
}

write_district <- function(district, la) {
  slug <- slugify(district)
  if (!file.exists(here::here("data", paste0("area_", slug, ".rds"))))
    stop("No data/area_", slug, ".rds: run R/17_area_analysis.R ", slug)
  txt <- readLines(TEMPLATE, encoding = "UTF-8")
  txt <- gsub("@@DISTRICT@@", district, txt, fixed = TRUE)
  txt <- gsub("@@COUNTY@@", la, txt, fixed = TRUE)
  txt <- gsub("@@SLUG@@", slug, txt, fixed = TRUE)
  f <- here::here(paste0("district_", slug, ".qmd"))
  writeLines(txt, f, useBytes = TRUE)
  f
}

district_reports <- function(slugs = NULL, summary = TRUE) {
  todo <- do.call(rbind, lapply(names(DISTRICTS), function(la)
    data.frame(district = DISTRICTS[[la]], la = la)))
  todo <- todo[!todo$district %in% HAND_WRITTEN, ]
  if (length(slugs)) todo <- todo[slugify(todo$district) %in% slugs, ]
  for (i in seq_len(nrow(todo))) {
    message(todo$district[i])
    render_one(write_district(todo$district[i], todo$la[i]))
  }
  if (summary) {
    message("District summary")
    render_one(here::here("sussex_districts.qmd"))
  }
  invisible(TRUE)
}

if (sys.nframe() == 0) {
  args <- commandArgs(trailingOnly = TRUE)
  district_reports(if (length(args)) args else NULL, summary = !length(args))
}
