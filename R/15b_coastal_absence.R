# 15b_coastal_absence.R - Is the illness excess a coastal thing?
# ---------------------------------------------------------------------------
# The authorities with the largest excess of illness-coded absence after
# disadvantage look coastal, and TEP's census reports weaker belonging and a
# steeper fall in engagement after Year 7 in coastal secondaries. This tests
# both against published absence data.
#
# Coastal is measured, not listed. The coastline is the outer edge of the
# English upper-tier authorities, with estuaries under 10km wide closed and
# the Welsh and Scottish land borders removed. A school is coastal within
# 10km of it, and an authority when at least half its sessions are in such
# schools. Closing estuaries leaves Southampton inland, and counties with a
# long coast and a large interior, such as Devon, fall below the threshold.
#
# Writes data/coastal_absence.rds, read by exclusions_and_absence.qmd.
#
# Run from project root.
# ---------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(sf); library(sandwich); library(lmtest)
})
source(here::here("R", "15_exclusions.R"))

COAST_KM <- 10
COAST_SHARE <- 0.5
ESTUARY_CLOSE_M <- 5000


#' England's coastline, in British National Grid
england_coast <- function() {
  old <- sf_use_s2(FALSE)
  on.exit(sf_use_s2(old))
  b <- readRDS(here::here("data", "cache", "la_boundaries.rds")) |>
    filter(startsWith(CTYUA23CD, "E")) |>
    st_transform(27700)
  land <- st_union(st_simplify(st_make_valid(b), dTolerance = 100))
  closed <- st_buffer(st_buffer(land, ESTUARY_CLOSE_M), -ESTUARY_CLOSE_M)
  parts <- st_cast(closed, "POLYGON")
  parts <- parts[as.numeric(st_area(parts)) > 5e7]
  edge <- st_sfc(st_multilinestring(lapply(parts, function(p) st_linestring(p[[1]]))),
                 crs = 27700)
  box <- function(x0, y0, x1, y1) st_transform(
    st_as_sfc(st_bbox(c(xmin = x0, ymin = y0, xmax = x1, ymax = y1), crs = 4326)), 27700)
  borders <- c(box(-3.35, 51.62, -2.35, 53.22),   # Wales
               box(-3.25, 54.97, -2.00, 55.82))   # Scotland
  st_difference(edge, st_union(borders))
}


#' Every secondary school-year, with intake and distance from the sea
school_frame <- function() {
  panel <- readRDS(here::here("data", "panel_data.rds")) |>
    filter(MINORGROUP %in% c("Academy", "Maintained school")) |>
    filter(year_label == max(year_label)) |>
    transmute(urn = as.character(URN), fsm = PTFSM6CLA1A,
              eal = as.numeric(PNUMEAL),
              sen = as.numeric(PSENE4) + as.numeric(PSENK4),
              gor = gor_name, easting = as.numeric(easting),
              northing = as.numeric(northing))
  gias <- readRDS(here::here("data", "gias", "gias_establishments.rds")) |>
    transmute(urn = as.character(urn), e2 = as.numeric(easting), n2 = as.numeric(northing))

  x <- read_absence() |>
    transmute(tp = time_period, urn, la_name, poss, overall,
              illness = 100 * sess_auth_illness / poss,
              unauth  = 100 * sess_unauth_other / poss,
              total   = 100 * overall / poss) |>
    left_join(panel, by = "urn") |>
    left_join(gias, by = "urn") |>
    mutate(easting = coalesce(easting, e2), northing = coalesce(northing, n2))

  loc <- x |> filter(!is.na(easting)) |> distinct(urn, easting, northing)
  pts <- st_as_sf(loc, coords = c("easting", "northing"), crs = 27700)
  loc$km <- as.numeric(st_distance(pts, england_coast())) / 1000
  x |> left_join(loc |> select(urn, km), by = "urn") |> filter(!is.na(km))
}


#' One row of a results table, with standard errors clustered on authority
#' where the rows are schools or authority-years
fit_row <- function(formula, d, label, term = "coastTRUE", cluster = TRUE) {
  d <- d |> filter(if_all(all_of(all.vars(formula)), ~ !is.na(.x)))
  m <- lm(formula, d)
  v <- if (cluster) vcovCL(m, cluster = d$la_name) else vcovHC(m, "HC1")
  r <- coeftest(m, v)[term, ]
  tibble(label = label, est = r[[1]], se = r[[2]], p = r[[4]], n = nobs(m))
}


build_coastal <- function(path = here::here("data", "coastal_absence.rds")) {
  x <- school_frame()
  latest <- max(x$tp)
  y <- x |> filter(tp == latest)

  la_share <- y |> group_by(la_name) |>
    summarise(share = sum(poss[km <= COAST_KM]) / sum(poss), .groups = "drop")
  ic <- readRDS(here::here("data", "exclusions_absence.rds"))$illness_ctx |>
    mutate(expected = as.numeric(expected), resid = as.numeric(resid)) |>
    left_join(la_share, by = "la_name") |>
    mutate(coast = share >= COAST_SHARE, bh = grepl("Brighton and Hove", la_name))

  la_models <- bind_rows(
    fit_row(illness ~ coast + fsm, ic, "Illness", cluster = FALSE),
    fit_row(unauth_other ~ coast + fsm, ic, "Unauthorised, other", cluster = FALSE),
    fit_row(total ~ coast + fsm, ic, "All absence", cluster = FALSE),
    fit_row(illness ~ coast + fsm, ic |> filter(!bh), "Illness, without B&H", cluster = FALSE),
    fit_row(illness ~ share + fsm, ic, "Illness, continuous share", term = "share", cluster = FALSE))

  fit <- lm(illness ~ coast + fsm, ic)
  ic$resid_coast <- resid(fit)

  s <- y |> filter(!is.na(fsm), !is.na(eal), !is.na(sen)) |> mutate(coast = km <= COAST_KM)
  specs <- c("Disadvantage only" = "coast + fsm",
             "+ EAL and SEN" = "coast + fsm + eal + sen",
             "+ region" = "coast + fsm + eal + sen + gor")
  fit_outcome <- function(o) bind_rows(
    fit_row(as.formula(paste(o, "~ coast")), s, "Raw"),
    lapply(names(specs), function(k) fit_row(as.formula(paste(o, "~", specs[[k]])), s, k)))
  school_models <- bind_rows(
    lapply(c(Illness = "illness", Unauthorised = "unauth", All = "total"), fit_outcome),
    .id = "outcome") |>
    mutate(n_coastal = sum(s$coast))

  by_year <- bind_rows(lapply(sort(unique(x$tp)), function(t) {
    d <- x |> filter(tp == t, !is.na(fsm), !is.na(eal), !is.na(sen)) |>
      mutate(coast = km <= COAST_KM)
    fit_row(illness ~ coast + fsm + eal + sen + gor, d, t)
  }))

  # Brighton & Hove's schools against the same model with no coastal term.
  m0 <- lm(illness ~ fsm + eal + sen + gor, s)
  s$r <- resid(m0)
  school_res <- s |> group_by(la_name) |>
    summarise(n = n(), r = weighted.mean(r, poss), .groups = "drop") |>
    filter(n >= 5) |> mutate(rank = rank(-r))

  # Year groups: the same children followed across one year, so nothing here
  # is a comparison between different cohorts. The series stops at 2024/25.
  intake <- s |> group_by(la_name) |>
    summarise(gor = first(gor), eal = weighted.mean(eal, poss),
              sen = weighted.mean(sen, poss), .groups = "drop")
  a <- readRDS(here::here("data", "engagement", "absence_by_yeargroup.rds")) |>
    filter(geographic_level == "Local authority",
           (education_phase == "State-funded secondary" & year_group %in% paste("Year", 7:8)) |
             (education_phase == "State-funded primary" & year_group == "Year 6")) |>
    transmute(tp = time_period, la_name,
              yg = paste0("y", sub("Year ", "", year_group)), rate = overall_absence_rate) |>
    pivot_wider(names_from = yg, values_from = rate) |>
    left_join(ic |> select(la_name, coast, fsm), by = "la_name") |>
    left_join(intake, by = "la_name") |>
    filter(!is.na(coast), !is.na(gor))
  cohort <- a |> filter(tp %in% c(202223, 202324)) |>
    transmute(t1 = tp + 101, la_name, coast, fsm, eal, sen, gor, y6_a = y6, y7_a = y7) |>
    inner_join(a |> transmute(t1 = tp, la_name, y7_b = y7, y8_b = y8),
               by = c("t1", "la_name")) |>
    mutate(step67 = y7_b - y6_a, step78 = y8_b - y7_a,
           pct78 = 100 * log(y8_b / y7_a))
  cohort_models <- bind_rows(
    fit_row(step78 ~ coast + fsm + factor(t1), cohort, "Year 7 to 8: disadvantage only"),
    fit_row(step78 ~ coast + fsm + eal + sen + gor + factor(t1), cohort,
            "Year 7 to 8: + EAL, SEN and region"),
    fit_row(step78 ~ coast + fsm + eal + sen + gor + factor(t1),
            cohort |> filter(!grepl("Brighton and Hove", la_name)),
            "Year 7 to 8: as above, without B&H"),
    fit_row(pct78 ~ coast + fsm + eal + sen + gor + factor(t1), cohort,
            "Year 7 to 8 as a percentage: + EAL, SEN and region"),
    fit_row(step67 ~ coast + fsm + factor(t1), cohort, "Year 6 to 7: disadvantage only"),
    fit_row(step67 ~ coast + fsm + eal + sen + gor + factor(t1), cohort,
            "Year 6 to 7: + EAL, SEN and region"))
  y6_ranked <- a |> filter(tp == max(tp)) |> mutate(rank6 = rank(-y6, na.last = "keep"))

  out <- list(
    school_km = y |> distinct(urn, km),
    la = ic, la_models = la_models, school_models = school_models, by_year = by_year,
    school_res = school_res, cohort_models = cohort_models,
    cohort_means = cohort |> group_by(coast) |>
      summarise(step67 = mean(step67, na.rm = TRUE), step78 = mean(step78, na.rm = TRUE),
                .groups = "drop"),
    cohort_bh = cohort |> filter(grepl("Brighton and Hove", la_name)) |>
      select(t1, step67, step78),
    y6_bh = y6_ranked |> filter(grepl("Brighton and Hove", la_name)) |> select(y6, rank6),
    latest = latest, built_at = Sys.time())
  saveRDS(out, path)
  message("Wrote ", path)
  invisible(out)
}


if (sys.nframe() == 0) build_coastal()
