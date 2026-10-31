# 17_area_analysis.R - Any Sussex area through the national model
# ---------------------------------------------------------------------------
# East Sussex County Council asked what its secondary school system looks
# like through the lens of How to Pull the Right Lever. The same questions
# were then asked of West Sussex, of Mid Sussex (a district within West
# Sussex), and of Sussex as a whole. This script assembles every number
# those documents report, for any area defined as a set of local
# authorities and, optionally, districts within them. The documents only
# read and draw.
#
# Nothing is refitted that the published report fitted. The national
# models are the published ones (data/models_imputed.rds); the value-added
# measure is the published residual extended to 2025/26 exactly as
# R/05b_value_added_2526.R does it; the with/without-absence LA rankings
# are the ones revisions/r27_absence_control.R produced for the peer
# review. Two models ARE fitted here, both copies of the report's own:
# stage 1 of the absence decomposition (expected absence from intake) and
# the M2 attainment model without an absence control, which together give
# the joint-signal plane in section 4.4 of the report.
#
# An AREA is
#   name    what the documents call it
#   las     the local authorities its schools are in
#   lads    NULL, or the districts within those LAs it is restricted to
#   comps   other areas (same shape) it is compared with, side by side
#
# A district-level area cannot use anything the published model estimates
# for local authorities (the LA effect, the with/without-absence ranks):
# those belong to its parent authority, and are reported as such.
#
# Every source is published and under the Open Government Licence v3.0.
#
# Writes data/area_<slug>.rds.
#
# Run from project root:   Rscript R/17_area_analysis.R [slug ...]
# ---------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(readr); library(lme4)
})
source(here::here("R", "00b_download_ees_api.R"))
source(here::here("R", "05b_value_added_2526.R"))

ctrl <- lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 20000))

area <- function(name, las, lads = NULL) list(name = name, las = las, lads = lads)

AREAS <- list(
  east_sussex = list(
    area = area("East Sussex", "East Sussex"),
    comps = list(area("Brighton and Hove", "Brighton and Hove")),
    coastal_towns = c("Hastings", "St Leonards-on-Sea", "Bexhill-on-Sea", "Eastbourne",
                      "Seaford", "Newhaven", "Peacehaven", "Rye")),
  west_sussex = list(
    area = area("West Sussex", "West Sussex"),
    comps = list(area("East Sussex", "East Sussex"), area("Brighton and Hove", "Brighton and Hove")),
    coastal_towns = c("Worthing", "Littlehampton", "Bognor Regis", "Shoreham-by-Sea",
                      "Lancing", "Selsey", "Rustington", "Angmering", "Felpham")),
  mid_sussex = list(
    area = area("Mid Sussex", "West Sussex", "Mid Sussex"),
    comps = list(area("West Sussex", "West Sussex"), area("Brighton and Hove", "Brighton and Hove")),
    coastal_towns = character(0)),
  sussex = list(
    area = area("Sussex", c("East Sussex", "West Sussex", "Brighton and Hove")),
    comps = list(area("East Sussex", "East Sussex"), area("West Sussex", "West Sussex"),
                 area("Brighton and Hove", "Brighton and Hove")),
    coastal_towns = c("Hastings", "St Leonards-on-Sea", "Bexhill-on-Sea", "Eastbourne",
                      "Seaford", "Newhaven", "Peacehaven", "Rye", "Worthing", "Littlehampton",
                      "Bognor Regis", "Shoreham-by-Sea", "Lancing", "Selsey", "Brighton", "Hove")),
  brighton_and_hove = list(
    area = area("Brighton and Hove", "Brighton and Hove"),
    comps = list(area("East Sussex", "East Sussex"), area("West Sussex", "West Sussex")),
    coastal_towns = character(0)))

# The lower tier: every district of the two counties (R/area_tiers.R),
# each set against its county and Brighton and Hove. Mid Sussex is
# defined above and keeps its own entry.
source(here::here("R", "area_tiers.R"))
for (la in names(DISTRICTS)) for (d in DISTRICTS[[la]]) {
  if (slugify(d) %in% names(AREAS)) next
  AREAS[[slugify(d)]] <- list(
    area = area(d, la, d),
    comps = list(area(la, la), area("Brighton and Hove", "Brighton and Hove")),
    coastal_towns = AREAS[[slugify(la)]]$coastal_towns)
}


# ---- 1. The model dataset, exactly as the published report builds it ----
model_data <- function(panel) {
  d <- panel |>
    filter(MINORGROUP %in% c("Academy", "Maintained school")) |>
    filter(!is.na(ATT8SCR), ATT8SCR > 0,
           PTFSM6CLA1A > 0, PERCTOT > 0, PNUMEAL > 0,
           !is.na(OFSTEDRATING_1), !is.na(gor_name), !is.na(LANAME)) |>
    filter(!is.na(remained_in_the_same_school),
           !is.na(teachers_on_leadership_pay_range_percent),
           average_number_of_days_taken > 0,
           !is.na(gorard_segregation)) |>
    mutate(OFSTEDRATING_1 = factor(OFSTEDRATING_1,
                                   levels = c("Outstanding", "Good",
                                              "Requires Improvement", "Inadequate"),
                                   ordered = TRUE),
           gor_name   = factor(gor_name),
           LANAME     = factor(LANAME),
           year_label = factor(year_label)) |>
    droplevels()
  contrasts(d$OFSTEDRATING_1) <- contr.treatment(levels(d$OFSTEDRATING_1))
  d
}


# ---- 2. Which schools are in an area ------------------------------------
# District comes from each school's postcode (R/18_district_effects.R
# builds and caches the lookup); the cache covers every school in the model.
school_lad <- function(panel) {
  f <- here::here("data", "cache", "school_lad.rds")
  if (!file.exists(f)) stop("Run R/18_district_effects.R first: it builds ", f)
  m <- readRDS(f) |> transmute(URN = as.integer(URN), lad)
  # Schools in the Sussex authorities that never made it into the model's
  # rows (a newer school, a missing predictor) are placed from their
  # postcode here, so no district summary silently drops one
  need <- panel |>
    filter(LANAME %in% c("East Sussex", "West Sussex", "Brighton and Hove"),
           MINORGROUP %in% c("Academy", "Maintained school")) |>
    distinct(URN = as.integer(URN), POSTCODE) |>
    filter(!URN %in% m$URN, !is.na(POSTCODE)) |>
    group_by(URN) |> slice(1) |> ungroup()
  if (nrow(need)) {
    r <- httr::POST("https://api.postcodes.io/postcodes",
                    body = list(postcodes = as.list(toupper(trimws(need$POSTCODE)))),
                    encode = "json")
    res <- jsonlite::fromJSON(httr::content(r, "text", encoding = "UTF-8"),
                              simplifyVector = FALSE)$result
    lad <- vapply(res, function(x) if (is.null(x$result)) NA_character_ else
      x$result$admin_district %||% NA_character_, character(1))
    m <- bind_rows(m, tibble(URN = need$URN, lad = lad))
    message("  placed ", sum(!is.na(lad)), " of ", nrow(need), " further Sussex schools by postcode")
  }
  m
}
`%||%` <- function(a, b) if (is.null(a)) b else a

#' URNs in an area, every year: the panel's own, plus each one's
#' predecessors, so absence and exclusion series run across conversions
area_urns <- function(panel, a, lad_map) {
  u <- panel |>
    filter(LANAME %in% a$las, MINORGROUP %in% c("Academy", "Maintained school")) |>
    distinct(URN = as.integer(URN)) |>
    left_join(lad_map, by = "URN")
  if (!is.null(a$lads)) u <- u |> filter(lad %in% a$lads)
  u$URN
}

#' Every URN each school has filed returns under: its own, its
#' predecessors and its successors (a fresh start or re-conversion after
#' the last results year), a few generations each way. One row per
#' (URN = the area's school, urn = a URN it filed under). A URN that is
#' itself one of the area's schools stays with that school, so no return
#' is counted twice.
urn_links <- function(urns) {
  self <- tibble(URN = urns, urn = urns)
  f <- list.files(here::here("data-raw", "gias"), "^links_edubasealldata.*csv$", full.names = TRUE)
  if (!length(f)) return(self)
  l <- read_csv(tail(sort(f), 1), locale = locale(encoding = "latin1"),
                col_types = cols(.default = col_character()), progress = FALSE) |>
    transmute(from = as.integer(URN), link = as.integer(LinkURN), type = LinkType) |>
    filter(grepl("Predecessor|Successor", type))
  out <- self
  for (i in 1:4) {
    nxt <- out |> inner_join(l, by = c("urn" = "from"), relationship = "many-to-many") |>
      transmute(URN, urn = link) |>
      anti_join(out, by = c("URN", "urn"))
    if (!nrow(nxt)) break
    out <- bind_rows(out, nxt)
  }
  out |>
    filter(!(urn %in% urns & URN != urn)) |>
    distinct(urn, .keep_all = TRUE)
}

urn_family <- function(urns) unique(urn_links(urns)$urn)

school_table <- function(panel, a, coastal_towns, lad_map) {
  s <- panel |>
    filter(LANAME %in% a$las, MINORGROUP %in% c("Academy", "Maintained school"),
           year_label == max(year_label)) |>
    mutate(URN = as.integer(URN)) |>
    left_join(lad_map, by = "URN") |>
    rename(district = lad)
  if (!is.null(a$lads)) s <- s |> filter(district %in% a$lads)
  if (anyNA(s$district)) stop("No district for: ", paste(s$SCHNAME[is.na(s$district)], collapse = ", "))
  s <- s |> mutate(coastal = tolower(TOWN) %in% tolower(coastal_towns))

  # Some schools carry no grid reference in the 2024/25 performance tables;
  # Get Information about Schools has every one
  gias <- readRDS(here::here("data", "gias", "gias_establishments.rds")) |>
    transmute(URN = as.integer(urn), e_g = as.numeric(easting), n_g = as.numeric(northing))
  s <- s |>
    left_join(gias, by = "URN") |>
    mutate(easting = coalesce(as.numeric(easting), e_g),
           northing = coalesce(as.numeric(northing), n_g)) |>
    select(-e_g, -n_g)
  if (any(is.na(s$easting))) stop("No location for: ", paste(s$SCHNAME[is.na(s$easting)], collapse = ", "))

  ll <- sf::st_as_sf(s |> select(URN, easting, northing),
                     coords = c("easting", "northing"), crs = 27700) |>
    sf::st_transform(4326)
  xy <- sf::st_coordinates(ll)
  s |>
    left_join(tibble(URN = ll$URN, lon = xy[, 1], lat = xy[, 2]), by = "URN") |>
    transmute(URN = as.character(URN), SCHNAME, TOWN, LANAME = as.character(LANAME),
              district, coastal, lon, lat,
              type = MINORGROUP, religion = RELCHAR, ofsted = as.character(OFSTEDRATING_1),
              admissions = ADMPOL_PT,
              pupils = TOTPUPS, ks4_cohort = TPUP,
              att8 = ATT8SCR, att8_dis = ATT8SCR_FSM6CLA1A, att8_non = ATT8SCR_NFSM6CLA1A,
              p8 = P8MEA, fsm = PTFSM6CLA1A, absence = PERCTOT, persistent = PPERSABS10,
              eal = PNUMEAL, ks2_c, sen_ehcp = PSENELSE, sen_support = PSENELK)
}


# ---- 3. Profiles: every LA, and the area itself if it is not an LA ------
profile_rows <- function(d) {
  d |>
    summarise(schools = n(),
              att8     = mean(ATT8SCR, na.rm = TRUE),
              att8_dis = mean(ATT8SCR_FSM6CLA1A, na.rm = TRUE),
              att8_non = mean(ATT8SCR_NFSM6CLA1A, na.rm = TRUE),
              fsm      = mean(PTFSM6CLA1A, na.rm = TRUE),
              absence  = mean(PERCTOT, na.rm = TRUE),
              persistent = mean(PPERSABS10, na.rm = TRUE),
              eal      = mean(PNUMEAL, na.rm = TRUE),
              ks2_c    = mean(ks2_c, na.rm = TRUE),
              sen_ehcp = mean(PSENELSE, na.rm = TRUE),
              .groups = "drop") |>
    mutate(gap = att8_non - att8_dis)
}

la_profiles <- function(panel) {
  panel |>
    filter(MINORGROUP %in% c("Academy", "Maintained school"),
           !is.na(ATT8SCR), ATT8SCR > 0) |>
    group_by(year_label, LANAME, gor_name) |>
    profile_rows() |>
    group_by(year_label) |>
    mutate(n_las = n(),
           rank_att8     = rank(-att8),
           rank_att8_dis = rank(-att8_dis),
           rank_absence  = rank(-absence),        # 1 = worst
           rank_fsm      = rank(-fsm),            # 1 = most disadvantaged
           rank_gap      = rank(-gap)) |>         # 1 = widest
    ungroup() |>
    mutate(LANAME = as.character(LANAME))
}

#' Every district in the model's LAs, profiled the same way, so a
#' district-level area can be ranked among districts
district_profiles <- function(panel, lad_map) {
  panel |>
    filter(MINORGROUP %in% c("Academy", "Maintained school"),
           !is.na(ATT8SCR), ATT8SCR > 0) |>
    mutate(URN = as.integer(URN)) |>
    left_join(lad_map, by = "URN") |>
    filter(!is.na(lad)) |>
    group_by(year_label, lad, LANAME) |>
    profile_rows() |>
    ungroup() |>
    mutate(LANAME = as.character(LANAME))
}


# ---- 4. LA random effects from the published models ---------------------
la_effects <- function(model, group) {
  re <- ranef(model, condVar = TRUE)[["LANAME:gor_name"]]
  tibble(grp = rownames(re), effect = re[["(Intercept)"]],
         se = sqrt(attr(re, "postVar")[1, 1, ])) |>
    mutate(la = sub(":.*$", "", grp),
           ci_lo = effect - 1.96 * se, ci_hi = effect + 1.96 * se,
           sig = ci_lo > 0 | ci_hi < 0,
           rank = rank(-effect), n_las = n(), group = group)
}


# ---- 5. Why an area differs from England, term by term ------------------
#' Rows of a model's frame that belong to a set of schools
#'
#' model.frame() keeps the predictors but not the URN. A school-year is
#' identified exactly by its LA, year and the three continuous intake
#' terms, which is checked to be unique before it is relied on.
frame_urn <- function(model, d) {
  mf <- model.frame(model)
  key <- function(la, yr, a, b, c) paste(la, yr, signif(a, 10), signif(b, 10), signif(c, 10))
  k_mf <- key(mf$LANAME, mf$year_label, mf[["log(PTFSM6CLA1A)"]], mf[["log(PERCTOT)"]], mf$ks2_c)
  k_d  <- key(d$LANAME, d$year_label, log(d$PTFSM6CLA1A), log(d$PERCTOT), d$ks2_c)
  look <- tibble(k = k_d, URN = as.integer(d$URN)) |> distinct()
  if (anyDuplicated(look$k)) {
    look <- look |> group_by(k) |> filter(n() == 1) |> ungroup()
  }
  look$URN[match(k_mf, look$k)]
}

#' Decompose an area's mean log Attainment 8 against the national mean
#'
#' The model is additive on the log scale, so the gap between an area's
#' average and England's splits exactly into one piece per fixed effect
#' (coefficient times the difference in the mean of that column), one per
#' random effect, and the mean residual. A piece times 100 is roughly a
#' percentage difference in Attainment 8. Every piece is computed over
#' the school-years the model was fitted on.
decompose_area <- function(model, urns, area_name, group, mf_urn) {
  X  <- getME(model, "X")
  b  <- fixef(model)
  mf <- model.frame(model)
  y  <- getME(model, "y")            # already log(Attainment 8)
  e  <- y - getME(model, "mu")       # residual, on exactly the fitted rows
  in_a <- !is.na(mf_urn) & mf_urn %in% urns
  if (!any(in_a)) return(NULL)

  fix <- (colMeans(X[in_a, , drop = FALSE]) - colMeans(X)) * b
  term_group <- function(t) dplyr::case_when(
    t == "(Intercept)"               ~ NA_character_,
    t == "log(PTFSM6CLA1A)"          ~ "Disadvantage (% FSM)",
    t == "log(PERCTOT)"              ~ "Absence",
    t == "log(PNUMEAL)"              ~ "English as an additional language",
    t == "ks2_c"                     ~ "Prior attainment (KS2)",
    grepl("^ADMPOL", t)              ~ "Admissions (selection)",
    t == "gorard_segregation"        ~ "Segregation (LA)",
    TRUE                             ~ "Teacher workforce")
  fixed <- tibble(term = names(fix), contrib = unname(fix)) |>
    mutate(component = term_group(term)) |>
    filter(!is.na(component)) |>
    group_by(component) |> summarise(contrib = sum(contrib), .groups = "drop")

  re   <- ranef(model)
  pick <- function(tab, key) { v <- tab[key, "(Intercept)"]; v[is.na(v)] <- 0; v }
  la_key  <- paste(mf$LANAME, mf$gor_name, sep = ":")
  re_rows <- tibble(
    `Ofsted rating`           = pick(re$OFSTEDRATING_1, as.character(mf$OFSTEDRATING_1)),
    `Region`                  = pick(re$gor_name, as.character(mf$gor_name)),
    `Local authority effect`  = pick(re[["LANAME:gor_name"]], la_key),
    `Year mix`                = pick(re$year_label, as.character(mf$year_label)),
    `Unexplained (school residual)` = unname(e))
  rand <- tibble(component = names(re_rows),
                 contrib = colMeans(re_rows[in_a, ]) - colMeans(re_rows))

  out <- bind_rows(fixed, rand) |>
    mutate(la = area_name, group = group,
           total_log = mean(y[in_a]) - mean(y),
           nat_att8  = exp(mean(y)),
           la_att8   = exp(mean(y[in_a])),
           n_rows    = sum(in_a),
           points    = contrib * nat_att8)
  stopifnot(abs(sum(out$contrib) - out$total_log[1]) < 1e-8)
  out
}


# ---- 6. Absence, sessions-weighted, ten years ---------------------------
REASONS <- c(
  sess_auth_illness                = "Illness",
  sess_unauth_other                = "Unauthorised, other",
  sess_auth_other                  = "Authorised, other",
  sess_auth_appointments           = "Medical appointments",
  sess_auth_temp_reduced_timetable = "Reduced timetable",
  sess_unauth_holiday              = "Unauthorised holiday",
  sess_unauth_late                 = "Late",
  sess_auth_excluded               = "Suspended (code E)",
  sess_auth_religious              = "Religious observance",
  sess_unauth_noyet                = "No reason yet",
  sess_auth_interview              = "Interview",
  sess_auth_reg_performance        = "Performance or sport",
  sess_auth_study                  = "Study leave",
  sess_auth_traveller              = "Traveller absence",
  sess_auth_holiday                = "Authorised holiday",
  sess_auth_ext_holiday            = "Extended holiday")
SECONDARY <- "State-funded secondary"

read_absence_all <- function() {
  zip <- ees_path("absence_2term_school")
  d <- read_csv(unz(zip, "data/Absence_2term_school.csv"),
                col_types = cols(.default = col_character()), progress = FALSE)
  num <- c("enrolments", "enrolments_pa_10_exact", "sess_possible",
           "sess_overall", "sess_authorised", "sess_unauthorised",
           names(REASONS))
  d |>
    filter(education_phase == SECONDARY) |>
    mutate(across(any_of(num), ~suppressWarnings(as.numeric(.x)))) |>
    filter(sess_possible > 0) |>
    transmute(time_period = as.character(time_period),
              urn = as.integer(school_urn), la_name, region_name,
              school_name, enrolments, pa = enrolments_pa_10_exact,
              poss = sess_possible, overall = sess_overall,
              authorised = sess_authorised, unauthorised = sess_unauthorised,
              across(any_of(names(REASONS))))
}

#' Rows of a school-level file that belong to an area: by LA for a whole
#' authority, by URN (and predecessors) for a district
in_area <- function(df, a, fam) {
  if (is.null(a$lads)) df$la_name %in% a$las else df$urn %in% fam
}

absence_agg <- function(d, area) d |>
  group_by(time_period) |>
  summarise(poss = sum(poss, na.rm = TRUE), overall = sum(overall, na.rm = TRUE),
            authorised = sum(authorised, na.rm = TRUE),
            unauthorised = sum(unauthorised, na.rm = TRUE),
            illness = sum(sess_auth_illness, na.rm = TRUE),
            excluded = sum(sess_auth_excluded, na.rm = TRUE),
            pa = sum(pa, na.rm = TRUE), enrol = sum(enrolments, na.rm = TRUE),
            schools = n(), .groups = "drop") |>
  mutate(area = area,
         abs_rate = 100 * overall / poss, auth_rate = 100 * authorised / poss,
         unauth_rate = 100 * unauthorised / poss,
         illness_rate = 100 * illness / poss, excl_rate = 100 * excluded / poss,
         pa_rate = 100 * pa / enrol)

#' LA and district ranks on headline absence, every year (1 = highest)
absence_ranks <- function(ab, lad_by_urn) {
  la <- ab |>
    group_by(time_period, la_name) |>
    summarise(poss = sum(poss), overall = sum(overall), pa = sum(pa, na.rm = TRUE),
              enrol = sum(enrolments, na.rm = TRUE), .groups = "drop") |>
    mutate(abs_rate = 100 * overall / poss, pa_rate = 100 * pa / enrol) |>
    group_by(time_period) |>
    mutate(rank_abs = rank(-abs_rate), rank_pa = rank(-pa_rate), n_la = n()) |>
    ungroup()
  # Districts, only where every school can be placed: the latest years
  lad <- ab |>
    inner_join(lad_by_urn, by = c("urn" = "URN")) |>
    group_by(time_period, lad) |>
    summarise(poss = sum(poss), overall = sum(overall), pa = sum(pa, na.rm = TRUE),
              enrol = sum(enrolments, na.rm = TRUE), schools = n(), .groups = "drop") |>
    mutate(abs_rate = 100 * overall / poss, pa_rate = 100 * pa / enrol) |>
    group_by(time_period) |>
    mutate(rank_abs = rank(-abs_rate), n_lad = n()) |>
    ungroup()
  list(la = la, lad = lad)
}

reason_one <- function(d, area, year) {
  d <- d |> filter(time_period == year)
  tot <- sum(d$poss, na.rm = TRUE)
  d |> summarise(across(all_of(names(REASONS)), ~sum(.x, na.rm = TRUE))) |>
    pivot_longer(everything(), names_to = "code", values_to = "sessions") |>
    mutate(area = area, reason = unname(REASONS[code]), pp = 100 * sessions / tot,
           time_period = year)
}

#' Each school's absence by reason in one year, keyed by the school's URN
#' in the area even when that year's return is under a successor's
school_reasons <- function(ab, links, year) {
  ab |>
    filter(time_period == year) |>
    inner_join(links, by = "urn") |>
    group_by(URN) |>
    summarise(school_name = dplyr::last(school_name, order_by = urn),
              across(c(enrolments, pa, poss, overall, sess_auth_illness, sess_unauth_other,
                       sess_unauth_holiday, sess_auth_holiday, sess_auth_excluded,
                       sess_auth_temp_reduced_timetable), ~sum(.x, na.rm = TRUE)),
              .groups = "drop") |>
    transmute(urn = as.character(URN), school_name, enrolments,
              abs_rate = 100 * overall / poss,
              illness = 100 * sess_auth_illness / poss,
              unauth_other = 100 * sess_unauth_other / poss,
              holiday = 100 * (sess_unauth_holiday + sess_auth_holiday) / poss,
              excluded = 100 * sess_auth_excluded / poss,
              reduced = 100 * sess_auth_temp_reduced_timetable / poss,
              pa_rate = 100 * pa / enrolments)
}


# ---- 7. Suspensions and permanent exclusions ----------------------------
read_exclusions_all <- function() {
  zip <- ees_path("exclusions_school")
  read_csv(unz(zip, "data/exc_school.csv"),
           col_types = cols(.default = col_character()), progress = FALSE) |>
    filter(education_phase == SECONDARY) |>
    transmute(time_period = as.character(time_period),
              urn = as.integer(school_urn), la_name, school_name,
              headcount  = suppressWarnings(as.numeric(headcount)),
              perm_excl  = suppressWarnings(as.numeric(perm_excl)),
              suspension = suppressWarnings(as.numeric(suspension)),
              one_plus   = suppressWarnings(as.numeric(one_plus_susp)))
}

excl_agg <- function(d, area) d |>
  filter(!is.na(headcount), headcount > 0) |>
  group_by(time_period) |>
  summarise(headcount = sum(headcount), susp = sum(suspension, na.rm = TRUE),
            perm = sum(perm_excl, na.rm = TRUE), .groups = "drop") |>
  mutate(la_name = area, susp_rate = 100 * susp / headcount, perm_rate = 100 * perm / headcount)

excl_la_ranks <- function(ex) {
  ex |>
    filter(!is.na(headcount), headcount > 0) |>
    group_by(time_period, la_name) |>
    summarise(headcount = sum(headcount), susp = sum(suspension, na.rm = TRUE),
              perm = sum(perm_excl, na.rm = TRUE), .groups = "drop") |>
    mutate(susp_rate = 100 * susp / headcount, perm_rate = 100 * perm / headcount) |>
    group_by(time_period) |>
    mutate(rank_susp = rank(-susp_rate), rank_perm = rank(-perm_rate), n_la = n()) |>
    ungroup()
}


# ---- 8. The joint-signal plane, as the report fits it -------------------
joint_signal <- function(d) {
  s1 <- d |> filter(!is.na(ks2_c)) |> droplevels()
  m1 <- lmer(log(PERCTOT) ~ log(PTFSM6CLA1A) + log(PNUMEAL) + ks2_c +
               gorard_segregation + (1 | year_label) + (1 | gor_name/LANAME),
             data = s1, REML = TRUE, control = ctrl)
  # The intake-only version: no LA term, so an LA's excess absence over
  # what its intake predicts stays visible rather than being absorbed.
  m1n <- lmer(log(PERCTOT) ~ log(PTFSM6CLA1A) + log(PNUMEAL) + ks2_c +
                gorard_segregation + (1 | year_label) + (1 | gor_name),
              data = s1, REML = TRUE, control = ctrl)
  s1 <- s1 |>
    mutate(expected_absence = exp(predict(m1, newdata = s1, re.form = NULL,
                                          allow.new.levels = TRUE)),
           expected_intake  = exp(predict(m1n, newdata = s1, re.form = NULL,
                                          allow.new.levels = TRUE)))
  s2 <- d |> filter(!is.na(ATT8SCR), ATT8SCR > 0) |>
    inner_join(s1 |> select(URN, year_label, expected_absence, expected_intake),
               by = c("URN", "year_label")) |>
    droplevels()
  contrasts(s2$OFSTEDRATING_1) <- contr.treatment(levels(s2$OFSTEDRATING_1))
  m2 <- lmer(log(ATT8SCR) ~ log(PTFSM6CLA1A) + log(PNUMEAL) + ks2_c + ADMPOL_PT +
               gorard_segregation + (1 | year_label) + (1 | OFSTEDRATING_1) +
               (1 | gor_name/LANAME),
             data = s2, REML = TRUE, control = ctrl)
  s2$resid_M2_log <- residuals(m2)
  per_school <- s2 |>
    group_by(URN, SCHNAME, LANAME) |>
    summarise(mean_log_resid = mean(resid_M2_log), mean_att8 = mean(ATT8SCR),
              obs_abs = mean(PERCTOT), exp_abs = mean(expected_absence),
              exp_intake = mean(expected_intake), n_years = n(), .groups = "drop") |>
    mutate(vadd_M2 = mean_att8 * (exp(mean_log_resid) - 1),
           resid_abs_pp = obs_abs - exp_abs,
           excess_intake_pp = obs_abs - exp_intake,
           LANAME = as.character(LANAME), URN = as.integer(URN))
  list(schools = per_school,
       stage1_r2 = cor(log(s1$PERCTOT), log(s1$expected_absence))^2,
       stage1_intake_r2 = cor(log(s1$PERCTOT), log(s1$expected_intake))^2)
}


# ---- 9. What closing the absence gap would be worth ---------------------
absence_scenarios <- function(schools, mods, nat_abs) {
  b_abs  <- fixef(mods$all)[["log(PERCTOT)"]]
  b_absd <- fixef(mods$disadvantaged)[["log(PERCTOT)"]]
  b_absn <- fixef(mods$non_disadvantaged)[["log(PERCTOT)"]]
  b_fsm  <- fixef(mods$all)[["log(PTFSM6CLA1A)"]]
  area_fsm <- mean(schools$fsm, na.rm = TRUE)
  schools |>
    transmute(SCHNAME, district, absence, fsm, att8, att8_dis, att8_non,
              abs_target = pmin(absence, nat_abs),
              shift_abs  = log(abs_target) - log(absence),
              gain_all = att8 * (exp(b_abs * shift_abs) - 1),
              gain_dis = att8_dis * (exp(b_absd * shift_abs) - 1),
              gain_non = att8_non * (exp(b_absn * shift_abs) - 1),
              fsm_target = pmin(fsm, area_fsm),
              shift_fsm  = log(fsm_target) - log(fsm),
              gain_fsm   = att8 * (exp(b_fsm * shift_fsm) - 1))
}


# ---- 10. Shared, national objects - built once for all areas ------------
build_shared <- function() {
  message("Panel and published models ...")
  # One year of the performance tables spells Uckfield College with two
  # spaces, which is enough to split a school in two in any group_by.
  panel <- readRDS(here::here("data", "panel_data.rds")) |>
    mutate(SCHNAME = gsub("\\s+", " ", trimws(SCHNAME)))
  mods  <- readRDS(here::here("data", "models_imputed.rds"))
  d     <- model_data(panel)
  lad_map <- school_lad(panel)
  latest <- max(as.character(d$year_label))
  nat_latest <- d |> filter(year_label == latest)
  national <- list(absence = mean(nat_latest$PERCTOT), fsm = mean(nat_latest$PTFSM6CLA1A),
                   att8 = mean(nat_latest$ATT8SCR),
                   att8_dis = mean(nat_latest$ATT8SCR_FSM6CLA1A, na.rm = TRUE),
                   att8_non = mean(nat_latest$ATT8SCR_NFSM6CLA1A, na.rm = TRUE),
                   ks2_c = mean(nat_latest$ks2_c, na.rm = TRUE),
                   eal = mean(nat_latest$PNUMEAL),
                   persistent = mean(nat_latest$PPERSABS10, na.rm = TRUE),
                   sd_absence = sd(nat_latest$PERCTOT), sd_fsm = sd(nat_latest$PTFSM6CLA1A),
                   year = latest)

  message("Profiles, effects, the five-year value added ...")
  va_all <- value_added_5yr(verify = FALSE) |>
    filter(!is.na(va_ATT8SCR)) |>
    mutate(SCHNAME = gsub("\\s+", " ", trimws(SCHNAME)), URN = as.integer(URN)) |>
    select(URN, SCHNAME, LANAME, year_label, ATT8SCR, ATT8SCR_FSM6CLA1A,
           ATT8SCR_NFSM6CLA1A, PERCTOT, PTFSM6CLA1A,
           va_ATT8SCR, va_ATT8SCR_FSM6CLA1A, va_ATT8SCR_NFSM6CLA1A)

  message("Absence and exclusions files ...")
  ab <- read_absence_all()
  ex <- read_exclusions_all()

  message("Joint-signal plane (two lmer fits) ...")
  js <- joint_signal(d)

  r27 <- readRDS(here::here("revisions", "r27_results.rds"))
  mf_urn <- lapply(mods, frame_urn, d = d)
  for (k in names(mf_urn))
    message(sprintf("  %s: %.1f%% of model rows matched to a school",
                    k, 100 * mean(!is.na(mf_urn[[k]]))))
  list(panel = panel, mods = mods, d = d, lad_map = lad_map, latest = latest,
       national = national,
       profiles = la_profiles(panel),
       lad_profiles = district_profiles(panel, lad_map),
       effects = bind_rows(la_effects(mods$all, "All pupils"),
                           la_effects(mods$disadvantaged, "Disadvantaged"),
                           la_effects(mods$non_disadvantaged, "Non-disadvantaged")),
       r27 = r27,
       mf_urn = mf_urn,
       va_all = va_all, ab = ab, ex = ex, js = js,
       abs_ranks = absence_ranks(ab, lad_map),
       excl_ranks = excl_la_ranks(ex))
}


# ---- 11. One area -------------------------------------------------------
build_area <- function(slug, sh) {
  spec <- AREAS[[slug]]
  a <- spec$area
  all_areas <- c(list(a), spec$comps)
  message("\n== ", a$name, " ==")

  schools <- school_table(sh$panel, a, spec$coastal_towns, sh$lad_map)
  urns  <- area_urns(sh$panel, a, sh$lad_map)
  links <- urn_links(urns)
  fam   <- unique(links$urn)
  message(sprintf("  %d state secondaries, %s", nrow(schools), sh$latest))

  # The area's own profile when it is not a whole LA, from the same school
  # means as the LA profiles
  area_profile <- sh$panel |>
    filter(MINORGROUP %in% c("Academy", "Maintained school"), !is.na(ATT8SCR), ATT8SCR > 0,
           as.integer(URN) %in% urns) |>
    group_by(year_label) |> profile_rows() |> mutate(area = a$name)

  decomp <- bind_rows(lapply(all_areas, function(x) {
    u <- area_urns(sh$panel, x, sh$lad_map)
    bind_rows(
      decompose_area(sh$mods$all, u, x$name, "All pupils", sh$mf_urn$all),
      decompose_area(sh$mods$disadvantaged, u, x$name, "Disadvantaged", sh$mf_urn$disadvantaged),
      decompose_area(sh$mods$non_disadvantaged, u, x$name, "Non-disadvantaged",
                     sh$mf_urn$non_disadvantaged))
  }))

  # Value added, followed by URN and shown under the latest name
  va <- sh$va_all |>
    filter(URN %in% urns) |>
    group_by(URN) |>
    mutate(school = {
      latest <- dplyr::last(SCHNAME, order_by = year_label)
      older  <- setdiff(unique(SCHNAME), latest)
      if (length(older)) paste0(latest, " (formerly ", paste(older, collapse = ", "), ")")
      else latest
    }) |>
    ungroup() |>
    mutate(LANAME = as.character(LANAME))

  years <- sort(unique(sh$ab$time_period))
  abs_series <- bind_rows(
    absence_agg(sh$ab, "England"),
    absence_agg(filter(sh$ab, region_name == "South East"), "South East"),
    bind_rows(lapply(all_areas, function(x) {
      fx <- urn_family(area_urns(sh$panel, x, sh$lad_map))
      absence_agg(sh$ab[in_area(sh$ab, x, fx), ], x$name)
    })))
  reasons <- bind_rows(lapply(c(tail(years, 1), "202425", "201819"), function(y) bind_rows(
    reason_one(sh$ab, "England", y),
    bind_rows(lapply(all_areas, function(x) {
      fx <- urn_family(area_urns(sh$panel, x, sh$lad_map))
      reason_one(sh$ab[in_area(sh$ab, x, fx), ], x$name, y)
    })))))
  sch_reasons <- school_reasons(sh$ab, links, tail(years, 1)) |>
    left_join(schools |> transmute(urn = URN, district, coastal), by = "urn")

  ex_area <- bind_rows(
    excl_agg(sh$ex, "England"),
    bind_rows(lapply(all_areas, function(x) {
      fx <- urn_family(area_urns(sh$panel, x, sh$lad_map))
      excl_agg(sh$ex[in_area(sh$ex, x, fx), ], x$name)
    })))
  # Keyed by the area's URN, as for the absence reasons
  excl_schools <- sh$ex |>
    filter(time_period == max(time_period), !is.na(headcount), headcount > 0) |>
    inner_join(links, by = "urn") |>
    group_by(time_period, URN) |>
    summarise(la_name = first(la_name), school_name = dplyr::last(school_name, order_by = urn),
              across(c(headcount, perm_excl, suspension, one_plus), ~sum(.x, na.rm = TRUE)),
              .groups = "drop") |>
    transmute(time_period, urn = URN, la_name, school_name, headcount, perm_excl,
              suspension, one_plus,
              susp_rate = 100 * suspension / headcount, perm_rate = 100 * perm_excl / headcount)

  scen <- absence_scenarios(schools, sh$mods, sh$national$absence)

  abs_context <- sh$r27$context |>
    mutate(LANAME = as.character(LANAME),
           excess = mean_abs - mean_exp_abs_NoL,
           rank_excess = rank(-excess), n_las = n())
  # The same comparison for the area's own schools, from the plane's
  # intake-only stage 1, so a district can be set against its county
  area_excess <- sh$js$schools |>
    filter(URN %in% urns) |>
    summarise(obs = mean(obs_abs), exp_intake = mean(exp_intake),
              excess = mean(excess_intake_pp), schools = n())

  changes <- readRDS(here::here("data", "gias", "gias_establishments.rds")) |>
    filter(la_name %in% a$las, phase %in% c("Secondary", "All-through"),
           (!is.na(open_date) & open_date >= as.Date("2021-09-01")) |
             (!is.na(close_date) & close_date >= as.Date("2021-09-01"))) |>
    select(urn, establishment_name, status, open_date, close_date,
           reason_opened, reason_closed, trust_name) |>
    arrange(establishment_name)

  list(
    spec = spec, area = a, comps = spec$comps, urns = urns, urn_family = fam,
    changes = changes, schools = schools, national = sh$national,
    profiles = sh$profiles, lad_profiles = sh$lad_profiles, area_profile = area_profile,
    effects = sh$effects, r27 = list(dis = sh$r27$dis, all = sh$r27$all),
    abs_context = abs_context, area_excess = area_excess,
    decomp = decomp, va = va,
    abs_series = abs_series, abs_ranks = sh$abs_ranks$la, abs_ranks_lad = sh$abs_ranks$lad,
    reasons = reasons, sch_reasons = sch_reasons,
    excl = list(la = sh$excl_ranks, area = ex_area,
                england = ex_area |> filter(la_name == "England")),
    excl_schools = excl_schools,
    joint = list(schools = sh$js$schools, stage1_r2 = sh$js$stage1_r2,
                 stage1_intake_r2 = sh$js$stage1_intake_r2),
    scenarios = scen,
    coefs = lapply(sh$mods, fixef),
    typology = readRDS(here::here("data", "la_typology.rds")),
    abs_years = years, latest_panel = sh$latest,
    built_at = Sys.time())
}


build_areas <- function(slugs = names(AREAS)) {
  sh <- build_shared()
  for (s in slugs) {
    out <- build_area(s, sh)
    f <- here::here("data", paste0("area_", s, ".rds"))
    saveRDS(out, f)
    message("Wrote ", f)
  }
  invisible(TRUE)
}

if (sys.nframe() == 0) {
  args <- commandArgs(trailingOnly = TRUE)
  build_areas(if (length(args)) args else names(AREAS))
}
