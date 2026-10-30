# 17_east_sussex.R - East Sussex through the national model
# ---------------------------------------------------------------------------
# East Sussex County Council asked what its secondary school system looks
# like through the lens of How to Pull the Right Lever: what is affecting
# attainment in the county, and the part attendance plays in it. This
# script assembles every number the companion document reports, so the
# document only reads and draws.
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
# Every source is published and under the Open Government Licence v3.0.
#
# Writes data/east_sussex.rds.
#
# Run from project root.
# ---------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(readr); library(lme4)
})
source(here::here("R", "00b_download_ees_api.R"))
source(here::here("R", "05b_value_added_2526.R"))

OUT   <- here::here("data", "east_sussex.rds")
FOCUS <- "East Sussex"
COMP  <- "Brighton and Hove"
ctrl  <- lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 20000))

# The five districts and boroughs, from each school's address. The county
# has 26 state-funded secondary schools, and every one has to be placed or
# the district summaries silently lose a school.
DISTRICT <- c(
  "Ark Alexandra Academy"           = "Hastings",
  "The Hastings Academy"            = "Hastings",
  "The St Leonards Academy"         = "Hastings",
  "Bexhill High Academy"            = "Rother",
  "St Richard's Catholic College"   = "Rother",
  "Claverham Community College"     = "Rother",
  "Robertsbridge Community College" = "Rother",
  "Rye College"                     = "Rother",
  "The Cavendish School"            = "Eastbourne",
  "The Eastbourne Academy"          = "Eastbourne",
  "Gildredge House"                 = "Eastbourne",
  "Ratton School"                   = "Eastbourne",
  "St Catherine's College"          = "Eastbourne",
  "The Turing School"               = "Eastbourne",
  "Chailey School"                  = "Lewes",
  "King's Academy Ringmer"          = "Lewes",
  "Peacehaven Community School"     = "Lewes",
  "Priory School"                   = "Lewes",
  "Seaford Head School"             = "Lewes",
  "Seahaven Academy"                = "Lewes",
  "Beacon Academy"                  = "Wealden",
  "Hailsham Community College"      = "Wealden",
  "Heathfield Community College"    = "Wealden",
  "Uckfield College"                = "Wealden",
  "Uplands Academy"                 = "Wealden",
  "Willingdon Community School"     = "Wealden")

# Schools in the coastal towns - the places the DfE's coastal programme is
# aimed at - against the inland market towns and villages.
COASTAL_TOWNS <- c("Hastings", "St Leonards-on-Sea", "Bexhill-on-Sea",
                   "Eastbourne", "Seaford", "Newhaven", "Peacehaven", "Rye")


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


# ---- 2. Schools ---------------------------------------------------------
school_table <- function(panel) {
  s <- panel |>
    filter(LANAME == FOCUS, MINORGROUP %in% c("Academy", "Maintained school"),
           year_label == max(year_label)) |>
    mutate(district = unname(DISTRICT[SCHNAME]),
           coastal  = TOWN %in% COASTAL_TOWNS |
             tolower(TOWN) %in% tolower(COASTAL_TOWNS))
  missing <- setdiff(s$SCHNAME, names(DISTRICT))
  if (length(missing)) stop("No district for: ", paste(missing, collapse = ", "))

  # Three schools carry no grid reference in the 2024/25 performance tables;
  # Get Information about Schools has every one
  gias <- readRDS(here::here("data", "gias", "gias_establishments.rds")) |>
    transmute(URN = as.character(urn), e_g = as.numeric(easting), n_g = as.numeric(northing))
  s <- s |>
    mutate(URN = as.character(URN)) |>
    left_join(gias, by = "URN") |>
    mutate(easting = coalesce(as.numeric(easting), e_g),
           northing = coalesce(as.numeric(northing), n_g)) |>
    select(-e_g, -n_g)
  if (any(is.na(s$easting))) stop("No location for: ", paste(s$SCHNAME[is.na(s$easting)], collapse = ", "))

  ll <- sf::st_as_sf(s |> select(URN, easting, northing) |> filter(!is.na(easting)),
                     coords = c("easting", "northing"), crs = 27700) |>
    sf::st_transform(4326)
  xy <- sf::st_coordinates(ll)
  s |>
    left_join(tibble(URN = ll$URN, lon = xy[, 1], lat = xy[, 2]), by = "URN") |>
    transmute(URN, SCHNAME, TOWN, district, coastal, lon, lat,
              type = MINORGROUP, religion = RELCHAR, ofsted = as.character(OFSTEDRATING_1),
              pupils = TOTPUPS, ks4_cohort = TPUP,
              att8 = ATT8SCR, att8_dis = ATT8SCR_FSM6CLA1A, att8_non = ATT8SCR_NFSM6CLA1A,
              p8 = P8MEA, fsm = PTFSM6CLA1A, absence = PERCTOT, persistent = PPERSABS10,
              eal = PNUMEAL, ks2_c, sen_ehcp = PSENELSE, sen_support = PSENELK)
}


# ---- 3. Local authority profiles, every LA, every panel year ------------
# School means, as the published report computes them, so the ranks here
# are the ranks it quotes.
la_profiles <- function(panel) {
  panel |>
    filter(MINORGROUP %in% c("Academy", "Maintained school"),
           !is.na(ATT8SCR), ATT8SCR > 0) |>
    group_by(year_label, LANAME, gor_name) |>
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
    mutate(gap = att8_non - att8_dis) |>
    group_by(year_label) |>
    mutate(n_las = n(),
           rank_att8     = rank(-att8),
           rank_att8_dis = rank(-att8_dis),
           rank_absence  = rank(-absence),        # 1 = worst
           rank_fsm      = rank(-fsm),            # 1 = most disadvantaged
           rank_gap      = rank(-gap)) |>         # 1 = widest
    ungroup()
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


# ---- 5. Why East Sussex differs from England, term by term --------------
#' Decompose an LA's mean log Attainment 8 against the national mean
#'
#' The model is additive on the log scale, so the gap between an LA's
#' average and England's splits exactly into one piece per fixed effect
#' (coefficient times the difference in the mean of that column), one per
#' random effect, and the mean residual. A piece times 100 is roughly a
#' percentage difference in Attainment 8. Every piece is computed over
#' the school-years the model was fitted on.
decompose_la <- function(model, la, group) {
  X  <- getME(model, "X")
  b  <- fixef(model)
  mf <- model.frame(model)
  y  <- getME(model, "y")            # already log(Attainment 8)
  e  <- y - getME(model, "mu")       # residual, on exactly the fitted rows
  in_la <- as.character(mf$LANAME) == la
  if (!any(in_la)) return(NULL)

  fix <- (colMeans(X[in_la, , drop = FALSE]) - colMeans(X)) * b
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

  # Random effects, row by row, then averaged in the LA and nationally
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
                 contrib = colMeans(re_rows[in_la, ]) - colMeans(re_rows))

  out <- bind_rows(fixed, rand) |>
    mutate(la = la, group = group,
           total_log = mean(y[in_la]) - mean(y),
           nat_att8  = exp(mean(y)),
           la_att8   = exp(mean(y[in_la])),
           points    = contrib * nat_att8)
  # The pieces must add up to the whole, or something has been left out
  stopifnot(abs(sum(out$contrib) - out$total_log[1]) < 1e-8)
  out
}


# ---- 6. Absence, sessions-weighted, ten years ---------------------------
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
              urn = as.character(school_urn), la_name, region_name,
              school_name, enrolments, pa = enrolments_pa_10_exact,
              poss = sess_possible, overall = sess_overall,
              authorised = sess_authorised, unauthorised = sess_unauthorised,
              across(any_of(names(REASONS))))
}

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

absence_series <- function(ab) {
  agg <- function(d, area) d |>
    group_by(time_period) |>
    summarise(poss = sum(poss, na.rm = TRUE), overall = sum(overall, na.rm = TRUE),
              authorised = sum(authorised, na.rm = TRUE),
              unauthorised = sum(unauthorised, na.rm = TRUE),
              illness = sum(sess_auth_illness, na.rm = TRUE),
              excluded = sum(sess_auth_excluded, na.rm = TRUE),
              pa = sum(pa, na.rm = TRUE), enrol = sum(enrolments, na.rm = TRUE),
              .groups = "drop") |>
    mutate(area = area,
           abs_rate = 100 * overall / poss, auth_rate = 100 * authorised / poss,
           unauth_rate = 100 * unauthorised / poss,
           illness_rate = 100 * illness / poss, excl_rate = 100 * excluded / poss,
           pa_rate = 100 * pa / enrol)
  bind_rows(agg(ab, "England"),
            agg(filter(ab, region_name == "South East"), "South East"),
            agg(filter(ab, la_name == FOCUS), FOCUS),
            agg(filter(ab, la_name == COMP), COMP))
}

#' LA ranks on headline absence, every year (1 = highest absence)
absence_la_ranks <- function(ab) {
  ab |>
    group_by(time_period, la_name) |>
    summarise(poss = sum(poss), overall = sum(overall), pa = sum(pa, na.rm = TRUE),
              enrol = sum(enrolments, na.rm = TRUE), .groups = "drop") |>
    mutate(abs_rate = 100 * overall / poss, pa_rate = 100 * pa / enrol) |>
    group_by(time_period) |>
    mutate(rank_abs = rank(-abs_rate), rank_pa = rank(-pa_rate), n_la = n()) |>
    ungroup()
}

#' Every reason, as a share of possible sessions, for one area and year
reason_profile <- function(ab, year) {
  one <- function(d, area) {
    tot <- sum(d$poss, na.rm = TRUE)
    d |> summarise(across(all_of(names(REASONS)), ~sum(.x, na.rm = TRUE))) |>
      pivot_longer(everything(), names_to = "code", values_to = "sessions") |>
      mutate(area = area, reason = unname(REASONS[code]),
             pp = 100 * sessions / tot)
  }
  y <- filter(ab, time_period == year)
  bind_rows(one(y, "England"), one(filter(y, la_name == FOCUS), FOCUS),
            one(filter(y, la_name == COMP), COMP)) |>
    mutate(time_period = year)
}

#' Each school's absence profile in the latest year
school_reasons <- function(ab, year) {
  ab |>
    filter(time_period == year, la_name == FOCUS) |>
    transmute(urn, school_name, enrolments,
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
              urn = as.character(school_urn), la_name, school_name,
              headcount  = suppressWarnings(as.numeric(headcount)),
              perm_excl  = suppressWarnings(as.numeric(perm_excl)),
              suspension = suppressWarnings(as.numeric(suspension)),
              one_plus   = suppressWarnings(as.numeric(one_plus_susp)))
}

exclusion_series <- function(ex) {
  la <- ex |>
    filter(!is.na(headcount), headcount > 0) |>
    group_by(time_period, la_name) |>
    summarise(headcount = sum(headcount), susp = sum(suspension, na.rm = TRUE),
              perm = sum(perm_excl, na.rm = TRUE), .groups = "drop") |>
    mutate(susp_rate = 100 * susp / headcount, perm_rate = 100 * perm / headcount) |>
    group_by(time_period) |>
    mutate(rank_susp = rank(-susp_rate), rank_perm = rank(-perm_rate), n_la = n()) |>
    ungroup()
  eng <- ex |>
    filter(!is.na(headcount), headcount > 0) |>
    group_by(time_period) |>
    summarise(headcount = sum(headcount), susp = sum(suspension, na.rm = TRUE),
              perm = sum(perm_excl, na.rm = TRUE), .groups = "drop") |>
    mutate(la_name = "England", susp_rate = 100 * susp / headcount,
           perm_rate = 100 * perm / headcount)
  list(la = la, england = eng)
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
           LANAME = as.character(LANAME))
  list(schools = per_school,
       stage1_r2 = cor(log(s1$PERCTOT), log(s1$expected_absence))^2,
       stage1_intake_r2 = cor(log(s1$PERCTOT), log(s1$expected_intake))^2)
}


# ---- 9. What closing the absence gap would be worth ---------------------
absence_scenarios <- function(schools, mods, nat_abs, nat_fsm) {
  b_abs  <- fixef(mods$all)[["log(PERCTOT)"]]
  b_absd <- fixef(mods$disadvantaged)[["log(PERCTOT)"]]
  b_absn <- fixef(mods$non_disadvantaged)[["log(PERCTOT)"]]
  b_fsm  <- fixef(mods$all)[["log(PTFSM6CLA1A)"]]
  county_fsm <- mean(schools$fsm, na.rm = TRUE)
  schools |>
    transmute(SCHNAME, district, absence, fsm, att8, att8_dis, att8_non,
              abs_target = pmin(absence, nat_abs),
              shift_abs  = log(abs_target) - log(absence),
              gain_all = att8 * (exp(b_abs * shift_abs) - 1),
              gain_dis = att8_dis * (exp(b_absd * shift_abs) - 1),
              gain_non = att8_non * (exp(b_absn * shift_abs) - 1),
              fsm_target = pmin(fsm, county_fsm),
              shift_fsm  = log(fsm_target) - log(fsm),
              gain_fsm   = att8 * (exp(b_fsm * shift_fsm) - 1))
}


build_east_sussex <- function() {
  message("Panel and published models ...")
  # One year of the performance tables spells Uckfield College with two
  # spaces, which is enough to split a school in two in any group_by.
  panel <- readRDS(here::here("data", "panel_data.rds")) |>
    mutate(SCHNAME = gsub("\\s+", " ", trimws(SCHNAME)))
  mods  <- readRDS(here::here("data", "models_imputed.rds"))
  d     <- model_data(panel)
  latest <- max(as.character(d$year_label))

  schools <- school_table(panel)
  message(sprintf("  %d East Sussex state secondaries, %s", nrow(schools), latest))

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

  message("LA profiles and effects ...")
  profiles <- la_profiles(panel)
  effects  <- bind_rows(la_effects(mods$all, "All pupils"),
                        la_effects(mods$disadvantaged, "Disadvantaged"),
                        la_effects(mods$non_disadvantaged, "Non-disadvantaged"))
  r27 <- readRDS(here::here("revisions", "r27_results.rds"))

  message("Decomposing the gap to England ...")
  decomp <- bind_rows(lapply(c(FOCUS, COMP), function(la) bind_rows(
    decompose_la(mods$all, la, "All pupils"),
    decompose_la(mods$disadvantaged, la, "Disadvantaged"),
    decompose_la(mods$non_disadvantaged, la, "Non-disadvantaged"))))

  message("Value added, five years ...")
  # Schools are followed by URN and shown under their latest name, with the
  # old one alongside: The Hastings Academy became Mulberry Academy
  # Hastings in 2025/26 without changing URN.
  va <- value_added_5yr(verify = FALSE) |>
    filter(LANAME %in% c(FOCUS, COMP), !is.na(va_ATT8SCR)) |>
    mutate(SCHNAME = gsub("\\s+", " ", trimws(SCHNAME))) |>
    select(URN, SCHNAME, LANAME, year_label, ATT8SCR, ATT8SCR_FSM6CLA1A,
           ATT8SCR_NFSM6CLA1A, PERCTOT, PTFSM6CLA1A,
           va_ATT8SCR, va_ATT8SCR_FSM6CLA1A, va_ATT8SCR_NFSM6CLA1A) |>
    group_by(URN) |>
    mutate(school = {
      # year_label is "2021-22" text: order it, don't which.max it
      latest <- dplyr::last(SCHNAME, order_by = year_label)
      older  <- setdiff(unique(SCHNAME), latest)
      if (length(older)) paste0(latest, " (formerly ", paste(older, collapse = ", "), ")")
      else latest
    }) |>
    ungroup()

  message("Absence, ten years, by reason ...")
  ab <- read_absence_all()
  years <- sort(unique(ab$time_period))
  abs_series <- absence_series(ab)
  abs_ranks  <- absence_la_ranks(ab)
  reasons <- bind_rows(reason_profile(ab, tail(years, 1)),
                       reason_profile(ab, "202425"),
                       reason_profile(ab, "201819"))
  sch_reasons <- school_reasons(ab, tail(years, 1)) |>
    left_join(schools |> transmute(urn = as.character(URN), district, coastal),
              by = "urn")

  message("Suspensions ...")
  ex <- read_exclusions_all()
  excl <- exclusion_series(ex)
  # A school that closed or opened mid-year can carry a row with nobody on
  # roll, which divides to Inf
  excl_schools <- ex |>
    filter(la_name == FOCUS, time_period == max(time_period),
           !is.na(headcount), headcount > 0) |>
    mutate(susp_rate = 100 * suspension / headcount, perm_rate = 100 * perm_excl / headcount)

  message("Joint-signal plane (two lmer fits) ...")
  js <- joint_signal(d)

  scen <- absence_scenarios(schools, mods, national$absence, national$fsm)

  # LA-level intake-expected absence for every LA, from the stage-1 model
  # without an LA term (the r27 'context' table)
  abs_context <- r27$context |>
    mutate(LANAME = as.character(LANAME),
           excess = mean_abs - mean_exp_abs_NoL,
           rank_excess = rank(-excess), n_las = n())

  # Openings, closures and fresh starts since the panel began: three of the
  # county's weakest schools on the measures below changed trust or URN in
  # 2025/26, and the document should say so where it names them.
  changes <- readRDS(here::here("data", "gias", "gias_establishments.rds")) |>
    filter(la_name == FOCUS, phase %in% c("Secondary", "All-through"),
           (!is.na(open_date) & open_date >= as.Date("2021-09-01")) |
             (!is.na(close_date) & close_date >= as.Date("2021-09-01"))) |>
    select(urn, establishment_name, status, open_date, close_date,
           reason_opened, reason_closed, trust_name) |>
    arrange(establishment_name)

  out <- list(
    changes = changes,
    schools = schools, national = national, profiles = profiles,
    effects = effects, r27 = list(dis = r27$dis, all = r27$all),
    abs_context = abs_context, decomp = decomp, va = va,
    abs_series = abs_series, abs_ranks = abs_ranks, reasons = reasons,
    sch_reasons = sch_reasons, excl = excl, excl_schools = excl_schools,
    joint = js, scenarios = scen,
    coefs = lapply(mods, fixef),
    typology = readRDS(here::here("data", "la_typology.rds")),
    abs_years = years, latest_panel = latest,
    built_at = Sys.time())
  saveRDS(out, OUT)
  message("\nWrote ", OUT)
  invisible(out)
}

if (sys.nframe() == 0) build_east_sussex()
