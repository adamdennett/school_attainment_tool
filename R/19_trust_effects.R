# 19_trust_effects.R - Do multi-academy trusts carry an effect of their own?
# ---------------------------------------------------------------------------
# Adds the trust as a grouping level in the published attainment models,
# alongside the school, and asks whether schools in the same trust share
# anything once intake, place and the school itself are accounted for.
#
# Two things make this harder than adding a column.
#
# 1. TRUSTS CHOOSE THEIR SCHOOLS, AND ARE GIVEN THEM. Sponsored academies
#    were placed with a trust because they were struggling. A trust that
#    takes on failing schools would look bad on raw results for reasons
#    that are not its doing. The models already adjust for intake, and the
#    route each school took (sponsor-led academy, converter, free school,
#    maintained) enters as a fixed effect so that the trust term is
#    compared like with like.
#
# 2. SCHOOLS MOVE BETWEEN TRUSTS. GIAS publishes current membership only.
#    The DfE's school-level exclusions file, though, records the trust
#    each school was in IN EACH YEAR, and runs to 2024/25 - the panel's
#    last year. That is the membership used: a school-year belongs to the
#    trust named for it that year. A trust with more than one school that
#    year (any phase) is a multi-academy trust.
#
# 3. ONLY TRUST SCHOOL-YEARS SHOULD CARRY A TRUST EFFECT. Giving every
#    maintained school or single-academy trust a "trust" of its own makes
#    that group indistinguishable from the school, and the trust variance
#    then soaks up school variance. The trust term is therefore
#    (0 + in_mat | trust): it applies to school-years in a multi-academy
#    trust and is zero otherwise.
#
# Specification, for each outcome, fitted by ML on identical rows:
#
#   base   published terms + route + (1 | school) + (1 | region/LA)
#   trust  base + (0 + in_mat | trust)   trust crossed with LA, not nested
#
# Sources: DfE suspensions and permanent exclusions, school level (trust
# by year); GIAS group links extract (current trust and size, for the
# description of today's schools); the published panel; all Open
# Government Licence v3.0.
#
# Writes data/trust_effects.rds.
#
# Run from project root.
# ---------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(readr); library(lme4)
})

OUT   <- here::here("data", "trust_effects.rds")
LINKS <- here::here("data-raw", "gias", "alllinksdata20261029.csv")
ctrl  <- lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 50000))

# The local authorities later documents report on. Their trusts' effects
# are extracted with intervals; every trust's effect is kept as well.
SUSSEX <- c("East Sussex", "West Sussex", "Brighton and Hove")


# ---- 1. Trust membership, by school and year ----------------------------
read_links <- function() {
  # data-raw/ is not in git; fetch the extract this analysis was built on
  if (!file.exists(LINKS)) {
    dir.create(dirname(LINKS), showWarnings = FALSE, recursive = TRUE)
    utils::download.file(paste0("https://ea-edubase-api-prod.azurewebsites.net/",
                                "edubase/downloads/public/", basename(LINKS)),
                         LINKS, mode = "wb", quiet = TRUE)
  }
  read_csv(LINKS, locale = locale(encoding = "latin1"),
           col_types = cols(.default = col_character()), progress = FALSE) |>
    transmute(URN = as.integer(URN), group = `Group Name`, group_id = `Group ID`,
              type = `Group Type`, status = `Group Status`,
              joined = as.Date(`Joined date`, "%d/%m/%Y"),
              phase = `PhaseOfEducation (name)`)
}

#' Each school's trust (MAT or SAT), with its join date and the trust's size
trust_membership <- function(links) {
  mats <- links |> filter(type %in% c("Multi-academy trust", "Single-academy trust"))
  size <- mats |> filter(type == "Multi-academy trust") |>
    group_by(group_id) |>
    summarise(trust_schools = n_distinct(URN),
              trust_secondaries = n_distinct(URN[phase %in% c("Secondary", "All-through")]),
              .groups = "drop")
  mats |>
    arrange(URN, desc(joined)) |>
    group_by(URN) |> slice(1) |> ungroup() |>
    left_join(size, by = "group_id") |>
    mutate(trust_schools = ifelse(type == "Single-academy trust", 1L, trust_schools))
}

# Performance-table NFTYPE codes to the route a school took
route_of <- function(nftype) dplyr::case_when(
  nftype == "AC"                    ~ "Sponsor-led academy",
  nftype == "ACC"                   ~ "Converter academy",
  nftype %in% c("F", "UTC", "SS", "CTC") ~ "Free school, UTC or studio",
  nftype %in% c("CY", "FD", "VA", "VC")  ~ "Maintained",
  TRUE                              ~ "Other")


# ---- 2. Model data, as the published report builds it -------------------
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
           gor_name = factor(gor_name), LANAME = factor(LANAME),
           year_label = factor(year_label))
  contrasts(d$OFSTEDRATING_1) <- contr.treatment(levels(d$OFSTEDRATING_1))
  d
}

#' The trust each school was in, each year, from the exclusions file
#'
#' Counted over every phase, so a trust with one secondary and twelve
#' primaries is correctly a multi-academy trust.
trust_by_year <- function() {
  source(here::here("R", "00b_download_ees_api.R"))
  zip <- ees_path("exclusions_school")
  t <- read_csv(unz(zip, "data/exc_school.csv"),
                col_types = cols(.default = col_character()), progress = FALSE) |>
    transmute(year_label = paste0(substr(time_period, 1, 4), "-", substr(time_period, 5, 6)),
              URN = as.integer(school_urn), trust = trimws(trust_name)) |>
    filter(year_label %in% c("2021-22", "2022-23", "2023-24", "2024-25")) |>
    mutate(trust = ifelse(trust %in% c("", "z", "x", "NA") | is.na(trust), NA, trust))
  sizes <- t |> filter(!is.na(trust)) |>
    group_by(year_label, trust) |> summarise(trust_size = n_distinct(URN), .groups = "drop")
  t |> left_join(sizes, by = c("year_label", "trust")) |>
    distinct(year_label, URN, .keep_all = TRUE)
}

#' Attach the trust each school-year was actually in
attach_trusts <- function(d, tby) {
  d |>
    mutate(year_chr = as.character(year_label)) |>
    left_join(tby, by = c("year_chr" = "year_label", "URN")) |>
    mutate(
      route = route_of(NFTYPE),
      # Maintained foundation schools can sit in a foundation or
      # co-operative trust (Priory School in Lewes is one). That is not
      # an academy trust, so only academies count.
      trust_kind = dplyr::case_when(
        MINORGROUP != "Academy"        ~ "Maintained",
        !is.na(trust) & trust_size > 1 ~ "Multi-academy trust",
        !is.na(trust)                  ~ "Single-academy trust",
        TRUE                           ~ "Academy, trust not recorded"),
      in_mat = as.numeric(trust_kind == "Multi-academy trust"),
      # Non-MAT rows share one placeholder level; in_mat = 0 switches the
      # trust effect off for them, so the placeholder is never estimated
      trust_group = ifelse(in_mat == 1, trust, "(not in a multi-academy trust)"),
      route = factor(route, c("Maintained", "Converter academy", "Sponsor-led academy",
                              "Free school, UTC or studio", "Other"))) |>
    select(-year_chr)
}

RHS <- paste("log(PTFSM6CLA1A) + log(PERCTOT) + log(PNUMEAL) + ks2_c + ADMPOL_PT +",
             "gorard_segregation + remained_in_the_same_school +",
             "teachers_on_leadership_pay_range_percent +",
             "log(average_number_of_days_taken) + route +",
             "(1 | year_label) + (1 | OFSTEDRATING_1) + (1 | URN) + (1 | gor_name/LANAME)")
RHS_ABS <- paste("log(PTFSM6CLA1A) + log(PNUMEAL) + ks2_c + gorard_segregation + route +",
                 "(1 | year_label) + (1 | URN) + (1 | gor_name/LANAME)")

fit_pair <- function(d, lhs, rhs) {
  m0 <- lmer(as.formula(paste(lhs, "~", rhs)), data = d, REML = FALSE, control = ctrl)
  m1 <- lmer(as.formula(paste(lhs, "~", rhs, "+ (0 + in_mat | trust_group)")), data = d,
             REML = FALSE, control = ctrl)
  lrt <- 2 * (as.numeric(logLik(m1)) - as.numeric(logLik(m0)))
  list(m0 = m0, m1 = m1,
       test = tibble(lrt = lrt, p = 0.5 * pchisq(lrt, 1, lower.tail = FALSE),
                     bic0 = BIC(m0), bic1 = BIC(m1), n = nobs(m1)),
       vc = as.data.frame(VarCorr(m1)) |> transmute(level = grp, sd = sdcor),
       route = as.data.frame(summary(m1)$coefficients) |>
         tibble::rownames_to_column("term") |> filter(grepl("^route", term)) |>
         transmute(term = sub("^route", "", term), estimate = Estimate,
                   se = `Std. Error`, t = `t value`))
}

#' Every multi-academy trust's effect, with its interval and its schools
trust_table <- function(m, d, label) {
  re <- ranef(m, condVar = TRUE)$trust_group
  tibble(trust_group = rownames(re), effect = re[["in_mat"]],
         se = sqrt(attr(re, "postVar")[1, 1, ])) |>
    inner_join(d |> filter(trust_kind == "Multi-academy trust") |>
                 group_by(trust_group) |>
                 summarise(schools_in_data = n_distinct(URN), school_years = n(),
                           las = paste(sort(unique(as.character(LANAME))), collapse = "; "),
                           sponsor_share = mean(route == "Sponsor-led academy"),
                           trust_size = max(trust_size), .groups = "drop"),
               by = "trust_group") |>
    mutate(outcome = label, ci_lo = effect - 1.96 * se, ci_hi = effect + 1.96 * se,
           sig = ci_lo > 0 | ci_hi < 0, in_sussex = grepl(paste(SUSSEX, collapse = "|"), las))
}


build_trust_effects <- function() {
  panel <- readRDS(here::here("data", "panel_data.rds")) |>
    mutate(SCHNAME = gsub("\\s+", " ", trimws(SCHNAME)), URN = as.integer(URN))
  links <- read_links()
  mem   <- trust_membership(links)
  tby   <- trust_by_year()
  d <- model_data(panel) |> attach_trusts(tby) |> droplevels()

  message("School-years by trust status:")
  print(table(d$trust_kind))
  message(sprintf("  %d multi-academy trusts with at least one school-year",
                  n_distinct(d$trust_group[d$trust_kind == "Multi-academy trust"])))

  message("Fitting 8 models ...")
  fits <- list(
    `All pupils`        = fit_pair(d, "log(ATT8SCR)", RHS),
    `Disadvantaged`     = fit_pair(d |> filter(!is.na(ATT8SCR_FSM6CLA1A), ATT8SCR_FSM6CLA1A > 0),
                                   "log(ATT8SCR_FSM6CLA1A)", RHS),
    `Non-disadvantaged` = fit_pair(d |> filter(!is.na(ATT8SCR_NFSM6CLA1A), ATT8SCR_NFSM6CLA1A > 0),
                                   "log(ATT8SCR_NFSM6CLA1A)", RHS),
    `Absence (stage 1)` = fit_pair(d |> filter(!is.na(ks2_c)), "log(PERCTOT)", RHS_ABS))

  tests  <- bind_rows(lapply(names(fits), function(k) fits[[k]]$test |> mutate(outcome = k)))
  vcs    <- bind_rows(lapply(names(fits), function(k) fits[[k]]$vc |> mutate(outcome = k)))
  routes <- bind_rows(lapply(names(fits), function(k) fits[[k]]$route |> mutate(outcome = k)))
  trusts <- bind_rows(lapply(names(fits), function(k) trust_table(fits[[k]]$m1, d, k)))
  print(as.data.frame(tests), digits = 4)
  print(as.data.frame(vcs), digits = 3)
  print(as.data.frame(routes), digits = 3)

  # Who runs each Sussex school, now and through the panel years
  gias <- readRDS(here::here("data", "gias", "gias_establishments.rds"))
  sussex_now <- gias |>
    filter(la_name %in% SUSSEX, phase %in% c("Secondary", "All-through"), status == "Open",
           !grepl("Independent|Online|Further education|special|Special|Pupil referral|Alternative",
                  establishment_type)) |>
    transmute(URN = as.integer(urn), name = establishment_name, la = la_name,
              type_now = establishment_type) |>
    left_join(mem |> select(URN, trust_now = group, trust_type_now = type, joined,
                            trust_schools, trust_secondaries), by = "URN")
  sussex_years <- d |>
    filter(as.character(LANAME) %in% SUSSEX) |>
    select(URN, SCHNAME, LANAME, year_label, MINORGROUP, NFTYPE, route, RELCHAR,
           trust, trust_size, trust_group, trust_kind)

  out <- list(tests = tests, vcs = vcs, routes = routes, trusts = trusts,
              membership = mem, sussex_now = sussex_now, sussex_years = sussex_years,
              kind_counts = as.data.frame(table(d$trust_kind)),
              n_mats = n_distinct(d$trust_group[d$trust_kind == "Multi-academy trust"]),
              links_file = basename(LINKS), built_at = Sys.time())
  saveRDS(out, OUT)
  message("\nWrote ", OUT)
  invisible(out)
}

if (sys.nframe() == 0) build_trust_effects()
