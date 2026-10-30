# 18_district_effects.R - Is there a district level below the local authority?
# ---------------------------------------------------------------------------
# The published models place schools in local authorities within regions.
# In the two-tier counties that skips a level: East Sussex is five districts
# and boroughs - Hastings, Eastbourne, Lewes, Rother, Wealden - with very
# different towns in them, and the question from the county is whether
# those districts carry an effect of their own once intake is accounted
# for.
#
# Tested here by adding a district level, nested in the local authority,
# to the published specification:
#
#   published  ... + (1 | year) + (1 | Ofsted) + (1 | region/LA)
#   district   ... + (1 | year) + (1 | Ofsted) + (1 | region/LA/district)
#
# fitted for the three attainment outcomes and for stage 1 of the absence
# decomposition (expected absence from intake). Both versions are fitted
# by maximum likelihood on identical rows so they can be compared with a
# likelihood-ratio test. A variance cannot be negative, so the null sits
# on the boundary of the parameter space and the reference distribution is
# a 50:50 mixture of chi-squared(0) and chi-squared(1): the p-value is half
# the naive one.
#
# In a unitary authority the district IS the authority, so its district
# term duplicates its LA term. The district variance is identified from
# the two-tier counties, which is exactly the variation being asked about.
#
# District comes from each school's postcode, via postcodes.io (ONS
# postcode directory, Open Government Licence). Postcodes that have been
# retired are placed by reverse-geocoding the school's grid reference.
# Cached in data/cache/school_lad.rds.
#
# Writes data/district_effects.rds.
#
# Run from project root.
# ---------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(lme4); library(jsonlite); library(httr)
})

OUT   <- here::here("data", "district_effects.rds")
CACHE <- here::here("data", "cache", "school_lad.rds")
FOCUS <- "East Sussex"
ctrl  <- lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 50000))


# ---- 1. District for every school ---------------------------------------
pc_bulk <- function(pcs) {
  r <- POST("https://api.postcodes.io/postcodes",
            body = list(postcodes = as.list(pcs)), encode = "json")
  stop_for_status(r)
  res <- fromJSON(content(r, "text", encoding = "UTF-8"), simplifyVector = FALSE)$result
  bind_rows(lapply(res, function(x) tibble(
    postcode = x$query,
    lad = if (is.null(x$result)) NA_character_ else x$result$admin_district %||% NA_character_,
    lad_cd = if (is.null(x$result)) NA_character_ else x$result$codes$admin_district %||% NA_character_)))
}

geo_bulk <- function(lonlat) {
  r <- POST("https://api.postcodes.io/postcodes",
            body = list(geolocations = lapply(seq_len(nrow(lonlat)), function(i)
              list(longitude = lonlat$lon[i], latitude = lonlat$lat[i],
                   radius = 2000, limit = 1))),
            encode = "json")
  stop_for_status(r)
  res <- fromJSON(content(r, "text", encoding = "UTF-8"), simplifyVector = FALSE)$result
  bind_rows(lapply(seq_along(res), function(i) {
    h <- res[[i]]$result
    tibble(i = i,
           lad = if (length(h)) h[[1]]$admin_district %||% NA_character_ else NA_character_,
           lad_cd = if (length(h)) h[[1]]$codes$admin_district %||% NA_character_ else NA_character_)
  }))
}

`%||%` <- function(a, b) if (is.null(a)) b else a

school_districts <- function(schools) {
  if (file.exists(CACHE)) {
    have <- readRDS(CACHE)
    if (all(schools$URN %in% have$URN)) return(have)
  }
  pcs <- unique(na.omit(schools$POSTCODE))
  message("  looking up ", length(pcs), " postcodes ...")
  by_pc <- bind_rows(lapply(split(pcs, ceiling(seq_along(pcs) / 100)), function(b) {
    Sys.sleep(0.2); pc_bulk(b)
  }))
  out <- schools |> left_join(by_pc, by = c("POSTCODE" = "postcode"))

  miss <- out |> filter(is.na(lad), !is.na(easting))
  if (nrow(miss)) {
    message("  reverse-geocoding ", nrow(miss), " schools with retired postcodes ...")
    ll <- sf::st_as_sf(miss, coords = c("easting", "northing"), crs = 27700) |>
      sf::st_transform(4326) |> sf::st_coordinates()
    g <- bind_rows(lapply(split(seq_len(nrow(miss)), ceiling(seq_len(nrow(miss)) / 100)), function(ix) {
      Sys.sleep(0.2)
      geo_bulk(tibble(lon = ll[ix, 1], lat = ll[ix, 2])) |> mutate(i = ix[i])
    }))
    miss$lad <- g$lad[order(g$i)]; miss$lad_cd <- g$lad_cd[order(g$i)]
    out <- out |> rows_update(miss |> select(URN, lad, lad_cd), by = "URN")
  }
  dir.create(dirname(CACHE), showWarnings = FALSE, recursive = TRUE)
  saveRDS(out, CACHE)
  out
}


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

RHS_ATT <- paste("log(PTFSM6CLA1A) + log(PERCTOT) + log(PNUMEAL) + ks2_c + ADMPOL_PT +",
                 "gorard_segregation + remained_in_the_same_school +",
                 "teachers_on_leadership_pay_range_percent +",
                 "log(average_number_of_days_taken) + (1 | year_label) + (1 | OFSTEDRATING_1)")
RHS_ABS <- "log(PTFSM6CLA1A) + log(PNUMEAL) + ks2_c + gorard_segregation + (1 | year_label)"

# `school = TRUE` adds a school random intercept to both models. The
# published models have none, so a school's persistent over- or
# under-performance across its four years has nowhere to go but the
# lowest place level - and a district with three schools is then mostly
# those three schools. With the school term in, a district effect has to
# be shared by its schools beyond what each school is on its own.
fit_pair <- function(d, lhs, rhs, school = FALSE) {
  if (school) rhs <- paste(rhs, "+ (1 | URN)")
  f0 <- as.formula(paste(lhs, "~", rhs, "+ (1 | gor_name/LANAME)"))
  f1 <- as.formula(paste(lhs, "~", rhs, "+ (1 | gor_name/LANAME/lad)"))
  m0 <- lmer(f0, data = d, REML = FALSE, control = ctrl)
  m1 <- lmer(f1, data = d, REML = FALSE, control = ctrl)
  lrt <- 2 * (as.numeric(logLik(m1)) - as.numeric(logLik(m0)))
  vc <- as.data.frame(VarCorr(m1))
  list(m0 = m0, m1 = m1,
       test = tibble(lrt = lrt, p = 0.5 * pchisq(lrt, df = 1, lower.tail = FALSE),
                     aic0 = AIC(m0), aic1 = AIC(m1), bic0 = BIC(m0), bic1 = BIC(m1),
                     n = nobs(m1)),
       vc = vc |> transmute(level = grp, sd = sdcor, var = vcov,
                            share = vcov / sum(vcov)))
}

#' District, LA and region effects for the focus county, with intervals
focus_effects <- function(m, d, outcome_label) {
  re <- ranef(m, condVar = TRUE)
  grab <- function(tab) {
    tibble(key = rownames(tab), effect = tab[["(Intercept)"]],
           se = sqrt(attr(tab, "postVar")[1, 1, ]))
  }
  dist_tab <- re[[grep("^lad:", names(re), value = TRUE)]]
  la_tab   <- re[["LANAME:gor_name"]]
  dl <- grab(dist_tab) |>
    filter(grepl(paste0(":", FOCUS, ":"), key)) |>
    mutate(lad = sub(":.*$", "", key))
  la <- grab(la_tab) |> filter(grepl(paste0("^", FOCUS, ":"), key))
  dl |>
    mutate(la_effect = la$effect, outcome = outcome_label,
           ci_lo = effect - 1.96 * se, ci_hi = effect + 1.96 * se,
           sig = ci_lo > 0 | ci_hi < 0,
           combined = effect + la_effect) |>
    select(outcome, lad, effect, se, ci_lo, ci_hi, sig, la_effect, combined)
}

#' Every two-tier district's effect, for the national picture
all_district_effects <- function(m, label) {
  re <- ranef(m, condVar = TRUE)
  tab <- re[[grep("^lad:", names(re), value = TRUE)]]
  tibble(key = rownames(tab), effect = tab[["(Intercept)"]],
         se = sqrt(attr(tab, "postVar")[1, 1, ])) |>
    separate(key, c("lad", "la", "region"), sep = ":", extra = "merge") |>
    group_by(la) |> mutate(n_lad = n()) |> ungroup() |>
    mutate(outcome = label, sig = abs(effect) > 1.96 * se)
}


build_district_effects <- function() {
  panel <- readRDS(here::here("data", "panel_data.rds")) |>
    mutate(SCHNAME = gsub("\\s+", " ", trimws(SCHNAME)))
  d <- model_data(panel)

  message("Districts ...")
  sch <- d |> distinct(URN, POSTCODE, easting, northing) |>
    group_by(URN) |> slice(1) |> ungroup() |>
    mutate(POSTCODE = toupper(trimws(POSTCODE)))
  lad <- school_districts(sch)
  d <- d |> left_join(lad |> select(URN, lad), by = "URN")
  message(sprintf("  %d of %d school-years placed in a district",
                  sum(!is.na(d$lad)), nrow(d)))
  d <- d |> filter(!is.na(lad)) |> mutate(lad = factor(lad)) |> droplevels()

  es_check <- d |> filter(LANAME == FOCUS) |> distinct(SCHNAME, lad) |> arrange(lad, SCHNAME)

  run_all <- function(school) {
    message(sprintf("Models, %s school term (8 fits) ...", if (school) "with" else "without"))
    list(
      `All pupils`        = fit_pair(d, "log(ATT8SCR)", RHS_ATT, school),
      `Disadvantaged`     = fit_pair(d |> filter(!is.na(ATT8SCR_FSM6CLA1A), ATT8SCR_FSM6CLA1A > 0),
                                     "log(ATT8SCR_FSM6CLA1A)", RHS_ATT, school),
      `Non-disadvantaged` = fit_pair(d |> filter(!is.na(ATT8SCR_NFSM6CLA1A), ATT8SCR_NFSM6CLA1A > 0),
                                     "log(ATT8SCR_NFSM6CLA1A)", RHS_ATT, school),
      `Absence (stage 1)` = fit_pair(d |> filter(!is.na(ks2_c)), "log(PERCTOT)", RHS_ABS, school))
  }
  collect <- function(fits, spec) list(
    tests = bind_rows(lapply(names(fits), function(k) fits[[k]]$test |> mutate(outcome = k, spec = spec))),
    vcs   = bind_rows(lapply(names(fits), function(k) fits[[k]]$vc |> mutate(outcome = k, spec = spec))),
    focus = bind_rows(lapply(names(fits), function(k) focus_effects(fits[[k]]$m1, d, k) |> mutate(spec = spec))),
    every = bind_rows(lapply(names(fits), function(k) all_district_effects(fits[[k]]$m1, k) |> mutate(spec = spec))))

  fits  <- run_all(FALSE)
  fitsS <- run_all(TRUE)
  a <- collect(fits, "Published specification")
  b <- collect(fitsS, "With a school term")
  tests <- bind_rows(a$tests, b$tests); vcs <- bind_rows(a$vcs, b$vcs)
  focus <- bind_rows(a$focus, b$focus); every <- bind_rows(a$every, b$every)

  # LA effect for the focus county before and after the district level, to
  # see how much of it the districts take
  la_shift <- bind_rows(lapply(names(fits), function(k) {
    g <- function(m) { t <- ranef(m)[["LANAME:gor_name"]]
      t[grepl(paste0("^", FOCUS, ":"), rownames(t)), "(Intercept)"] }
    tibble(outcome = k, la_before = g(fits[[k]]$m0), la_after = g(fits[[k]]$m1))
  }))

  # Each East Sussex school's own persistent effect, from the school-term
  # model, so the document can show what a district effect is made of
  school_eff <- bind_rows(lapply(names(fitsS), function(k) {
    re <- ranef(fitsS[[k]]$m1)$URN
    tibble(URN = as.integer(rownames(re)), school_effect = re[["(Intercept)"]], outcome = k)
  })) |>
    inner_join(d |> filter(LANAME == FOCUS) |> distinct(URN, SCHNAME, lad) |>
                 mutate(URN = as.integer(URN)), by = "URN")

  print(as.data.frame(tests), digits = 4)
  print(as.data.frame(vcs |> filter(grepl("lad|LANAME|gor_name", level))), digits = 3)
  print(as.data.frame(focus), digits = 3)

  # ---- 3. Inside the county alone: does value added differ by district?
  # Four years of the published value-added residual, averaged per school,
  # set against district. 26 schools, so this can only find a large effect.
  es_va <- panel |>
    filter(LANAME == FOCUS, MINORGROUP %in% c("Academy", "Maintained school"),
           !is.na(residual_ATT8SCR_imputed)) |>
    left_join(lad |> select(URN, lad), by = "URN") |>
    group_by(URN, SCHNAME, lad) |>
    summarise(va = mean(residual_ATT8SCR_imputed), absence = mean(PERCTOT, na.rm = TRUE),
              fsm = mean(PTFSM6CLA1A, na.rm = TRUE), years = n(), .groups = "drop")
  aov_va  <- summary(aov(va ~ lad, data = es_va))[[1]]
  kw_va   <- kruskal.test(va ~ factor(lad), data = es_va)

  out <- list(tests = tests, vcs = vcs, focus = focus, every = every,
              la_shift = la_shift, school_eff = school_eff,
              es_check = es_check, es_va = es_va,
              aov_va = aov_va, kw_va = kw_va,
              n_lad = n_distinct(d$lad), n_la = n_distinct(d$LANAME),
              built_at = Sys.time())
  saveRDS(out, OUT)
  message("\nWrote ", OUT)
  invisible(out)
}

if (sys.nframe() == 0) build_district_effects()
