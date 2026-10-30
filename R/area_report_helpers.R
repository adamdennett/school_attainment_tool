# area_report_helpers.R - Shared pieces for the Sussex area documents
# ---------------------------------------------------------------------------
# The West Sussex, Mid Sussex and all-Sussex documents draw the same kinds
# of table and chart as the East Sussex one. They are defined here once,
# so a fix to one is a fix to all. Each takes an area object written by
# R/17_area_analysis.R (and, where needed, the trust and district objects
# from R/18 and R/19). Sourced by the .qmd files, not run on its own.
# ---------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(ggplot2); library(scales)
  library(ggrepel); library(knitr); library(kableExtra); library(sf)
})

INK    <- "#1a1a2e"; PURPLE <- "#361a54"; TEAL <- "#2e6260"; PINK <- "#e16fca"
WARM   <- "#f56300"; BAD <- "#b2182b"; GREY <- "#666688"; LIGHT <- "#c9c9d6"
AREA_COL <- c("East Sussex" = TEAL, "West Sussex" = "#1f77b4", "Mid Sussex" = "#7b61c9",
              "Brighton and Hove" = PINK, "South East" = "#8a8aa3", "England" = INK)

theme_note <- function(base = 12) {
  theme_minimal(base_size = base) +
    theme(plot.title = element_text(face = "bold", colour = PURPLE),
          plot.subtitle = element_text(colour = GREY, size = rel(0.9)),
          plot.title.position = "plot",
          axis.title = element_text(colour = GREY, size = rel(0.9)),
          panel.grid.minor = element_blank(),
          legend.position = "bottom")
}

kt <- function(df, ...) knitr::kable(df, format = "html", ...) |>
  kableExtra::kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                            full_width = FALSE, font_size = 12)

yr  <- function(x) paste0(substr(x, 1, 4), "/", substr(x, 5, 6))
ord <- function(n) { n <- round(n); s <- ifelse(n %% 100 %in% 11:13, "th",
  c("th", "st", "nd", "rd", rep("th", 6))[n %% 10 + 1]); paste0(n, s) }
pts <- function(x, d = 1) sprintf(paste0("%.", d, "f"), x)
pp  <- pts
pct <- function(x) 100 * (exp(x) - 1)
sgn <- function(x, d = 1) sprintf(paste0("%+.", d, "f"), x)

tidy_trust <- function(x) ifelse(is.na(x), NA_character_,
  sub(" Limited$", "", gsub("\\bOf\\b", "of", gsub("\\bAnd\\b", "and",
    tools::toTitleCase(tolower(x))))))


# ---- Who runs each school ----------------------------------------------
#' Successor URNs from the GIAS links extract: a school that re-opened as
#' a fresh start or re-converted has a new URN; this maps old to new
successors <- function() {
  f <- list.files(here::here("data-raw", "gias"), "^links_edubasealldata.*csv$", full.names = TRUE)
  if (!length(f)) return(tibble(URN = integer(), URN_now = integer()))
  readr::read_csv(tail(sort(f), 1), locale = readr::locale(encoding = "latin1"),
                  col_types = readr::cols(.default = readr::col_character()), progress = FALSE) |>
    filter(grepl("Successor", LinkType)) |>
    transmute(URN = as.integer(URN), URN_now = as.integer(LinkURN)) |>
    distinct(URN, .keep_all = TRUE)
}

#' One row per school in the area: who ran it in 2024/25 (DfE exclusions
#' file) and who runs it now (GIAS), with value added and absence
gov_table <- function(X, TR, DE, council = "the county council") {
  S <- X$schools
  succ <- successors()
  now <- TR$sussex_now |> filter(la %in% X$area$las)
  maint <- paste("Maintained by", council)
  TR$sussex_years |>
    filter(URN %in% as.integer(S$URN), year_label == "2024-25") |>
    transmute(URN, SCHNAME, route = as.character(route), trust_kind, trust_size,
              run_by = case_when(
                trust_kind == "Maintained" ~ maint,
                trust_kind == "Single-academy trust" ~ "Single-academy trust",
                trust_kind == "Multi-academy trust" ~ tidy_trust(trust),
                TRUE ~ "Academy, trust not recorded for 2024/25")) |>
    left_join(succ, by = "URN") |>
    mutate(URN_now = ifelse(!is.na(URN_now) & !URN %in% now$URN, URN_now, URN)) |>
    left_join(now |> transmute(URN_now = URN, name_now = name, type_now,
                               trust_now = tidy_trust(trust_now), trust_type_now),
              by = "URN_now") |>
    mutate(run_by_now = case_when(
             trust_type_now %in% "Single-academy trust" ~ "Single-academy trust",
             !is.na(trust_now) ~ trust_now,
             grepl("Community|Foundation|Voluntary", type_now) ~ maint,
             !is.na(type_now) ~ "Single-academy trust",
             TRUE ~ "Closed"),
           changed = run_by_now != run_by &
             !(run_by == "Single-academy trust" & run_by_now == "Single-academy trust")) |>
    left_join(S |> transmute(URN = as.integer(URN), district, religion, pupils, absence, fsm),
              by = "URN") |>
    left_join(DE$sussex_va |> transmute(URN = as.integer(URN), va), by = "URN")
}

fig_governance <- function(GOV, title = "Schools by who ran them in 2024/25") {
  ord_g <- GOV |>
    mutate(grp = case_when(grepl("^Maintained", run_by) ~ 1,
                           trust_kind == "Multi-academy trust" ~ 2, TRUE ~ 3)) |>
    group_by(run_by, grp) |> summarise(n = n(), .groups = "drop") |>
    arrange(grp, desc(n), run_by) |> pull(run_by)
  wrap_lab <- function(x) sapply(x, function(s) paste(strwrap(s, 24), collapse = "\n"))
  GOV |>
    mutate(run_by = factor(wrap_lab(run_by), wrap_lab(ord_g)),
           SCHNAME = reorder(SCHNAME, va),
           route = factor(route, c("Maintained", "Converter academy", "Sponsor-led academy",
                                   "Free school, UTC or studio"))) |>
    ggplot(aes(va, SCHNAME)) +
    geom_vline(xintercept = 0, colour = GREY) +
    geom_segment(aes(x = 0, xend = va, yend = SCHNAME), colour = LIGHT) +
    geom_point(aes(fill = district, shape = route), size = 3.2, colour = INK, stroke = 0.3) +
    facet_grid(run_by ~ ., scales = "free_y", space = "free_y", switch = "y") +
    scale_shape_manual(values = c(21, 22, 24, 23), name = NULL, drop = FALSE) +
    scale_fill_brewer(palette = "Dark2", name = NULL) +
    guides(fill = guide_legend(override.aes = list(shape = 21, size = 3.5), nrow = 1),
           shape = guide_legend(override.aes = list(fill = LIGHT, size = 3.5), nrow = 1)) +
    scale_y_discrete(position = "right") +
    labs(x = "Value added, mean 2021/22 to 2024/25 (Attainment 8 points)", y = NULL, title = title) +
    theme_note(10) +
    theme(strip.placement = "outside",
          strip.text.y.left = element_text(angle = 0, hjust = 1, face = "bold",
                                           colour = PURPLE, size = 8),
          panel.spacing.y = unit(2, "pt"), legend.box = "vertical",
          panel.grid.major.y = element_blank())
}


# ---- The map ------------------------------------------------------------
map_area <- function(X, GOV, LADS, height = 560) {
  S <- X$schools
  pal <- leaflet::colorNumeric(c("#2e6260", "#f2e6b6", "#b2182b"), domain = S$absence)
  lads <- LADS |> filter(if (is.null(X$area$lads)) la %in% X$area$las else lad %in% X$area$lads)
  lad_c <- suppressWarnings(sf::st_coordinates(sf::st_point_on_surface(lads)))
  m_dat <- S |>
    mutate(URN = as.integer(URN)) |>
    left_join(GOV |> select(URN, run_by, run_by_now, route), by = "URN") |>
    mutate(gov = case_when(grepl("^Maintained", run_by) ~ "Maintained",
                           run_by == "Single-academy trust" | grepl("not recorded", run_by) ~
                             "Single-academy trust or not recorded",
                           route == "Sponsor-led academy" ~ "Multi-academy trust, sponsored",
                           TRUE ~ "Multi-academy trust, converter or free school"),
           tip = sprintf("<b>%s</b> (%s)<br>Absence %.1f%%, FSM %.0f%%, Attainment 8 %.1f<br>Run by (2024/25): %s%s",
                         SCHNAME, district, absence, fsm, att8, run_by,
                         ifelse(!is.na(run_by_now) & run_by_now != run_by,
                                paste0("<br>Now: ", run_by_now), "")))
  gpal <- leaflet::colorFactor(c("#2e6260", "#993bff", "#f56300", "#8a8aa3"),
    levels = c("Maintained", "Multi-academy trust, converter or free school",
               "Multi-academy trust, sponsored", "Single-academy trust or not recorded"))
  leaflet::leaflet(m_dat, height = height, options = leaflet::leafletOptions(scrollWheelZoom = FALSE)) |>
    leaflet::addProviderTiles("Esri.WorldGrayCanvas") |>
    leaflet::addPolygons(data = lads, color = PURPLE, weight = 1.5, fill = TRUE,
                         fillOpacity = 0.03, fillColor = PURPLE, label = ~lad) |>
    leaflet::addLabelOnlyMarkers(lng = lad_c[, 1], lat = lad_c[, 2], label = lads$lad,
      labelOptions = leaflet::labelOptions(noHide = TRUE, textOnly = TRUE,
        style = list(color = PURPLE, "font-weight" = "bold", "font-size" = "12px"))) |>
    leaflet::addCircleMarkers(~lon, ~lat, radius = ~sqrt(pupils) / 4, group = "Absence",
                              color = "white", weight = 1, fillOpacity = 0.9,
                              fillColor = ~pal(absence), label = ~lapply(tip, htmltools::HTML)) |>
    leaflet::addCircleMarkers(~lon, ~lat, radius = ~sqrt(pupils) / 4, group = "Who runs it",
                              color = "white", weight = 1, fillOpacity = 0.9,
                              fillColor = ~gpal(gov), label = ~lapply(tip, htmltools::HTML)) |>
    leaflet::addLegend("bottomright", pal = pal, values = ~absence, group = "Absence",
                       title = "Absence %", opacity = 0.9) |>
    leaflet::addLegend("bottomright", pal = gpal, values = ~gov, group = "Who runs it",
                       title = "Who runs it (2024/25)", opacity = 0.9) |>
    leaflet::addLayersControl(baseGroups = c("Absence", "Who runs it"),
                              options = leaflet::layersControlOptions(collapsed = FALSE)) |>
    # Show only the legend for the chosen layer, told apart by title
    htmlwidgets::onRender("function(el, x) {
      function show(name) {
        el.querySelectorAll('.info.legend').forEach(function(l) {
          var isAbs = l.innerText.indexOf('Absence %') !== -1;
          l.style.display = ((name === 'Absence') === isAbs) ? '' : 'none'; }); }
      this.on('baselayerchange', function(e) { show(e.name); });
      show('Absence'); }")
}


# ---- Tables and charts --------------------------------------------------
district_table <- function(S) {
  S |>
    group_by(District = district) |>
    summarise(Schools = n(), Pupils = sum(pupils),
              `FSM %` = mean(fsm), `Absence %` = mean(absence),
              `Persistent absence %` = mean(persistent),
              `KS2 (centred)` = mean(ks2_c, na.rm = TRUE),
              `Attainment 8` = mean(att8),
              `Attainment 8, disadvantaged` = mean(att8_dis, na.rm = TRUE),
              .groups = "drop") |>
    arrange(desc(`FSM %`)) |>
    mutate(across(where(is.double), ~round(.x, 1)),
           Pupils = format(Pupils, big.mark = ",")) |>
    kt(align = "lrrrrrrrr")
}

school_table <- function(S) {
  S |>
    arrange(district, SCHNAME) |>
    transmute(School = SCHNAME, District = district,
              Type = ifelse(type == "Academy", "Academy", "Maintained"),
              Pupils = pupils, `FSM %` = round(fsm, 1),
              `Absence %` = round(absence, 1), `Persistent %` = round(persistent, 1),
              `KS2` = round(ks2_c, 1), `EHCP %` = round(sen_ehcp, 1),
              `Att 8` = round(att8, 1), `Att 8 disadv.` = round(att8_dis, 1),
              Ofsted = ofsted) |>
    kt(align = "llllrrrrrrrl")
}

COMP_ORDER <- c("Absence", "Prior attainment (KS2)", "Disadvantage (% FSM)",
                "English as an additional language", "Admissions (selection)",
                "Segregation (LA)", "Teacher workforce", "Ofsted rating",
                "Region", "Local authority effect", "Year mix",
                "Unexplained (school residual)")

fig_decomp <- function(dec, areas, groups = c("All pupils", "Disadvantaged", "Non-disadvantaged")) {
  dec <- dec |> filter(la %in% areas, group %in% groups)
  lab <- dec |> distinct(la, group, la_att8, nat_att8) |>
    mutate(la = factor(la, areas),
           txt = sprintf("%.1f vs England %.1f\ntotal %+.1f", la_att8, nat_att8, la_att8 - nat_att8))
  dec |>
    mutate(component = factor(component, rev(COMP_ORDER)), la = factor(la, areas),
           dir = ifelse(points < 0, "Pulls below England", "Lifts above England")) |>
    ggplot(aes(points, component, fill = dir)) +
    geom_col(width = 0.7) +
    geom_vline(xintercept = 0, colour = GREY) +
    geom_label(data = lab, aes(x = Inf, y = 7, label = txt), inherit.aes = FALSE,
               hjust = 1.02, size = 2.8, colour = INK, fill = "white", label.size = 0) +
    facet_grid(la ~ group) +
    scale_fill_manual(values = c("Pulls below England" = BAD, "Lifts above England" = TEAL), name = NULL) +
    labs(x = "Approximate Attainment 8 points", y = NULL,
         title = "What separates each area's results from England's",
         subtitle = "Each bar is one factor's contribution to the difference") +
    theme_note(10.5) + theme(strip.text = element_text(face = "bold"))
}

decomp_table <- function(dec, area) {
  dec |>
    filter(la == area) |>
    mutate(component = case_when(component %in% c("Region", "Local authority effect") ~ "Place (region and local authority)",
                                 component %in% c("Segregation (LA)", "Year mix", "Admissions (selection)") ~ "Other model terms",
                                 TRUE ~ component)) |>
    group_by(group, component) |> summarise(points = sum(points), .groups = "drop") |>
    pivot_wider(names_from = group, values_from = points) |>
    arrange(`All pupils`) |>
    mutate(across(where(is.numeric), ~sprintf("%+.2f", .x))) |>
    rename(Factor = component) |>
    kt(align = "lrrr")
}

fig_caterpillar <- function(eff, hl, group = "Disadvantaged", title = NULL) {
  cd <- eff |> filter(.data$group == !!group) |>
    mutate(h = ifelse(la %in% hl, la, "Other"), la_o = reorder(la, effect))
  ggplot(cd, aes(la_o, effect)) +
    geom_errorbar(aes(ymin = ci_lo, ymax = ci_hi), colour = LIGHT, width = 0, linewidth = 0.3) +
    geom_point(colour = LIGHT, size = 1.2) +
    geom_point(data = filter(cd, h != "Other"), aes(fill = h), shape = 21, size = 3.5, colour = "white") +
    geom_hline(yintercept = 0, linetype = "dashed", colour = GREY) +
    geom_text_repel(data = filter(cd, h != "Other"), aes(label = la), size = 3.4, nudge_y = 0.06,
                    segment.colour = GREY) +
    scale_fill_manual(values = AREA_COL, guide = "none") +
    labs(x = "Local authorities, ordered", y = "Effect (log scale; 0.01 is about 1%)", title = title) +
    theme_note() +
    theme(axis.text.x = element_blank(), panel.grid.major.x = element_blank())
}

fig_abs_series <- function(ser, areas, title) {
  ser |>
    filter(area %in% c(areas, "England")) |>
    mutate(year = yr(time_period), area = factor(area, intersect(names(AREA_COL), c(areas, "England")))) |>
    ggplot(aes(year, abs_rate, colour = area, group = area)) +
    geom_line(linewidth = 1) + geom_point(size = 2) +
    scale_colour_manual(values = AREA_COL, name = NULL) +
    labs(x = NULL, y = "Overall absence, % of sessions", title = title) +
    theme_note() + theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

fig_reasons <- function(rs, areas, title) {
  w <- rs |> select(area, reason, pp) |> pivot_wider(names_from = area, values_from = pp)
  top <- w |> arrange(desc(England)) |> slice_head(n = 10) |> pull(reason)
  rs |> filter(reason %in% top, area %in% c(areas, "England")) |>
    mutate(reason = factor(reason, rev(top)),
           area = factor(area, intersect(names(AREA_COL), c(areas, "England")))) |>
    ggplot(aes(pp, reason, fill = area)) +
    geom_col(position = position_dodge(width = 0.8), width = 0.75) +
    scale_fill_manual(values = AREA_COL, name = NULL) +
    labs(x = "% of possible sessions", y = NULL, title = title) +
    theme_note()
}

reason_gap_table <- function(rs, area) {
  w <- rs |> select(area, reason, pp) |> pivot_wider(names_from = area, values_from = pp)
  w$gap <- w[[area]] - w$England
  tot <- sum(w$gap)
  w |> filter(abs(gap) >= 0.01) |> arrange(desc(abs(gap))) |>
    transmute(Reason = reason, !!area := sprintf("%.2f", .data[[area]]),
              England = sprintf("%.2f", England), Gap = sprintf("%+.2f", gap),
              `Share of gap` = sprintf("%.0f%%", 100 * gap / tot)) |>
    kt(align = "lrrrr")
}

fig_plane <- function(js, focus_urns, title = "Attendance and attainment go together school by school") {
  sel <- js |> filter(URN %in% focus_urns)
  xl <- quantile(js$resid_abs_pp, c(0.005, 0.995)); yl <- quantile(js$vadd_M2, c(0.005, 0.995))
  xl <- c(min(xl[1], -4), max(xl[2], 4.5)); yl <- c(min(yl[1], -8), max(yl[2], 9))
  ggplot(js, aes(resid_abs_pp, vadd_M2)) +
    geom_point(colour = LIGHT, size = 0.8, alpha = 0.6) +
    geom_hline(yintercept = 0, colour = GREY) + geom_vline(xintercept = 0, colour = GREY) +
    geom_point(data = sel, fill = TEAL, shape = 21, colour = "white", size = 3.4) +
    geom_text_repel(data = sel, aes(label = SCHNAME), size = 2.6, max.overlaps = 50,
                    segment.colour = GREY, colour = INK) +
    annotate("text", x = xl[1], y = yl[2], hjust = 0, vjust = 1, size = 3, colour = TEAL,
             label = "Adds value, attendance\nbetter than intake predicts") +
    annotate("text", x = xl[2], y = yl[2], hjust = 1, vjust = 1, size = 3, colour = WARM,
             label = "Adds value despite\nworse attendance") +
    annotate("text", x = xl[2], y = yl[1], hjust = 1, vjust = 0, size = 3, colour = BAD,
             label = "Below expectation,\nattendance worse") +
    annotate("text", x = xl[1], y = yl[1], hjust = 0, vjust = 0, size = 3, colour = PURPLE,
             label = "Below expectation despite\nbetter attendance") +
    coord_cartesian(xlim = xl, ylim = yl) +
    labs(x = "Absence above what the intake predicts (percentage points)",
         y = "Value added, attendance counted in (Attainment 8 points)", title = title) +
    theme_note()
}

scenario_table <- function(sc) {
  sc |>
    filter(gain_all > 0 | gain_fsm > 0) |>
    arrange(desc(gain_all)) |>
    transmute(School = SCHNAME, District = district,
              `Absence %` = round(absence, 1), `FSM %` = round(fsm, 1),
              `Gain, all pupils` = sprintf("%+.1f", gain_all),
              `Gain, disadvantaged` = sprintf("%+.1f", gain_dis),
              `Gain from FSM to area average` = sprintf("%+.1f", gain_fsm)) |>
    kt(align = "llrrrrr")
}

va_tab <- function(va, col) {
  va |>
    select(school, year_label, v = all_of(col)) |>
    filter(!is.na(v)) |>
    group_by(school) |> mutate(Mean = mean(v), Years = n()) |> ungroup() |>
    mutate(year_label = sub("-", "/", year_label)) |>
    pivot_wider(names_from = year_label, values_from = v) |>
    arrange(desc(Mean)) |>
    relocate(Mean, Years, .after = last_col())
}
fmt_va <- function(t) {
  num <- names(t)[sapply(t, is.numeric) & names(t) != "Years"]
  t |> mutate(across(all_of(num), ~ifelse(is.na(.x), "", sprintf("%+.1f", .x)))) |>
    rename(School = school)
}

fig_district_effects <- function(DE, la_name, title) {
  DE$focus |> filter(la == la_name) |>
    mutate(spec = factor(spec, c("Published specification", "With a school term")),
           outcome = factor(outcome, c("All pupils", "Disadvantaged", "Non-disadvantaged", "Absence (stage 1)")),
           lad = factor(lad, rev(sort(unique(lad))))) |>
    filter(outcome != "Non-disadvantaged") |>
    ggplot(aes(pct(effect), lad, colour = spec)) +
    geom_vline(xintercept = 0, colour = GREY) +
    geom_errorbarh(aes(xmin = pct(ci_lo), xmax = pct(ci_hi)), height = 0,
                   position = position_dodge(width = 0.6), linewidth = 0.8) +
    geom_point(position = position_dodge(width = 0.6), size = 2.6) +
    facet_wrap(~outcome, scales = "free_x") +
    scale_colour_manual(values = c(WARM, TEAL), name = NULL) +
    labs(x = "District effect, %", y = NULL, title = title) +
    theme_note()
}

fig_school_effects <- function(DE, urns, title) {
  se <- DE$school_eff |> filter(URN %in% urns)
  ord_s <- se |> filter(outcome == "All pupils") |> arrange(school_effect) |> pull(SCHNAME)
  se |>
    filter(outcome %in% c("All pupils", "Absence (stage 1)")) |>
    mutate(outcome = factor(outcome, c("All pupils", "Absence (stage 1)"),
                            c("Attainment 8, all pupils (%)", "Absence beyond intake (%)")),
           SCHNAME = factor(SCHNAME, unique(ord_s))) |>
    ggplot(aes(pct(school_effect), SCHNAME)) +
    geom_vline(xintercept = 0, colour = GREY) +
    geom_segment(aes(x = 0, xend = pct(school_effect), yend = SCHNAME), colour = LIGHT) +
    geom_point(aes(colour = lad), size = 2.4) +
    facet_wrap(~outcome, scales = "free_x") +
    scale_colour_brewer(palette = "Dark2", name = NULL) +
    labs(x = "School effect, %", y = NULL, title = title) +
    theme_note(10)
}

fig_trusts <- function(TE, title) {
  TE |>
    filter(outcome %in% c("All pupils", "Disadvantaged", "Absence (stage 1)")) |>
    mutate(label = sprintf("%s (%d)", name, schools_in_data),
           outcome = factor(outcome, c("All pupils", "Disadvantaged", "Absence (stage 1)")),
           sigc = ifelse(sig, "Distinguishable from average", "Not distinguishable")) |>
    group_by(label) |> mutate(o = effect[outcome == "All pupils"][1]) |> ungroup() |>
    ggplot(aes(pct(effect), reorder(label, o), colour = sigc)) +
    geom_vline(xintercept = 0, colour = GREY) +
    geom_errorbarh(aes(xmin = pct(ci_lo), xmax = pct(ci_hi)), height = 0) +
    geom_point(size = 2.4) +
    facet_wrap(~outcome, scales = "free_x") +
    scale_colour_manual(values = c("Distinguishable from average" = WARM,
                                   "Not distinguishable" = TEAL), name = NULL) +
    labs(x = "Trust effect, %", y = NULL, title = title) +
    theme_note(10.5)
}
