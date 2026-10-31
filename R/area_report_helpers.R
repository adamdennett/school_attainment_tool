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

#' "a", "a and b", "a, b and c"
and_list <- function(x) {
  x <- as.character(x)
  if (length(x) <= 1) return(paste(x, collapse = ""))
  paste(paste(head(x, -1), collapse = ", "), "and", tail(x, 1))
}

#' A district's rank among every district in England with five or more
#' schools (1 = highest; lowest with dir = 1). A smaller district is
#' placed among them. list(r = rank, n = number ranked)
district_rank <- function(lad_profiles, year, lad, col, dir = -1) {
  lp <- lad_profiles[lad_profiles$year_label == year, ]
  v <- lp[[col]][lp$lad == lad]
  others <- lp[lp$schools >= 5 & lp$lad != lad, ]
  list(r = 1 + sum(dir * others[[col]] < dir * v, na.rm = TRUE), n = nrow(others) + 1)
}

#' What a set of schools adds beyond the model's expectation, as a phrase:
#' "add 1.4 points beyond what the model expects", or "do about what the
#' model expects" when it rounds to zero
add_take <- function(u, who = "") {
  if (abs(u) < 0.05) return(paste0("do about what the model expects", who))
  sprintf("%s %.1f points%s beyond what the model expects", ifelse(u > 0, "add", "take"), abs(u), who)
}

#' An outcome as it reads in a sentence: "Absence (stage 1)" -> "absence"
effect_word <- function(outcome) c("All pupils" = "all pupils", "Disadvantaged" = "disadvantaged pupils",
  "Non-disadvantaged" = "non-disadvantaged pupils", "Absence (stage 1)" = "absence")[outcome]

#' A district's document: the template's, or a hand-written one
district_doc <- function(lad, hand_written = "Mid Sussex")
  ifelse(lad %in% hand_written, paste0(slugify_doc(lad), ".html"),
         paste0("district_", slugify_doc(lad), ".html"))

#' A document's file name from an area's name: "East Sussex" -> "east_sussex"
slugify_doc <- function(x) gsub("[^a-z]+", "_", tolower(x))

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
# The variables the map's dropdown offers. `var` names a column of the
# table map_area() builds; `scale` is seq (magnitude), div (centred on
# zero; `good` says which end is good) or cat (categories, coloured from
# MAP_CATS). Absence comes first and is the default.
map_vars <- function(abs_last) {
  rs <- paste0(", autumn and spring ", yr(abs_last))
  tribble(
    ~var,          ~label,                                                  ~group,       ~scale, ~good,  ~dp, ~unit,
    "absence",     "Overall absence, 2024/25",                              "Attendance", "seq",  "",     1,   "%",
    "persistent",  "Persistent absence, 2024/25",                           "Attendance", "seq",  "",     1,   "%",
    "abs_excess",  "Absence beyond what the intake predicts, 2021/22 to 2024/25", "Attendance", "div", "low", 1, " pp",
    "illness",     paste0("Illness absence", rs),                           "Attendance", "seq",  "",     1,   "%",
    "unauth",      paste0("Unauthorised absence", rs),                      "Attendance", "seq",  "",     1,   "%",
    "reduced",     paste0("Reduced timetables", rs),                        "Attendance", "seq",  "",     2,   "%",
    "susp_rate",   "Suspensions per 100 pupils, 2024/25",                   "Attendance", "seq",  "",     1,   "",
    "att8",        "Attainment 8, all pupils, 2024/25",                     "Results",    "seq",  "",     1,   "",
    "att8_dis",    "Attainment 8, disadvantaged pupils, 2024/25",           "Results",    "seq",  "",     1,   "",
    "att8_non",    "Attainment 8, other pupils, 2024/25",                   "Results",    "seq",  "",     1,   "",
    "va_all",      "Value added, all pupils, 2021/22 to 2024/25",           "Results",    "div",  "high", 1,   " pts",
    "va_dis",      "Value added, disadvantaged pupils, 2021/22 to 2024/25", "Results",    "div",  "high", 1,   " pts",
    "va_non",      "Value added, other pupils, 2021/22 to 2024/25",         "Results",    "div",  "high", 1,   " pts",
    "fsm",         "Disadvantaged pupils (FSM in last six years), 2024/25", "Intake",     "seq",  "",     1,   "%",
    "ks2_c",       "Prior attainment at KS2 (centred), 2024/25",            "Intake",     "seq",  "",     1,   "",
    "eal",         "English as an additional language, 2024/25",            "Intake",     "seq",  "",     1,   "%",
    "sen_ehcp",    "Education, health and care plans, 2024/25",             "Intake",     "seq",  "",     1,   "%",
    "sen_support", "SEN support, 2024/25",                                  "Intake",     "seq",  "",     1,   "%",
    "pupils",      "Pupils on roll, 2024/25",                               "Intake",     "seq",  "",     0,   "",
    "gov",         "Who runs it, 2024/25",                                  "The school", "cat",  "",     0,   "",
    "ofsted",      "Ofsted rating",                                         "The school", "cat",  "",     0,   "",
    "religion",    "Religious character",                                   "The school", "cat",  "",     0,   "")
}

MAP_CATS <- list(
  gov = c("Maintained" = TEAL, "Multi-academy trust, converter or free school" = "#993bff",
          "Multi-academy trust, sponsored" = WARM, "Single-academy trust or not recorded" = "#8a8aa3"),
  ofsted = c("Outstanding" = TEAL, "Good" = "#8fc1b5", "Requires Improvement" = WARM,
             "Inadequate" = BAD),
  religion = c("None" = "#8a8aa3", "Church of England" = "#993bff", "Roman Catholic" = WARM,
               "Christian" = "#2196f3"))

#' Every school in the area on one table, with each variable the
#' dropdown offers
map_data <- function(X, GOV) {
  mean_va <- X$va |>
    filter(year_label %in% c("2021-22", "2022-23", "2023-24", "2024-25")) |>
    group_by(URN) |>
    summarise(va_all = mean(va_ATT8SCR, na.rm = TRUE),
              va_dis = mean(va_ATT8SCR_FSM6CLA1A, na.rm = TRUE),
              va_non = mean(va_ATT8SCR_NFSM6CLA1A, na.rm = TRUE), .groups = "drop") |>
    mutate(across(-URN, ~ifelse(is.finite(.x), .x, NA_real_)))
  X$schools |>
    mutate(URN = as.integer(URN)) |>
    left_join(GOV |> select(URN, run_by, run_by_now, route), by = "URN") |>
    left_join(mean_va, by = "URN") |>
    left_join(X$joint$schools |> transmute(URN, abs_excess = resid_abs_pp), by = "URN") |>
    left_join(X$sch_reasons |> transmute(URN = as.integer(urn), illness,
                                         unauth = unauth_other, reduced), by = "URN") |>
    left_join(X$excl_schools |> transmute(URN = urn, susp_rate), by = "URN") |>
    mutate(gov = case_when(grepl("^Maintained", run_by) ~ "Maintained",
                           run_by == "Single-academy trust" | grepl("not recorded", run_by) ~
                             "Single-academy trust or not recorded",
                           route == "Sponsor-led academy" ~ "Multi-academy trust, sponsored",
                           TRUE ~ "Multi-academy trust, converter or free school"),
           religion = ifelse(religion %in% c("Does not apply", "None"), "None", religion),
           tip = sprintf("<b>%s</b> (%s)<br>Absence %.1f%%, FSM %.0f%%, Attainment 8 %.1f<br>Run by (2024/25): %s%s",
                         SCHNAME, district, absence, fsm, att8, run_by,
                         ifelse(!is.na(run_by_now) & run_by_now != run_by,
                                paste0("<br>Now: ", run_by_now), "")))
}

#' The area's schools within its district boundaries, coloured by the
#' variable chosen from a dropdown above the map (absence by default).
#' Circle size is the school's roll throughout, so the size of a school
#' never reads as a value. Built on the Brighton & Hove case study's
#' school characteristics map: the markers are drawn in the browser.
map_area <- function(X, GOV, LADS, height = 560) {
  lads <- LADS |> filter(if (is.null(X$area$lads)) la %in% X$area$las else lad %in% X$area$lads)
  lad_c <- suppressWarnings(sf::st_coordinates(sf::st_point_on_surface(lads)))
  bb <- sf::st_bbox(lads)
  vars <- map_vars(max(X$abs_years))
  dat <- map_data(X, GOV) |>
    select(SCHNAME, lon, lat, pupils, tip, any_of(vars$var))
  payload <- list(
    schools = jsonlite::toJSON(dat, dataframe = "rows", na = "null", digits = 4),
    vars = jsonlite::toJSON(vars, dataframe = "rows"),
    cats = jsonlite::toJSON(lapply(MAP_CATS, as.list), auto_unbox = TRUE))
  leaflet::leaflet(height = height, options = leaflet::leafletOptions(scrollWheelZoom = FALSE)) |>
    leaflet::addProviderTiles("Esri.WorldGrayCanvas") |>
    leaflet::addPolygons(data = lads, color = PURPLE, weight = 1.5, fill = TRUE,
                         fillOpacity = 0.03, fillColor = PURPLE, label = ~lad) |>
    leaflet::addLabelOnlyMarkers(lng = lad_c[, 1], lat = lad_c[, 2], label = lads$lad,
      labelOptions = leaflet::labelOptions(noHide = TRUE, textOnly = TRUE,
        style = list(color = PURPLE, "font-weight" = "bold", "font-size" = "12px"))) |>
    leaflet::fitBounds(bb[["xmin"]], bb[["ymin"]], bb[["xmax"]], bb[["ymax"]]) |>
    htmlwidgets::onRender(MAP_JS, data = payload)
}

MAP_JS <- '
function(el, x, data) {
  var map = this;
  // htmlwidgets writes the "json" strings into the page as objects
  var schools = data.schools, vars = data.vars, cats = data.cats;
  var defaultVar = "absence";

  // Sequential: light blue -> teal -> deep purple. Diverging: red -> grey -> teal.
  var SEQ = [[187,210,240],[46,98,96],[78,60,86]];
  var DIV = [[178,24,43],[232,232,238],[46,98,96]];
  function ramp(C, t) {
    t = Math.max(0, Math.min(1, t));
    var i = Math.min(Math.floor(t * 2), 1), f = t * 2 - i;
    return "rgb(" + [0,1,2].map(function(k) {
      return Math.round(C[i][k] + f * (C[i+1][k] - C[i][k])); }).join(",") + ")";
  }
  function css(C) { return "linear-gradient(to right," + C.map(function(c) {
    return "rgb(" + c.join(",") + ")"; }).join(",") + ")"; }

  var markers = L.layerGroup().addTo(map);
  var legDiv;
  var leg = L.control({position: "bottomright"});
  leg.onAdd = function() {
    legDiv = L.DomUtil.create("div");
    legDiv.style.cssText = "background:white;padding:8px 12px;border-radius:5px;" +
      "box-shadow:0 1px 5px rgba(0,0,0,.25);font-size:12px;line-height:1.5;max-width:200px;";
    return legDiv;
  };
  leg.addTo(map);

  function fmt(v, m) { return v == null ? "no data" :
    (m.scale === "div" && v > 0 ? "+" : "") + (+v).toFixed(m.dp) + m.unit; }

  function draw(name) {
    markers.clearLayers();
    var m = vars.find(function(v) { return v["var"] === name; });
    var vals = schools.map(function(d) { return d[name]; })
                      .filter(function(v) { return v != null && (m.scale === "cat" || !isNaN(v)); });
    var lo = 0, hi = 1, colour;
    if (m.scale === "cat") {
      colour = function(v) { return cats[name][v] || "#c9c9d6"; };
    } else if (m.scale === "div") {
      hi = Math.max.apply(null, vals.map(Math.abs)) || 1; lo = -hi;
      colour = function(v) { var t = (v - lo) / (hi - lo);
        return ramp(DIV, m.good === "high" ? t : 1 - t); };
    } else {
      lo = Math.min.apply(null, vals); hi = Math.max.apply(null, vals);
      colour = function(v) { return ramp(SEQ, (v - lo) / ((hi - lo) || 1)); };
    }
    var missing = 0, counts = {};
    schools.forEach(function(d) {
      var v = d[name], none = v == null;
      if (none) missing++; else if (m.scale === "cat") counts[v] = (counts[v] || 0) + 1;
      L.circleMarker([d.lat, d.lon], {
        radius: Math.sqrt(d.pupils) / 4,
        fillColor: none ? "#ffffff" : colour(v), fillOpacity: none ? 0.6 : 0.9,
        color: none ? "#8a8aa3" : "white", weight: none ? 1.5 : 1
      }).bindTooltip(d.tip + "<br><b>" + m.label + ": " +
          (m.scale === "cat" ? (v || "not known") : fmt(v, m)) + "</b>")
        .addTo(markers);
    });

    var h = "<strong>" + m.label + "</strong><br>";
    if (m.scale === "cat") {
      Object.keys(cats[name]).forEach(function(k) {
        if (!counts[k]) return;
        h += "<div><span style=\\"display:inline-block;width:11px;height:11px;border-radius:50%;" +
          "margin-right:6px;vertical-align:-1px;background:" + cats[name][k] + "\\"></span>" +
          k + " (" + counts[k] + ")</div>";
      });
    } else {
      var C = m.scale === "div" ? (m.good === "high" ? DIV : DIV.slice().reverse()) : SEQ;
      h += "<div style=\\"height:12px;border-radius:2px;margin:4px 0;background:" + css(C) + "\\"></div>" +
        "<span>" + fmt(lo, m) + "</span>" +
        (m.scale === "div" ? "<span style=\\"position:absolute;left:50%;transform:translateX(-50%)\\">0</span>" : "") +
        "<span style=\\"float:right\\">" + fmt(hi, m) + "</span>";
      if (m.scale === "div") h += "<div style=\\"font-size:11px;color:#666\\">" +
        (m.good === "high" ? "red below expectation, teal above" : "teal below expectation, red above") + "</div>";
    }
    if (missing) h += "<div style=\\"font-size:11px;color:#666;margin-top:2px\\">" +
      "<span style=\\"display:inline-block;width:9px;height:9px;border-radius:50%;margin-right:5px;" +
      "border:1.5px solid #8a8aa3\\"></span>no data (" + missing + ")</div>";
    h += "<div style=\\"font-size:11px;color:#666\\">Circle size: pupils on roll</div>";
    legDiv.style.position = "relative";
    legDiv.innerHTML = h;
  }

  // Dropdown above the map
  var wrap = document.createElement("div");
  wrap.style.cssText = "margin:0 0 8px;font-size:14px;display:flex;flex-wrap:wrap;gap:6px;align-items:center;";
  var lbl = document.createElement("label");
  lbl.textContent = "Colour schools by:";
  lbl.style.fontWeight = "bold";
  var sel = document.createElement("select");
  sel.setAttribute("aria-label", "Variable to colour the schools by");
  sel.style.cssText = "padding:4px 8px;font-size:13px;max-width:100%;border:1px solid #ccc;border-radius:4px;";
  var og = null;
  vars.forEach(function(v) {
    if (!og || og.label !== v.group) {
      og = document.createElement("optgroup"); og.label = v.group; sel.appendChild(og);
    }
    var o = document.createElement("option");
    o.value = v["var"]; o.textContent = v.label;
    if (v["var"] === defaultVar) o.selected = true;
    og.appendChild(o);
  });
  wrap.appendChild(lbl); wrap.appendChild(sel);
  el.parentNode.insertBefore(wrap, el);
  sel.addEventListener("change", function() { draw(sel.value); });
  draw(defaultVar);
}
'


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
