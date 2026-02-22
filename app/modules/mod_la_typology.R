# mod_la_typology.R - Local Authority Typology module (map + cluster analysis)
# -----------------------------------------------------------------------------
# Exports two UI functions sharing one server:
#   mod_la_typology_ui("la_typology")      → Tab 4: map + LA profile + individual radar
#   mod_la_cluster_ui("la_typology")       → Tab 5: cluster radar + bar chart + data table
#   mod_la_typology_server("la_typology")  → single server for both tabs
# -----------------------------------------------------------------------------

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0 || all(is.na(x))) y else x
}

# ---- Methodology report link ----
TYPOLOGY_REPORT_URL <- "https://adamdennett.github.io/school_attainment_tool/la_typology_report.html"

# ---- Radar chart variables and labels ----
RADAR_VARS <- c(
  "att8_mean",           "pct_fsm_mean",       "abs_overall_mean",
  "ofsted_good_plus_pct","retention_mean",      "pct_eal_mean",
  "att8_fsm_mean",       "gorard_mean",         "sickness_mean",
  "ptr_qual_mean"
)
RADAR_LABELS <- c(
  "ATT8", "% FSM", "Absence",
  "Good+ Ofsted", "Retention", "% EAL",
  "FSM ATT8", "Segregation", "Sickness",
  "PTR"
)
# FSM ATT8 is NOT inverted: higher = better (outward)
RADAR_INVERT <- c("pct_fsm_mean", "abs_overall_mean",
                  "gorard_mean",  "sickness_mean",    "ptr_qual_mean")

# ---- Shared CSS injected once ----
.typology_css <- tags$head(tags$style(HTML("
  .cluster-badge {
    display: inline-block; padding: 3px 10px; border-radius: 12px;
    color: white; font-size: 12px; font-weight: bold; margin-bottom: 4px;
  }
  .portrait-box {
    background: #f8f9fa; border-left: 4px solid #0d6efd;
    padding: 10px 14px; border-radius: 0 6px 6px 0;
    font-size: 13px; line-height: 1.6; margin-bottom: 8px;
  }
  .metric-row {
    display: flex; justify-content: space-between;
    border-bottom: 1px solid #eee; padding: 4px 0; font-size: 12px;
  }
  .metric-label { color: #555; }
  .metric-value { font-weight: bold; }
  .cluster-legend-item { display: flex; align-items: center; margin-bottom: 5px; font-size: 12px; }
  .cluster-swatch { width: 16px; height: 16px; border-radius: 3px; margin-right: 8px; flex-shrink: 0; }
  .radar-note { font-size: 10px; color: #888; font-style: italic; margin-top: 4px; }
")))


# ==============================================================================
# TAB 4 UI: Map + LA profile + individual radar
# ==============================================================================

mod_la_typology_ui <- function(id) {
  ns <- NS(id)

  tagList(
    .typology_css,

    layout_sidebar(
      sidebar = sidebar(
        width = 280,
        title = "Typology Controls",

        selectInput(
          ns("colour_by"), "Colour map by",
          choices = c(
            "Cluster type"            = "cluster",
            "Mean ATT8 score"         = "att8_mean",
            "FSM ATT8 (disadv.)"      = "att8_fsm_mean",
            "Non-FSM ATT8"            = "att8_nfsm_mean",
            "Disadvantage gap"        = "att8_gap_mean",
            "Gap driver index"        = "gap_driver_index",
            "% Disadvantaged (FSM)"   = "pct_fsm_mean",
            "% EAL pupils"            = "pct_eal_mean",
            "Overall absence (%)"     = "abs_overall_mean",
            "Good+ Ofsted (%)"        = "ofsted_good_plus_pct",
            "Gorard segregation"      = "gorard_mean",
            "Teacher retention (FTE)" = "retention_mean",
            "Teacher sickness (days)" = "sickness_mean",
            "Pupil:teacher ratio"     = "ptr_qual_mean",
            "% Selective schools"     = "pct_selective",
            "% Academy schools"       = "pct_academy",
            "ATT8 trend (slope)"      = "att8_trend_slope"
          ),
          selected = "cluster"
        ),

        hr(),

        checkboxGroupInput(
          ns("cluster_filter"), "Show clusters",
          choices = NULL, selected = NULL
        ),

        hr(),

        h6("Cluster Pen Portraits"),
        uiOutput(ns("pen_portraits_sidebar")),

        hr(),
        tags$div(
          style = "font-size:12px; text-align:center; padding:4px 0;",
          icon("book-open", class = "me-1"),
          tags$a(href = TYPOLOGY_REPORT_URL, target = "_blank",
                 "Typology methodology report"),
          tags$br(),
          tags$span(class = "text-muted", style = "font-size:10px;",
                    "Full details of the clustering approach")
        )
      ),

      # Map (left, wider) + LA profile + individual radar (right)
      layout_columns(
        col_widths = c(7, 5),

        card(
          full_screen = TRUE,
          card_header(
            "LA Typology Map",
            span(class = "ms-2 text-muted",
                 style = "font-size:12px; font-weight:normal;",
                 textOutput(ns("map_subtitle"), inline = TRUE))
          ),
          leafletOutput(ns("cluster_map"), height = "620px")
        ),

        card(
          full_screen = TRUE,
          card_header("Selected LA Profile"),
          card_body(
            style = "overflow-y:auto; flex:1 1 auto;",
            uiOutput(ns("la_profile_panel")),
            uiOutput(ns("la_radar_container"))
          )
        )
      )
    )
  )
}


# ==============================================================================
# TAB 5 UI: Cluster radar + bar chart + data table
# ==============================================================================

mod_la_cluster_ui <- function(id) {
  ns <- NS(id)

  tagList(
    .typology_css,

    layout_sidebar(
      sidebar = sidebar(
        width = 260,
        title = "Cluster Controls",

        # Real Shiny checkboxGroupInput — synced bidirectionally with Tab 4 sidebar
        checkboxGroupInput(
          ns("cluster_filter_tab5"), "Show clusters",
          choices  = NULL,
          selected = NULL
        ),

        hr(),

        tags$p(style = "font-size:12px; color:#555; margin-bottom:4px;",
               tags$b("Bar chart variable:")),
        selectInput(
          ns("compare_var"), NULL,
          choices = c(
            "Mean ATT8 score"           = "att8_mean",
            "FSM ATT8 (disadv.)"        = "att8_fsm_mean",
            "Non-FSM ATT8"              = "att8_nfsm_mean",
            "Disadvantage gap"          = "att8_gap_mean",
            "Gap driver index"          = "gap_driver_index",
            "Progress 8 mean"           = "p8_mean",
            "% Disadvantaged (FSM)"     = "pct_fsm_mean",
            "% EAL pupils"              = "pct_eal_mean",
            "Overall absence (%)"       = "abs_overall_mean",
            "Persistent absence (%)"    = "abs_persist_mean",
            "Good+ Ofsted (%)"          = "ofsted_good_plus_pct",
            "Outstanding Ofsted (%)"    = "ofsted_outstanding_pct",
            "Gorard segregation"        = "gorard_mean",
            "Teacher retention (FTE)"   = "retention_mean",
            "Teacher sickness (days)"   = "sickness_mean",
            "Pupil:teacher ratio"       = "ptr_qual_mean",
            "% Selective schools"       = "pct_selective",
            "% Academy schools"         = "pct_academy",
            "Mean school size (pupils)" = "totpups_mean",
            "ATT8 trend (slope/yr)"     = "att8_trend_slope",
            "Gender gap (girls-boys)"   = "att8_gender_gap"
          ),
          selected = "att8_mean"
        ),

        hr(),
        tags$div(
          style = "font-size:12px; text-align:center; padding:4px 0;",
          icon("book-open", class = "me-1"),
          tags$a(href = TYPOLOGY_REPORT_URL, target = "_blank",
                 "Typology methodology report"),
          tags$br(),
          tags$span(class = "text-muted", style = "font-size:10px;",
                    "Full details of the clustering approach")
        )
      ),

      tagList(
        # Radar (left) + bar chart (right)
        layout_columns(
          col_widths = c(6, 6),

          card(
            full_screen = TRUE,
            card_header(
              "Cluster Radar Profiles",
              span(class = "ms-2 text-muted",
                   style = "font-size:11px; font-weight:normal;",
                   "z-scores vs England average — outward = better")
            ),
            plotlyOutput(ns("cluster_radar"), height = "500px"),
            uiOutput(ns("radar_legend"))
          ),

          card(
            full_screen = TRUE,
            card_header("Cluster Comparison: Key Indicators"),
            plotlyOutput(ns("cluster_bar"), height = "500px")
          )
        ),

        # Data table
        card(
          full_screen = TRUE,
          card_header("All Local Authorities — Typology Data"),
          DTOutput(ns("la_table"))
        )
      )
    )
  )
}


# ==============================================================================
# SHARED SERVER (handles both tabs via the same module ID)
# ==============================================================================

mod_la_typology_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ---- Data ----
    la_typ  <- la_typology
    cl_sum  <- la_cluster_summary
    cl_meta <- cluster_meta_data
    la_ind  <- la_indicators_data

    chosen_k     <- cl_meta$chosen_k
    cluster_cols <- cl_meta$cluster_colours
    cluster_labs <- cl_meta$cluster_labels

    # ---- Z-scores for radar ----
    radar_vars_avail <- intersect(RADAR_VARS, names(la_ind))

    la_z_scores <- la_ind %>%
      select(LANAME, cluster, all_of(radar_vars_avail)) %>%
      mutate(across(all_of(radar_vars_avail), function(x) {
        m <- mean(x, na.rm = TRUE); s <- sd(x, na.rm = TRUE)
        if (is.na(s) || s == 0) rep(0, length(x)) else (x - m) / s
      }))

    cluster_z_scores <- la_z_scores %>%
      group_by(cluster) %>%
      summarise(across(all_of(radar_vars_avail), ~ mean(.x, na.rm = TRUE)),
                .groups = "drop")

    available_mask   <- RADAR_VARS %in% radar_vars_avail
    radar_vars_use   <- RADAR_VARS[available_mask]
    radar_labels_use <- RADAR_LABELS[available_mask]
    radar_invert_use <- RADAR_INVERT[RADAR_INVERT %in% radar_vars_use]

    # ---- Cluster filter: single reactiveVal as source of truth ----
    # Two checkboxGroupInputs (Tab 4 sidebar: cluster_filter,
    # Tab 5 sidebar: cluster_filter_tab5) both read from / write to
    # selected_clusters_rv.  A sync flag prevents feedback loops:
    # when we are in the middle of pushing a new value out to a widget
    # we ignore the echo that widget fires back.

    filter_choices <- setNames(
      as.character(1:chosen_k),
      paste0(cluster_labs, " (", cl_meta$cluster_sizes, " LAs)")
    )
    all_selected <- as.character(1:chosen_k)

    selected_clusters_rv <- reactiveVal(all_selected)
    syncing <- FALSE   # plain R flag — not reactive, used only to break echo

    # Populate both widgets once on startup (choices + initial selection)
    observe({
      updateCheckboxGroupInput(session, "cluster_filter",
                               choices  = filter_choices,
                               selected = all_selected)
      updateCheckboxGroupInput(session, "cluster_filter_tab5",
                               choices  = filter_choices,
                               selected = all_selected)
    })

    # Tab 4 checkbox → update rv + mirror to Tab 5
    observeEvent(input$cluster_filter, {
      if (syncing) return()
      new_val <- sort(input$cluster_filter)
      if (identical(new_val, sort(selected_clusters_rv()))) return()
      syncing <<- TRUE
      selected_clusters_rv(new_val)
      updateCheckboxGroupInput(session, "cluster_filter_tab5", selected = new_val)
      syncing <<- FALSE
    }, ignoreNULL = FALSE, ignoreInit = TRUE)

    # Tab 5 checkbox → update rv + mirror to Tab 4
    observeEvent(input$cluster_filter_tab5, {
      if (syncing) return()
      new_val <- sort(input$cluster_filter_tab5)
      if (identical(new_val, sort(selected_clusters_rv()))) return()
      syncing <<- TRUE
      selected_clusters_rv(new_val)
      updateCheckboxGroupInput(session, "cluster_filter", selected = new_val)
      syncing <<- FALSE
    }, ignoreNULL = FALSE, ignoreInit = TRUE)

    # ---- Helper: selected clusters (single source of truth) ----
    selected_clusters_r <- reactive({
      sel <- as.integer(selected_clusters_rv())
      if (length(sel) == 0) 1:chosen_k else sel
    })

    # ---- Filtered sf reactive (debounced to prevent rapid-fire map redraws) ----
    filtered_typ_raw <- reactive({
      la_boundaries_sf %>%
        filter(cluster %in% selected_clusters_r())
    })
    filtered_typ <- filtered_typ_raw %>% debounce(300)

    # ========================
    # MAP TAB OUTPUTS
    # ========================

    output$map_subtitle <- renderText({
      cb <- input$colour_by
      if (cb == "cluster") {
        paste0(chosen_k, "-cluster typology")
      } else {
        c(att8_mean = "Mean ATT8 score",
          att8_fsm_mean = "FSM ATT8 (disadvantaged)", att8_nfsm_mean = "Non-FSM ATT8",
          att8_gap_mean = "Disadvantage gap",
          gap_driver_index = "Gap driver (neg = low FSM, pos = high non-FSM)",
          pct_fsm_mean = "% FSM pupils", pct_eal_mean = "% EAL pupils",
          abs_overall_mean = "Overall absence (%)",
          ofsted_good_plus_pct = "% Good or Outstanding",
          gorard_mean = "Gorard segregation index",
          retention_mean = "Teacher retention (FTE)",
          sickness_mean = "Teacher sickness (days)",
          ptr_qual_mean = "Pupil:teacher ratio",
          pct_selective = "% selective schools",
          pct_academy = "% academy schools",
          att8_trend_slope = "ATT8 trend (pts/yr)"
        )[cb] %||% cb
      }
    })

    # ---- Helper: build polygon layer arguments ----
    build_map_layer <- function(map_sf, cb) {
      if (cb == "cluster") {
        map_sf$fill_var <- as.character(map_sf$cluster)
        pal    <- colorFactor(palette = cluster_cols,
                              domain  = as.character(1:chosen_k),
                              na.color = "#cccccc")
        pal_fn <- function(x) pal(x)
      } else {
        fill_vals <- map_sf[[cb]]
        if (is.null(fill_vals) || all(is.na(fill_vals))) {
          map_sf$fill_var <- as.character(map_sf$cluster)
          pal    <- colorFactor(palette = cluster_cols,
                                domain  = as.character(1:chosen_k),
                                na.color = "#cccccc")
          pal_fn <- function(x) pal(x)
        } else {
          map_sf$fill_var <- fill_vals
          pal    <- colorNumeric("YlOrRd", domain = map_sf$fill_var,
                                 na.color = "#cccccc",
                                 reverse  = cb %in% c("retention_mean", "gap_driver_index",
                                                       "att8_fsm_mean", "att8_nfsm_mean"))
          pal_fn <- function(x) pal(x)
        }
      }
      list(map_sf = map_sf, pal = pal, pal_fn = pal_fn)
    }

    build_popups <- function(map_sf) {
      make_row <- function(lbl, val, fmt = "%.1f", sfx = "") {
        if (is.null(val) || length(val) == 0 || is.na(val)) return("")
        paste0('<div class="metric-row"><span class="metric-label">', lbl,
               '</span><span class="metric-value">',
               sprintf(fmt, val), sfx, '</span></div>')
      }
      purrr::map_chr(seq_len(nrow(map_sf)), function(i) {
        row <- st_drop_geometry(map_sf[i, ])
        cl  <- row$cluster; col <- cluster_cols[cl]
        paste0(
          '<div style="font-family:Arial,sans-serif;max-width:300px;">',
          '<div style="background:', col, ';color:white;padding:6px 10px;border-radius:4px 4px 0 0;">',
          '<strong>', htmltools::htmlEscape(row$LANAME), '</strong><br>',
          '<span style="font-size:11px;">', htmltools::htmlEscape(row$cluster_label), '</span></div>',
          '<div style="border:1px solid ', col, ';border-top:none;padding:7px 10px;',
          'border-radius:0 0 4px 4px;font-size:12px;">',
          '<div style="color:#666;font-size:11px;margin-bottom:4px;">',
          htmltools::htmlEscape(as.character(row$gor_name)), '</div>',
          make_row("ATT8",             row$att8_mean),
          make_row("Disadv. gap",      row$att8_gap_mean),
          make_row("  FSM ATT8",       row$att8_fsm_mean),
          make_row("  Non-FSM ATT8",   row$att8_nfsm_mean),
          make_row("% FSM",            row$pct_fsm_mean,           sfx = "%"),
          make_row("% EAL",            row$pct_eal_mean,           sfx = "%"),
          make_row("Absence",          row$abs_overall_mean,       sfx = "%"),
          make_row("Good+ Ofsted",     row$ofsted_good_plus_pct * 100, "%.0f", "%"),
          make_row("Gorard seg.",      row$gorard_mean, "%.3f"),
          make_row("Retention",        row$retention_mean),
          make_row("Sickness (days)",  row$sickness_mean),
          make_row("PTR",              row$ptr_qual_mean),
          '</div></div>'
        )
      })
    }

    build_legend_html <- function() {
      items <- purrr::map_chr(1:chosen_k, function(k) {
        cr <- cl_sum %>% filter(cluster == k)
        paste0('<div class="cluster-legend-item">',
               '<div class="cluster-swatch" style="background:', cluster_cols[k], ';"></div>',
               '<span>', htmltools::htmlEscape(cr$label), ' (', cr$n_las, ')</span></div>')
      })
      paste0('<div style="background:white;padding:9px 12px;border-radius:5px;',
             'box-shadow:0 1px 5px rgba(0,0,0,.25);font-family:Arial,sans-serif;">',
             '<strong style="font-size:12px;display:block;margin-bottom:6px;">',
             'Clusters (', chosen_k, ')</strong>',
             paste(items, collapse = ""),
             '<div style="font-size:10px;color:#888;margin-top:6px;">',
             'Click an LA for details</div></div>')
    }

    output$cluster_map <- renderLeaflet({
      # Build initial map with all polygons so it renders correctly on first tab visit.
      # Subsequent updates (cluster filter, colour-by changes) are handled via leafletProxy.
      init_sf <- la_boundaries_sf
      layer   <- build_map_layer(init_sf, "cluster")
      popups  <- build_popups(layer$map_sf)

      leaflet() %>%
        addProviderTiles(providers$CartoDB.Positron,
                         options = tileOptions(minZoom = 5, maxZoom = 14)) %>%
        setView(lng = -1.5, lat = 52.8, zoom = 6) %>%
        addPolygons(
          data = layer$map_sf, group = "la_polygons",
          fillColor = ~layer$pal_fn(fill_var),
          fillOpacity = 0.75, color = "white", weight = 1.2, opacity = 0.8,
          highlightOptions = highlightOptions(weight = 3, color = "#333",
                                              fillOpacity = 0.9, bringToFront = TRUE),
          popup = popups,
          label = ~paste0(LANAME, ": ", cluster_label),
          labelOptions = labelOptions(style = list("font-size" = "12px"), direction = "auto"),
          layerId = ~LANAME
        ) %>%
        addControl(html = build_legend_html(), position = "topright")
    })

    observe({
      req(la_boundaries_sf)
      cb     <- input$colour_by
      map_sf <- filtered_typ()
      if (is.null(map_sf) || nrow(map_sf) == 0) return()

      layer  <- build_map_layer(map_sf, cb)
      map_sf <- layer$map_sf
      popups <- build_popups(map_sf)

      proxy <- leafletProxy(ns("cluster_map")) %>%
        clearGroup("la_polygons") %>% clearControls() %>%
        addPolygons(
          data = map_sf, group = "la_polygons",
          fillColor = ~layer$pal_fn(fill_var),
          fillOpacity = 0.75, color = "white", weight = 1.2, opacity = 0.8,
          highlightOptions = highlightOptions(weight = 3, color = "#333",
                                              fillOpacity = 0.9, bringToFront = TRUE),
          popup = popups,
          label = ~paste0(LANAME, ": ", cluster_label),
          labelOptions = labelOptions(style = list("font-size" = "12px"), direction = "auto"),
          layerId = ~LANAME
        )

      if (cb == "cluster") {
        proxy <- proxy %>% addControl(html = build_legend_html(), position = "topright")
      } else {
        proxy <- proxy %>%
          addLegend("topright", pal = layer$pal, values = map_sf$fill_var,
                    title = input$colour_by, opacity = 0.8, na.label = "N/A")
      }
      proxy
    })

    # ---- Selected LA ----
    selected_la <- reactiveVal(NULL)
    observeEvent(input$cluster_map_shape_click, {
      id <- input$cluster_map_shape_click$id
      if (!is.null(id)) selected_la(id)
    })

    # ---- LA profile panel ----
    output$la_profile_panel <- renderUI({
      la_name <- selected_la()
      if (is.null(la_name)) {
        return(tags$div(
          class = "text-center text-muted p-4", style = "margin-top:30px;",
          icon("mouse-pointer", style = "font-size:28px;margin-bottom:8px;"),
          tags$br(), "Click a local authority on the map to see its profile"
        ))
      }
      row <- la_typ %>% filter(LANAME == la_name)
      if (nrow(row) == 0) return(tags$p("No data for ", la_name))
      row <- row[1, ]

      cl  <- row$cluster;  cl_col <- cluster_cols[cl]
      portrait <- (cl_sum %>% filter(cluster == cl))$pen_portrait[1]

      f1 <- function(x) if (is.null(x) || length(x) == 0 || is.na(x)) "N/A" else round(x, 1)
      f0 <- function(x) if (is.null(x) || length(x) == 0 || is.na(x)) "N/A" else round(x, 0)
      fp <- function(x) if (is.null(x) || length(x) == 0 || is.na(x)) "N/A" else paste0(round(x*100, 0), "%")
      f3 <- function(x) if (is.na(x)) "N/A" else round(x, 3)

      tagList(
        tags$div(
          style = paste0("background:", cl_col,
                         ";color:white;padding:9px 12px;border-radius:6px;margin-bottom:8px;"),
          tags$div(class = "fw-bold", la_name),
          tags$div(style = "font-size:12px;", row$cluster_label)
        ),
        tags$div(
          style = "font-size:12px;",
          tags$div(class="metric-row", tags$span(class="metric-label","Region"),
                   tags$span(class="metric-value", as.character(row$gor_name))),
          tags$div(class="metric-row", tags$span(class="metric-label","Mean ATT8"),
                   tags$span(class="metric-value", f1(row$att8_mean))),
          tags$div(class="metric-row", tags$span(class="metric-label","Disadvantage gap"),
                   tags$span(class="metric-value", f1(row$att8_gap_mean), " pts")),
          tags$div(class="metric-row", tags$span(class="metric-label","  FSM ATT8"),
                   tags$span(class="metric-value", f1(row$att8_fsm_mean))),
          tags$div(class="metric-row", tags$span(class="metric-label","  Non-FSM ATT8"),
                   tags$span(class="metric-value", f1(row$att8_nfsm_mean))),
          tags$div(class="metric-row", tags$span(class="metric-label","  Gap driver"),
                   tags$span(class="metric-value",
                     if (!is.null(row$gap_driver_index) && !is.na(row$gap_driver_index)) {
                       gdi <- round(row$gap_driver_index, 2)
                       lbl <- if (gdi < -0.3) " (low FSM)" else if (gdi > 0.3) " (high non-FSM)" else " (mixed)"
                       paste0(gdi, lbl)
                     } else "N/A"
                   )),
          tags$div(class="metric-row", tags$span(class="metric-label","% FSM"),
                   tags$span(class="metric-value", f1(row$pct_fsm_mean), "%")),
          tags$div(class="metric-row", tags$span(class="metric-label","% EAL"),
                   tags$span(class="metric-value", f1(row$pct_eal_mean), "%")),
          tags$div(class="metric-row", tags$span(class="metric-label","Overall absence"),
                   tags$span(class="metric-value", f1(row$abs_overall_mean), "%")),
          tags$div(class="metric-row", tags$span(class="metric-label","Good+ Ofsted"),
                   tags$span(class="metric-value", fp(row$ofsted_good_plus_pct))),
          tags$div(class="metric-row", tags$span(class="metric-label","Gorard seg."),
                   tags$span(class="metric-value", f3(row$gorard_mean))),
          tags$div(class="metric-row", tags$span(class="metric-label","Retention"),
                   tags$span(class="metric-value", f1(row$retention_mean))),
          tags$div(class="metric-row", tags$span(class="metric-label","Sickness (days)"),
                   tags$span(class="metric-value", f1(row$sickness_mean))),
          tags$div(class="metric-row", tags$span(class="metric-label","PTR"),
                   tags$span(class="metric-value", f1(row$ptr_qual_mean))),
          tags$div(class="metric-row", tags$span(class="metric-label","Schools (mean)"),
                   tags$span(class="metric-value", f0(row$n_schools_mean))),
          tags$div(class="metric-row", tags$span(class="metric-label","ATT8 trend"),
                   tags$span(class="metric-value", f1(row$att8_trend_slope), " pts/yr"))
        ),
        tags$hr(),
        tags$h6("Cluster Portrait"),
        tags$div(
          class = "portrait-box",
          style = paste0("border-left-color:", cl_col, ";"),
          tags$p(style = "margin:0;font-size:12px;", {
            lines <- stringr::str_split(portrait, "\n")[[1]]
            lines <- lines[nchar(trimws(lines)) > 0]
            if (length(lines) >= 3) lines[3] else lines[1] %||% ""
          })
        )
      )
    })

    # ---- Individual LA radar ----
    output$la_radar_container <- renderUI({
      la_name <- selected_la()
      if (is.null(la_name)) return(NULL)
      tagList(
        tags$hr(),
        tags$h6(paste0(la_name, " — Radar Profile")),
        plotlyOutput(ns("la_radar"), height = "300px"),
        tags$p(class = "radar-note text-center",
               "Solid = this LA | Dashed = cluster mean | Dotted ring = England avg. ",
               "Outward = better on each axis.")
      )
    })

    output$la_radar <- renderPlotly({
      la_name <- selected_la()
      req(!is.null(la_name))

      la_z_row <- la_z_scores %>% filter(LANAME == la_name)
      if (nrow(la_z_row) == 0) return(plotly_empty())
      la_z_row <- la_z_row[1, ]

      cl  <- as.integer(la_z_row$cluster)
      col <- cluster_cols[cl]

      z_la <- vapply(radar_vars_use, function(v) as.numeric(la_z_row[[v]]), numeric(1))
      names(z_la) <- radar_vars_use

      cl_z_row <- cluster_z_scores %>% filter(cluster == cl)
      z_cl <- if (nrow(cl_z_row) > 0)
        vapply(radar_vars_use, function(v) as.numeric(cl_z_row[[v]][1]), numeric(1))
      else
        setNames(rep(0, length(radar_vars_use)), radar_vars_use)
      names(z_cl) <- radar_vars_use

      to_radar <- function(z) {
        d <- z
        for (v in radar_invert_use) d[v] <- -d[v]
        pmin(pmax(d, -2.5), 2.5) + 2.5
      }

      rv_la  <- to_radar(z_la)
      rv_cl  <- to_radar(z_cl)
      rv_eng <- rep(2.5, length(radar_vars_use))
      theta  <- c(radar_labels_use, radar_labels_use[1])

      plotly::plot_ly() %>%
        add_trace(type="scatterpolar", r=c(rv_eng,rv_eng[1]), theta=theta,
                  mode="lines", line=list(color="#bbb",dash="dot",width=1.2),
                  fill="none", name="England avg", hoverinfo="skip") %>%
        add_trace(type="scatterpolar", r=c(rv_cl,rv_cl[1]), theta=theta,
                  fill="toself", fillcolor=adjustcolor(col,.06),
                  line=list(color=col,width=1.5,dash="dash"),
                  name=paste0("Cluster: ",cluster_labs[cl]),
                  customdata=c(z_cl,z_cl[1]),
                  hovertemplate="<b>%{theta}</b><br>z: %{customdata:.2f}<extra>Cluster mean</extra>") %>%
        add_trace(type="scatterpolar", r=c(rv_la,rv_la[1]), theta=theta,
                  fill="toself", fillcolor=adjustcolor(col,.20),
                  line=list(color=col,width=2.5),
                  name=la_name,
                  customdata=c(z_la,z_la[1]),
                  hovertemplate="<b>%{theta}</b><br>z: %{customdata:.2f}<extra>This LA</extra>") %>%
        layout(
          polar = list(
            radialaxis = list(visible=TRUE, range=c(0,5),
                              tickvals=c(0,1.5,2.5,3.5,5),
                              ticktext=c("","−2","Avg","+2",""),
                              tickfont=list(size=8), gridcolor="#e8e8e8"),
            angularaxis = list(tickfont=list(size=10))
          ),
          legend = list(orientation="h", y=-0.18, font=list(size=9)),
          margin = list(t=5, b=40, l=10, r=10),
          showlegend = TRUE
        ) %>%
        config(displayModeBar=FALSE)
    })

    # ========================
    # CLUSTER ANALYSIS TAB OUTPUTS
    # ========================

    # ---- Pen portraits sidebar (Tab 4) ----
    output$pen_portraits_sidebar <- renderUI({
      tagList(purrr::map(1:chosen_k, function(k) {
        cr  <- cl_sum %>% filter(cluster == k)
        col <- cluster_cols[k]
        lines <- stringr::str_split(cr$pen_portrait[1], "\n")[[1]]
        lines <- lines[nchar(trimws(lines)) > 0]
        tags$div(
          style = "margin-bottom:9px;",
          tags$div(style = paste0("background:", col,
                                  ";color:white;font-size:11px;font-weight:bold;",
                                  "padding:3px 8px;border-radius:3px;margin-bottom:2px;"),
                   cr$label),
          tags$div(style = "font-size:10px;color:#555;line-height:1.4;",
                   if (length(lines) >= 2) lines[2] else "")
        )
      }))
    })

    # ---- All-cluster radar ----
    output$cluster_radar <- renderPlotly({
      sel <- selected_clusters_r()
      p   <- plotly::plot_ly()

      # England ring
      eng_theta <- c(radar_labels_use, radar_labels_use[1])
      p <- p %>% add_trace(
        type="scatterpolar", r=rep(2.5, length(radar_labels_use)+1), theta=eng_theta,
        mode="lines", line=list(color="#999",dash="dot",width=1.5),
        fill="none", name="England average", hoverinfo="skip", showlegend=TRUE
      )

      for (k in sel) {
        cr <- cluster_z_scores %>% filter(cluster == k)
        if (nrow(cr) == 0) next
        z <- vapply(radar_vars_use, function(v) as.numeric(cr[[v]][1]), numeric(1))
        names(z) <- radar_vars_use
        d <- z
        for (v in radar_invert_use) d[v] <- -d[v]
        rv  <- c(pmin(pmax(d,-2.5),2.5)+2.5); rv <- c(rv, rv[1])
        lab <- c(radar_labels_use, radar_labels_use[1])
        col <- cluster_cols[k]; lname <- cluster_labs[k]
        p <- p %>% add_trace(
          type="scatterpolar", r=rv, theta=lab,
          fill="toself", fillcolor=adjustcolor(col,.12),
          line=list(color=col,width=2.2), name=lname,
          customdata=c(z,z[1]),
          hovertemplate=paste0("<b>%{theta}</b><br>z: %{customdata:.2f}<extra>",lname,"</extra>")
        )
      }

      p %>% layout(
        polar = list(
          radialaxis = list(visible=TRUE, range=c(0,5),
                            tickvals=c(0,1.5,2.5,3.5,5),
                            ticktext=c("−2.5","−1","0","+1","+2.5"),
                            tickfont=list(size=9), gridcolor="#e0e0e0"),
          angularaxis = list(tickfont=list(size=11))
        ),
        legend = list(orientation="h", y=-0.12, font=list(size=11)),
        margin = list(t=20, b=60, l=30, r=30),
        showlegend = TRUE
      ) %>% config(displayModeBar=FALSE)
    })

    # ---- Radar legend ----
    output$radar_legend <- renderUI({
      make_item <- function(i) {
        v <- radar_vars_use[i]; lbl <- radar_labels_use[i]; inv <- v %in% radar_invert_use
        tags$div(
          style = "display:flex;align-items:center;padding:1px 0;font-size:10px;",
          tags$span(style="font-weight:bold;min-width:90px;", lbl),
          tags$span(style=paste0("color:", if(inv)"#c0392b" else "#27ae60",";"),
                    if(inv) "↓ inverted" else "↑ higher = better")
        )
      }
      tags$div(
        style = "padding:6px 10px;border-top:1px solid #eee;margin-top:4px;",
        tags$p(style="font-size:10px;color:#777;margin-bottom:4px;",
               tags$b("Outward = better"), " on every axis. ",
               "Dotted ring = England average (z = 0). ",
               tags$span(style="color:#c0392b;","Red axes"),
               " are inverted so lower values point outward."),
        tags$div(style="display:grid;grid-template-columns:1fr 1fr;gap:0 16px;",
                 purrr::map(seq_along(radar_labels_use), make_item))
      )
    })

    # ---- Bar chart ----
    output$cluster_bar <- renderPlotly({
      var <- input$compare_var
      if (!var %in% names(la_ind))
        return(plotly_empty() %>% layout(title="Variable not found"))

      sel <- selected_clusters_r()
      grand_mean <- mean(la_ind[[var]], na.rm=TRUE)

      bar_data <- la_ind %>%
        filter(!is.na(cluster), cluster %in% sel) %>%
        group_by(cluster) %>%
        summarise(mean_val=mean(.data[[var]],na.rm=TRUE),
                  sd_val=sd(.data[[var]],na.rm=TRUE), n_las=n(), .groups="drop") %>%
        mutate(label=cluster_labs[cluster], colour=cluster_cols[cluster],
               se=sd_val/sqrt(n_las)) %>%
        arrange(mean_val) %>%
        mutate(label=factor(label, levels=label))

      ylabels <- c(
        att8_mean="Mean ATT8", att8_fsm_mean="FSM ATT8", att8_nfsm_mean="Non-FSM ATT8",
        att8_gap_mean="Disadv. gap (pts)", gap_driver_index="Gap driver (z-score)",
        p8_mean="Progress 8",
        pct_fsm_mean="% FSM", pct_eal_mean="% EAL", abs_overall_mean="Absence (%)",
        abs_persist_mean="Pers. absence (%)", ofsted_good_plus_pct="Good+ Ofsted (%)",
        ofsted_outstanding_pct="Outstanding (%)", gorard_mean="Gorard seg.",
        retention_mean="Retention (FTE)", sickness_mean="Sickness (days)",
        ptr_qual_mean="PTR", pct_selective="% selective", pct_academy="% academy",
        totpups_mean="School size", att8_trend_slope="ATT8 trend", att8_gender_gap="Gender gap"
      )
      y_label <- ylabels[var] %||% var

      plot_ly(bar_data, x=~label, y=~mean_val, type="bar",
              marker=list(color=~colour, opacity=0.85),
              error_y=list(type="data", array=~se*1.96,
                           color="#555", thickness=1.5, width=4),
              hovertemplate=paste0("<b>%{x}</b><br>",y_label,": %{y:.2f}<extra></extra>")
      ) %>%
        layout(
          xaxis = list(title="", tickangle=-30),
          yaxis = list(title=y_label),
          margin = list(t=10, b=110, l=55, r=20),
          showlegend = FALSE,
          shapes = list(list(
            type="line", x0=-0.5, x1=nrow(bar_data)-0.5,
            y0=grand_mean, y1=grand_mean,
            line=list(color="#dc3545", dash="dash", width=2)
          )),
          annotations = list(list(
            x=nrow(bar_data)-0.5, y=grand_mean,
            text=paste0("England mean: ", round(grand_mean,2)),
            showarrow=FALSE, xanchor="right",
            font=list(color="#dc3545", size=11)
          ))
        ) %>%
        config(displayModeBar=FALSE)
    })

    # ---- Data table ----
    output$la_table <- renderDT({
      ind_data <- la_ind
      if (!"cluster_label" %in% names(ind_data))
        ind_data <- ind_data %>%
          left_join(cl_sum %>% select(cluster, cluster_label=label), by="cluster")

      col_map <- list(
        `LA`="LANAME", `Region`="gor_name", `Cluster`="cluster_label",
        `ATT8`="att8_mean", `FSM ATT8`="att8_fsm_mean", `Non-FSM ATT8`="att8_nfsm_mean",
        `Gap`="att8_gap_mean", `Gap Driver`="gap_driver_index", `P8`="p8_mean",
        `% FSM`="pct_fsm_mean", `% EAL`="pct_eal_mean",
        `Absence`="abs_overall_mean", `Pers.Abs`="abs_persist_mean",
        `Good+ Ofsted`="ofsted_good_plus_pct", `Gorard`="gorard_mean",
        `Retention`="retention_mean", `Sickness`="sickness_mean",
        `PTR`="ptr_qual_mean", `% Sel.`="pct_selective",
        `% Acad.`="pct_academy", `Schools`="n_schools_mean",
        `Trend`="att8_trend_slope"
      )
      col_map <- col_map[unlist(col_map) %in% names(ind_data)]

      tbl <- ind_data %>%
        select(!!!setNames(unlist(col_map), names(col_map))) %>%
        mutate(across(where(is.numeric), ~round(.,2))) %>%
        arrange(Cluster, LA)

      for (pct_col in c("Good+ Ofsted","% Sel.","% Acad.")) {
        if (pct_col %in% names(tbl))
          tbl[[pct_col]] <- paste0(round(as.numeric(tbl[[pct_col]])*100,1),"%")
      }

      dt <- datatable(
        tbl, rownames=FALSE, filter="top",
        class="compact stripe hover",
        extensions="Buttons",
        options=list(pageLength=20, scrollX=TRUE, dom="Bfrtip",
                     buttons=c("csv","excel"),
                     columnDefs=list(list(targets="_all", className="dt-center")))
      )
      if ("ATT8" %in% names(tbl))
        dt <- dt %>% formatStyle("ATT8",
          background=styleColorBar(range(la_ind$att8_mean,na.rm=TRUE),"#cfe2ff"),
          backgroundSize="98% 70%", backgroundRepeat="no-repeat",
          backgroundPosition="center")
      if ("% FSM" %in% names(tbl))
        dt <- dt %>% formatStyle("% FSM",
          background=styleColorBar(range(la_ind$pct_fsm_mean,na.rm=TRUE),"#ffd6d6"),
          backgroundSize="98% 70%", backgroundRepeat="no-repeat",
          backgroundPosition="center")
      dt
    })

  })
}
