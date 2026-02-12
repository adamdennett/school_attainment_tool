# mod_la_overview.R - Local Authority Overview tab module
# -------------------------------------------------------
# Shows all schools in a selected LA with:
#   - Map coloured by residual (over/under-performing)
#   - Table of schools sorted by residual
#   - Distribution comparison (LA vs national)
# -------------------------------------------------------

mod_la_overview_ui <- function(id) {
  ns <- NS(id)

  layout_sidebar(
    sidebar = sidebar(
      width = 300,
      title = "Select Local Authority",

      selectInput(
        ns("la_select"),
        "Local Authority",
        choices = la_choices,
        selected = if (length(la_choices) > 0) la_choices[1] else NULL
      ),

      selectInput(
        ns("year_select"),
        "Academic Year",
        choices = year_choices,
        selected = latest_year
      ),

      hr(),

      # LA summary stats
      uiOutput(ns("la_summary"))
    ),

    layout_columns(
      col_widths = c(6, 6),

      card(
        card_header("Schools in LA (coloured by model residual)"),
        leafletOutput(ns("la_map"), height = "400px")
      ),

      card(
        card_header("Attainment Distribution: LA vs National"),
        plotlyOutput(ns("dist_chart"), height = "400px")
      )
    ),

    card(
      card_header("All Schools in LA"),
      DTOutput(ns("la_table"))
    )
  )
}


mod_la_overview_server <- function(id, selected_outcome) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ---- LA data ----
    la_data <- reactive({
      req(input$la_select, input$year_select)
      outcome <- selected_outcome()
      oc <- OUTCOME_CONFIG[[outcome]]

      panel_data %>%
        filter(
          LANAME == input$la_select,
          year_label == input$year_select
        ) %>%
        mutate(
          actual = !!sym(oc$var),
          predicted = !!sym(oc$pred_var),
          residual = actual - predicted
        )
    })

    # ---- LA summary ----
    output$la_summary <- renderUI({
      ld <- la_data()
      if (nrow(ld) == 0) return(tags$p("No data for this selection"))

      outcome <- selected_outcome()
      oc <- OUTCOME_CONFIG[[outcome]]

      n_schools <- nrow(ld)
      mean_att <- round(mean(ld$actual, na.rm = TRUE), 1)
      mean_resid <- round(mean(ld$residual, na.rm = TRUE), 2)
      n_above <- sum(ld$residual > 0, na.rm = TRUE)
      n_below <- sum(ld$residual < 0, na.rm = TRUE)

      tags$div(
        tags$p(tags$strong("Schools: "), n_schools),
        tags$p(tags$strong("Mean ", oc$label, ": "), mean_att),
        tags$p(tags$strong("Mean Residual: "),
               span(style = paste0("color:", ifelse(mean_resid >= 0, "#28a745", "#dc3545")),
                    round(mean_resid, 2))),
        tags$p(
          span(class = "text-success", paste(n_above, "above")),
          " / ",
          span(class = "text-danger", paste(n_below, "below")),
          " expected"
        )
      )
    })

    # ---- LA Map ----
    output$la_map <- renderLeaflet({
      leaflet() %>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        setView(lng = -1.5, lat = 52.5, zoom = 6)
    })

    observe({
      ld <- la_data()
      if (nrow(ld) == 0) return()

      # Get coordinates
      coords <- schools_spatial %>%
        filter(URN %in% ld$URN) %>%
        mutate(
          lng = st_coordinates(.)[, 1],
          lat = st_coordinates(.)[, 2]
        ) %>%
        st_drop_geometry() %>%
        select(URN, lng, lat)

      map_data <- ld %>%
        left_join(coords, by = "URN") %>%
        filter(!is.na(lng), !is.na(lat))

      if (nrow(map_data) == 0) return()

      # Diverging palette for residuals
      # Guard against all-NA residuals (e.g. no predictions for this LA/year)
      non_na_resids <- map_data$residual[!is.na(map_data$residual)]
      if (length(non_na_resids) == 0) {
        max_abs_resid <- 1  # fallback domain
      } else {
        max_abs_resid <- max(abs(non_na_resids))
        if (max_abs_resid == 0) max_abs_resid <- 1
      }
      pal <- colorNumeric(
        palette = c("#dc3545", "#ffc107", "#28a745"),
        domain = c(-max_abs_resid, max_abs_resid),
        na.color = "#CCCCCC"
      )

      leafletProxy(ns("la_map")) %>%
        clearMarkers() %>%
        clearControls() %>%
        addCircleMarkers(
          data = map_data,
          lng = ~lng, lat = ~lat,
          radius = ~pmin(pmax(sqrt(TOTPUPS) / 4, 4), 14),
          color = ~pal(residual),
          fillOpacity = 0.8,
          stroke = TRUE,
          weight = 1,
          popup = ~paste0(
            "<strong>", SCHNAME, "</strong><br>",
            "Actual: ", round(actual, 1), "<br>",
            "Predicted: ", round(predicted, 1), "<br>",
            "Residual: ", ifelse(residual >= 0, "+", ""),
            round(residual, 1)
          )
        ) %>%
        addLegend(
          position = "bottomright",
          pal = pal,
          values = c(-max_abs_resid, max_abs_resid),
          title = "Residual<br>(Actual - Predicted)",
          opacity = 0.8
        ) %>%
        fitBounds(
          lng1 = min(map_data$lng) - 0.02,
          lat1 = min(map_data$lat) - 0.02,
          lng2 = max(map_data$lng) + 0.02,
          lat2 = max(map_data$lat) + 0.02
        )
    })

    # ---- Distribution chart ----
    output$dist_chart <- renderPlotly({
      ld <- la_data()
      yr <- input$year_select
      outcome <- selected_outcome()
      oc <- OUTCOME_CONFIG[[outcome]]

      national <- panel_data %>%
        filter(year_label == yr) %>%
        pull(!!sym(oc$var))

      la_vals <- ld$actual

      plot_ly(alpha = 0.5) %>%
        add_histogram(
          x = national,
          name = "National",
          marker = list(color = "#6c757d"),
          nbinsx = 40,
          histnorm = "probability density"
        ) %>%
        add_histogram(
          x = la_vals,
          name = input$la_select,
          marker = list(color = "#0d6efd"),
          nbinsx = 20,
          histnorm = "probability density"
        ) %>%
        layout(
          barmode = "overlay",
          xaxis = list(title = oc$label),
          yaxis = list(title = "Density"),
          showlegend = TRUE,
          legend = list(orientation = "h", y = -0.15)
        ) %>%
        config(displayModeBar = FALSE)
    })

    # ---- LA Table ----
    output$la_table <- renderDT({
      ld <- la_data()
      if (nrow(ld) == 0) return(NULL)

      outcome <- selected_outcome()
      oc <- OUTCOME_CONFIG[[outcome]]

      table_data <- ld %>%
        arrange(desc(residual)) %>%
        select(
          School = SCHNAME,
          Actual = actual,
          Predicted = predicted,
          Residual = residual,
          `% Disadvantaged` = PTFSM6CLA1A,
          `Absence (%)` = PERCTOT,
          Pupils = TOTPUPS,
          Ofsted = OFSTEDRATING_1
        ) %>%
        mutate(across(where(is.numeric), ~ round(., 1)))

      datatable(
        table_data,
        options = list(
          pageLength = 20,
          order = list(list(3, "desc"))
        ),
        rownames = FALSE,
        class = "compact stripe hover"
      ) %>%
        formatStyle(
          "Residual",
          color = styleInterval(0, c("#dc3545", "#28a745")),
          fontWeight = "bold"
        )
    })
  })
}
