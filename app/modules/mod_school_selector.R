# mod_school_selector.R - School Finder tab module
# -------------------------------------------------
# Provides:
#   - LA dropdown + school name autocomplete
#   - Leaflet map with all schools (coloured by attainment)
#   - School profile card with key stats
# -------------------------------------------------

mod_school_selector_ui <- function(id) {
  ns <- NS(id)

  layout_sidebar(
    sidebar = sidebar(
      width = 350,
      title = "Find a School",

      selectInput(
        ns("la_select"),
        "Local Authority",
        choices = c("All England" = "", la_choices),
        selected = ""
      ),

      selectizeInput(
        ns("school_search"),
        "Search by School Name",
        choices = NULL,
        options = list(
          placeholder = "Type to search...",
          maxOptions = 50
        )
      ),

      selectInput(
        ns("year_select"),
        "Academic Year",
        choices = year_choices,
        selected = latest_year
      ),

      hr(),

      # Selected school card
      uiOutput(ns("school_card")),

      actionButton(
        ns("go_simulator"),
        "Open in Policy Simulator",
        class = "btn-primary w-100 mt-3",
        icon = icon("sliders-h")
      )
    ),

    # Main panel: map + table
    card(
      card_header("School Map"),
      leafletOutput(ns("map"), height = "450px")
    ),

    card(
      card_header("School Profile"),
      DTOutput(ns("school_table"))
    )
  )
}


mod_school_selector_server <- function(id, selected_outcome, selected_school_urn) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ---- Reactive: filtered schools based on LA ----
    filtered_schools <- reactive({
      if (is.null(input$la_select) || input$la_select == "") {
        school_choices
      } else {
        school_choices %>% filter(LANAME == input$la_select)
      }
    })

    # ---- Update school search choices when LA changes ----
    observe({
      choices <- filtered_schools()
      updateSelectizeInput(
        session, "school_search",
        choices = setNames(choices$URN, choices$label),
        server = TRUE
      )
    })

    # ---- Selected school data ----
    school_data <- reactive({
      urn <- input$school_search
      yr <- input$year_select

      if (is.null(urn) || urn == "") return(NULL)

      panel_data %>%
        filter(URN == urn, year_label == yr) %>%
        slice(1)
    })

    # ---- Update the shared selected_school_urn reactive ----
    observe({
      req(input$school_search)
      selected_school_urn(input$school_search)
    })

    # ---- School profile card ----
    output$school_card <- renderUI({
      sd <- school_data()
      if (is.null(sd) || nrow(sd) == 0) {
        return(tags$div(class = "text-muted", "Select a school to see details"))
      }

      outcome <- selected_outcome()
      oc <- OUTCOME_CONFIG[[outcome]]

      actual <- sd[[oc$var]]
      predicted <- sd[[oc$pred_var]]
      residual <- if (!is.null(sd[[oc$resid_var]])) sd[[oc$resid_var]] else NA

      tags$div(
        class = "p-2",
        tags$h5(sd$SCHNAME),
        tags$p(class = "text-muted mb-1", paste(sd$LANAME, "|", sd$gor_name)),
        tags$p(class = "text-muted mb-2",
               paste("Ofsted:", ifelse(is.na(sd$OFSTEDRATING_1), "N/A", as.character(sd$OFSTEDRATING_1)))),
        tags$hr(),
        tags$div(
          class = "d-flex justify-content-between",
          tags$div(
            tags$small(class = "text-muted", paste("Actual", oc$label)),
            tags$h4(class = "mb-0",
                    ifelse(is.na(actual), "N/A", round(actual, 1)))
          ),
          tags$div(
            tags$small(class = "text-muted", "Model Predicted"),
            tags$h4(class = "mb-0",
                    ifelse(is.na(predicted), "N/A", round(predicted, 1)))
          )
        ),
        if (!is.na(residual)) {
          resid_class <- ifelse(residual > 0, "text-success", "text-danger")
          resid_label <- ifelse(residual > 0, "Above expected", "Below expected")
          tags$p(class = paste("mt-2 mb-0", resid_class),
                 paste(resid_label, "by", round(abs(residual), 1), "points"))
        }
      )
    })

    # ---- Leaflet map ----
    output$map <- renderLeaflet({
      leaflet() %>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        setView(lng = -1.5, lat = 52.5, zoom = 6)
    })

    # Update map markers when LA or outcome changes
    observe({
      outcome <- selected_outcome()
      oc <- OUTCOME_CONFIG[[outcome]]
      yr <- input$year_select

      # Get data for the selected year
      map_data <- panel_data %>%
        filter(year_label == yr) %>%
        select(URN, SCHNAME, LANAME, gor_name, TOTPUPS,
               all_of(c(oc$var, oc$pred_var))) %>%
        inner_join(
          schools_spatial %>% select(URN),
          by = "URN"
        )

      # Filter by LA if selected
      if (!is.null(input$la_select) && input$la_select != "") {
        map_data <- map_data %>% filter(LANAME == input$la_select)
      }

      if (nrow(map_data) == 0) return()

      # Get coordinates from the spatial object
      coords <- schools_spatial %>%
        filter(URN %in% map_data$URN) %>%
        mutate(
          lng = st_coordinates(.)[, 1],
          lat = st_coordinates(.)[, 2]
        ) %>%
        st_drop_geometry() %>%
        select(URN, lng, lat)

      map_data <- map_data %>%
        st_drop_geometry() %>%
        left_join(coords, by = "URN") %>%
        filter(!is.na(lng), !is.na(lat))

      # Colour palette based on attainment
      att_vals <- map_data[[oc$var]]
      pal <- colorNumeric(
        palette = "viridis",
        domain = att_vals,
        na.color = "#CCCCCC"
      )

      leafletProxy(ns("map")) %>%
        clearMarkers() %>%
        clearControls() %>%
        addCircleMarkers(
          data = map_data,
          lng = ~lng, lat = ~lat,
          radius = ~pmin(pmax(sqrt(TOTPUPS) / 4, 3), 12),
          color = ~pal(get(oc$var)),
          fillOpacity = 0.7,
          stroke = TRUE,
          weight = 1,
          popup = ~paste0(
            "<strong>", SCHNAME, "</strong><br>",
            LANAME, "<br>",
            oc$label, ": ", round(get(oc$var), 1), "<br>",
            "Pupils: ", TOTPUPS
          ),
          layerId = ~URN
        ) %>%
        addLegend(
          position = "bottomright",
          pal = pal,
          values = att_vals,
          title = oc$label,
          opacity = 0.8
        )

      # Zoom to LA if selected
      if (!is.null(input$la_select) && input$la_select != "" && nrow(map_data) > 0) {
        leafletProxy(ns("map")) %>%
          fitBounds(
            lng1 = min(map_data$lng) - 0.05,
            lat1 = min(map_data$lat) - 0.05,
            lng2 = max(map_data$lng) + 0.05,
            lat2 = max(map_data$lat) + 0.05
          )
      }
    })

    # Click on map marker to select school
    observeEvent(input$map_marker_click, {
      click <- input$map_marker_click
      if (!is.null(click$id)) {
        urn <- click$id
        # Update the school search to this URN
        label <- school_choices %>% filter(URN == urn) %>% pull(label)
        if (length(label) > 0) {
          updateSelectizeInput(session, "school_search",
                               selected = urn)
        }
      }
    })

    # ---- School profile table ----
    output$school_table <- renderDT({
      urn <- input$school_search
      if (is.null(urn) || urn == "") return(NULL)

      outcome <- selected_outcome()
      oc <- OUTCOME_CONFIG[[outcome]]

      profile <- panel_data %>%
        filter(URN == urn) %>%
        arrange(year_label) %>%
        select(
          Year = year_label,
          `Attainment 8` = !!sym(oc$var),
          `Predicted` = !!sym(oc$pred_var),
          `% Disadvantaged` = PTFSM6CLA1A,
          `Overall Absence (%)` = PERCTOT,
          `% EAL` = PNUMEAL,
          `% Low Prior` = PTPRIORLO,
          `Pupils` = TOTPUPS,
          Ofsted = OFSTEDRATING_1
        ) %>%
        mutate(across(where(is.numeric), ~ round(., 1)))

      datatable(
        profile,
        options = list(dom = "t", paging = FALSE, ordering = FALSE),
        rownames = FALSE,
        class = "compact stripe"
      )
    })

    # Return the go_simulator button click for navigation
    reactive(input$go_simulator)
  })
}
