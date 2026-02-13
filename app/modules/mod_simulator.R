# mod_simulator.R - Policy Simulator tab module
# ------------------------------------------------
# The core feature: users adjust school-level variables
# via sliders and see the predicted impact on attainment.
# ------------------------------------------------

mod_simulator_ui <- function(id) {
  ns <- NS(id)

  layout_sidebar(
    sidebar = sidebar(
      width = 380,
      title = "Adjust Parameters",

      # Show selected school name
      uiOutput(ns("school_header")),

      hr(),

      # Dynamic sliders - one for each adjustable variable
      uiOutput(ns("sliders")),

      hr(),

      actionButton(
        ns("reset_sliders"),
        "Reset to Current Values",
        class = "btn-outline-secondary w-100",
        icon = icon("undo")
      )
    ),

    # Main panel: results
    layout_columns(
      col_widths = c(6, 6),

      # Left: headline numbers
      card(
        card_header("Predicted Impact"),
        uiOutput(ns("headline_numbers"))
      ),

      # Right: equity comparison
      card(
        card_header("Equity Comparison"),
        plotlyOutput(ns("equity_chart"), height = "250px")
      )
    ),

    layout_columns(
      col_widths = c(7, 5),

      # Waterfall chart
      card(
        card_header("Contribution of Each Change"),
        plotlyOutput(ns("waterfall_chart"), height = "350px")
      ),

      # Trend chart
      card(
        card_header("Attainment Trend"),
        plotlyOutput(ns("trend_chart"), height = "350px")
      )
    )
  )
}


mod_simulator_server <- function(id, selected_outcome, selected_school_urn) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ---- School data for the selected school (most recent year) ----
    current_school <- reactive({
      urn <- selected_school_urn()
      if (is.null(urn) || urn == "") return(NULL)

      panel_data %>%
        filter(URN == urn) %>%
        arrange(desc(year_numeric)) %>%
        slice(1)
    })

    # ---- School header ----
    output$school_header <- renderUI({
      sd <- current_school()
      if (is.null(sd)) {
        return(tags$p(class = "text-muted",
                      "Select a school in the School Finder tab first"))
      }
      tags$div(
        tags$h6(class = "mb-0", sd$SCHNAME),
        tags$small(class = "text-muted",
                   paste(sd$LANAME, "|", sd$year_label))
      )
    })

    # ---- Dynamic sliders ----
    output$sliders <- renderUI({
      sd <- current_school()
      if (is.null(sd)) return(NULL)

      slider_list <- map(names(slider_config), function(var_name) {
        cfg <- slider_config[[var_name]]
        current_val <- sd[[var_name]]

        if (is.na(current_val)) return(NULL)

        # Calculate slider range
        min_val <- max(current_val + cfg$min_change, 0.01)
        max_val <- current_val + cfg$max_change

        tags$div(
          class = "mb-3",
          tags$label(
            class = "form-label",
            paste0(cfg$display_name, " (current: ", round(current_val, 1), cfg$unit, ")")
          ),
          sliderInput(
            ns(paste0("slider_", var_name)),
            label = NULL,
            min = round(min_val, 2),
            max = round(max_val, 2),
            value = round(current_val, 2),
            step = cfg$step,
            width = "100%"
          )
        )
      })

      # Remove NULLs and return
      tagList(compact(slider_list))
    })

    # ---- Reset sliders ----
    observeEvent(input$reset_sliders, {
      sd <- current_school()
      if (is.null(sd)) return()

      for (var_name in names(slider_config)) {
        current_val <- sd[[var_name]]
        if (!is.na(current_val)) {
          updateSliderInput(session, paste0("slider_", var_name),
                           value = round(current_val, 2))
        }
      }
    })

    # ---- Compute modifications from slider values ----
    modifications <- reactive({
      sd <- current_school()
      if (is.null(sd)) return(list())

      mods <- list()
      for (var_name in names(slider_config)) {
        slider_id <- paste0("slider_", var_name)
        slider_val <- input[[slider_id]]
        current_val <- sd[[var_name]]

        if (!is.null(slider_val) && !is.na(current_val)) {
          change <- slider_val - current_val
          if (abs(change) > 0.001) {
            mods[[var_name]] <- change
          }
        }
      }
      mods
    })

    # ---- Scenario prediction ----
    scenario_result <- reactive({
      sd <- current_school()
      mods <- modifications()
      outcome <- selected_outcome()

      if (is.null(sd) || nrow(sd) == 0) return(NULL)

      model <- core_models[[outcome]]
      if (is.null(model)) return(NULL)

      predict_scenario(model, sd, mods)
    })

    # ---- Headline numbers ----
    output$headline_numbers <- renderUI({
      result <- scenario_result()
      outcome <- selected_outcome()
      oc <- OUTCOME_CONFIG[[outcome]]

      if (is.null(result)) {
        return(tags$p(class = "text-muted", "Select a school to see predictions"))
      }

      change_color <- if (result$change >= 0) "#28a745" else "#dc3545"
      change_icon <- if (result$change >= 0) "arrow-up" else "arrow-down"
      change_label <- if (result$change >= 0) "improvement" else "decline"

      tags$div(
        class = "text-center p-3",
        tags$p(class = "text-muted mb-1", oc$description),
        layout_columns(
          col_widths = c(4, 4, 4),
          tags$div(
            tags$small(class = "text-muted", "Baseline"),
            tags$h3(class = "mb-0", round(result$baseline, 1))
          ),
          tags$div(
            tags$small(class = "text-muted", "Scenario"),
            tags$h3(class = "mb-0", style = paste0("color:", change_color),
                    round(result$scenario, 1))
          ),
          tags$div(
            tags$small(class = "text-muted", "Change"),
            tags$h3(
              class = "mb-0",
              style = paste0("color:", change_color),
              icon(change_icon),
              paste0(ifelse(result$change >= 0, "+", ""),
                     round(result$change, 2))
            ),
            tags$small(
              style = paste0("color:", change_color),
              paste0("(", ifelse(result$pct_change >= 0, "+", ""),
                     round(result$pct_change, 1), "%)")
            )
          )
        )
      )
    })

    # ---- Waterfall chart ----
    output$waterfall_chart <- renderPlotly({
      sd <- current_school()
      mods <- modifications()
      outcome <- selected_outcome()

      if (is.null(sd) || length(mods) == 0) {
        return(plotly_empty() %>%
                 layout(title = list(text = "Adjust sliders to see contributions",
                                     font = list(color = "#999"))))
      }

      model <- core_models[[outcome]]
      decomp <- decompose_scenario(model, sd, mods)

      if (nrow(decomp) == 0) return(plotly_empty())

      decomp <- decomp %>%
        mutate(
          color = ifelse(marginal_effect >= 0, "#28a745", "#dc3545"),
          hover_text = paste0(
            display_name, "<br>",
            "Current: ", round(current_value, 1), "<br>",
            "Scenario: ", round(scenario_value, 1), "<br>",
            "Effect: ", ifelse(marginal_effect >= 0, "+", ""),
            round(marginal_effect, 3), " ATT8 points"
          )
        ) %>%
        arrange(desc(abs(marginal_effect)))

      plot_ly(
        data = decomp,
        x = ~reorder(display_name, abs(marginal_effect)),
        y = ~marginal_effect,
        type = "bar",
        marker = list(color = ~color),
        text = ~hover_text,
        hoverinfo = "text"
      ) %>%
        layout(
          xaxis = list(title = ""),
          yaxis = list(title = "Effect on Attainment 8"),
          showlegend = FALSE,
          margin = list(b = 100)
        ) %>%
        config(displayModeBar = FALSE)
    })

    # ---- Equity comparison chart ----
    output$equity_chart <- renderPlotly({
      sd <- current_school()
      mods <- modifications()

      if (is.null(sd) || nrow(sd) == 0) {
        return(plotly_empty() %>%
                 layout(title = list(text = "Select a school",
                                     font = list(color = "#999"))))
      }

      equity <- predict_equity_comparison(models, sd, mods)

      if (nrow(equity) == 0) return(plotly_empty())

      # Filter to just the three pupil groups (not the gap)
      equity_pupils <- equity %>% filter(model != "gap")

      plot_ly(data = equity_pupils) %>%
        add_bars(
          x = ~label, y = ~baseline,
          name = "Baseline",
          marker = list(color = "#6c757d"),
          text = ~paste0("Baseline: ", round(baseline, 1)),
          hoverinfo = "text"
        ) %>%
        add_bars(
          x = ~label, y = ~scenario,
          name = "Scenario",
          marker = list(color = ~ifelse(change >= 0, "#28a745", "#dc3545")),
          text = ~paste0("Scenario: ", round(scenario, 1),
                         "<br>Change: ", ifelse(change >= 0, "+", ""),
                         round(change, 2)),
          hoverinfo = "text"
        ) %>%
        layout(
          barmode = "group",
          xaxis = list(title = ""),
          yaxis = list(title = "Attainment 8"),
          showlegend = TRUE,
          legend = list(orientation = "h", y = -0.2)
        ) %>%
        config(displayModeBar = FALSE)
    })

    # ---- Trend chart ----
    output$trend_chart <- renderPlotly({
      urn <- selected_school_urn()
      outcome <- selected_outcome()
      oc <- OUTCOME_CONFIG[[outcome]]

      if (is.null(urn) || urn == "") {
        return(plotly_empty() %>%
                 layout(title = list(text = "Select a school",
                                     font = list(color = "#999"))))
      }

      trend_data <- panel_data %>%
        filter(URN == urn) %>%
        arrange(year_label) %>%
        select(year_label, actual = !!sym(oc$var), predicted = !!sym(oc$pred_var))

      if (nrow(trend_data) == 0) return(plotly_empty())

      plot_ly(data = trend_data) %>%
        add_trace(
          x = ~year_label, y = ~actual,
          type = "scatter", mode = "lines+markers",
          name = "Actual",
          line = list(color = "#0d6efd", width = 2),
          marker = list(size = 8)
        ) %>%
        add_trace(
          x = ~year_label, y = ~predicted,
          type = "scatter", mode = "lines+markers",
          name = "Model Predicted",
          line = list(color = "#6c757d", dash = "dash", width = 2),
          marker = list(size = 6)
        ) %>%
        layout(
          xaxis = list(title = "Academic Year"),
          yaxis = list(title = oc$label),
          showlegend = TRUE,
          legend = list(orientation = "h", y = -0.2),
          hovermode = "x unified"
        ) %>%
        config(displayModeBar = FALSE)
    })
  })
}
