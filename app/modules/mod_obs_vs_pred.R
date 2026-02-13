# mod_obs_vs_pred.R - Observed vs Predicted scatter plot module
# ----------------------------------------------------------------
# Displays interactive scatter plots of observed ATT8 scores (y-axis)
# against core-model fitted values (x-axis) for each of the three
# outcome groups.  Users can filter by academic year and highlight
# schools within a selected Local Authority in orange.
# ----------------------------------------------------------------


# ---- UI ----

mod_obs_vs_pred_ui <- function(id) {
  ns <- NS(id)

  layout_sidebar(
    sidebar = sidebar(
      title = "Filters",
      width = 280,
      selectInput(
        ns("year_select"),
        label = "Academic Year",
        choices = NULL  # populated server-side
      ),
      selectInput(
        ns("la_select"),
        label = "Highlight Local Authority",
        choices = NULL,  # populated server-side
        selected = NULL
      ),
      tags$hr(),
      tags$p(class = "text-muted small",
             "Schools in the selected LA are shown in orange. ",
             "All other schools are shown in blue. ",
             "Grey dashed line = perfect prediction; ",
             "red line = linear regression fit.")
    ),
    # Three scatter plots stacked vertically
    card(
      card_header("All Pupils: Observed vs Predicted ATT8"),
      plotlyOutput(ns("scatter_all"), height = "380px")
    ),
    card(
      card_header("Disadvantaged Pupils: Observed vs Predicted ATT8"),
      plotlyOutput(ns("scatter_disadv"), height = "380px")
    ),
    card(
      card_header("Non-Disadvantaged Pupils: Observed vs Predicted ATT8"),
      plotlyOutput(ns("scatter_nondisadv"), height = "380px")
    )
  )
}


# ---- Server ----

mod_obs_vs_pred_server <- function(id) {
  moduleServer(id, function(input, output, session) {

    # Populate dropdowns
    observe({
      updateSelectInput(session, "year_select",
                        choices = year_choices,
                        selected = latest_year)
      updateSelectInput(session, "la_select",
                        choices = c("(none)" = "", la_choices),
                        selected = "")
    })

    # Reactive: filtered data for the selected year
    year_data <- reactive({
      req(input$year_select)
      panel_data %>%
        filter(year_label == input$year_select)
    })

    # Helper to build a scatter plot for one outcome
    make_scatter <- function(obs_var, pred_var, la_name) {

      d <- year_data() %>%
        filter(!is.na(!!sym(obs_var)), !is.na(!!sym(pred_var)))

      if (nrow(d) == 0) return(plotly_empty() %>% layout(title = "No data"))

      # R-squared for annotation
      r2_val <- cor(d[[obs_var]], d[[pred_var]], use = "complete.obs")^2

      # Determine if we are highlighting an LA
      has_highlight <- !is.null(la_name) && nzchar(la_name)

      # Colour: orange for selected LA, blue for everything else
      if (has_highlight) {
        d <- d %>%
          mutate(
            highlight = ifelse(as.character(LANAME) == la_name,
                               "Selected LA", "Other"),
            highlight = factor(highlight, levels = c("Other", "Selected LA"))
          ) %>%
          arrange(highlight)  # draw highlighted points on top
      } else {
        d <- d %>%
          mutate(highlight = factor("Other", levels = c("Other", "Selected LA")))
      }

      # Tooltip text
      d <- d %>%
        mutate(
          tooltip = paste0(
            SCHNAME, " (", LANAME, ")\n",
            "Observed: ", round(.data[[obs_var]], 1), "\n",
            "Predicted: ", round(.data[[pred_var]], 1), "\n",
            "Residual: ", round(.data[[obs_var]] - .data[[pred_var]], 1)
          )
        )

      # Axis range for 1:1 line
      rng <- range(c(d[[obs_var]], d[[pred_var]]), na.rm = TRUE)

      # Regression line data (pre-compute to avoid geom_smooth + ggplotly issues)
      fit <- lm(y ~ x, data = data.frame(y = d[[obs_var]], x = d[[pred_var]]))
      line_df <- data.frame(x = rng)
      line_df$y <- predict(fit, newdata = line_df)

      colour_map <- c("Other" = "#1565c0", "Selected LA" = "#e65100")
      alpha_map  <- c("Other" = 0.25, "Selected LA" = 0.85)

      p <- ggplot(d, aes(x = .data[[pred_var]], y = .data[[obs_var]],
                         colour = highlight, alpha = highlight,
                         text = tooltip)) +
        geom_abline(slope = 1, intercept = 0, colour = "grey60",
                    linewidth = 0.5, linetype = "dashed") +
        geom_point(size = 1.5) +
        geom_line(data = line_df, aes(x = x, y = y),
                  inherit.aes = FALSE,
                  colour = "#c62828", linewidth = 0.6) +
        scale_colour_manual(values = colour_map, drop = FALSE) +
        scale_alpha_manual(values = alpha_map, drop = FALSE, guide = "none") +
        coord_cartesian(xlim = rng, ylim = rng) +
        labs(x = "Predicted (core model)", y = "Observed",
             colour = NULL,
             title = paste0("\u00b2 = ", round(r2_val, 3),
                            "   n = ", scales::comma(nrow(d)))) +
        theme_minimal(base_size = 11) +
        theme(
          legend.position = if (has_highlight) "bottom" else "none",
          plot.title = element_text(size = 10, colour = "grey40")
        )

      ggplotly(p, tooltip = "text") %>%
        layout(legend = list(orientation = "h", x = 0.5,
                             xanchor = "center", y = -0.15))
    }

    # Render the three scatter plots
    output$scatter_all <- renderPlotly({
      make_scatter("ATT8SCR", "predicted_ATT8SCR_core",
                   input$la_select)
    })

    output$scatter_disadv <- renderPlotly({
      make_scatter("ATT8SCR_FSM6CLA1A", "predicted_ATT8SCR_FSM6CLA1A_core",
                   input$la_select)
    })

    output$scatter_nondisadv <- renderPlotly({
      make_scatter("ATT8SCR_NFSM6CLA1A", "predicted_ATT8SCR_NFSM6CLA1A_core",
                   input$la_select)
    })
  })
}
