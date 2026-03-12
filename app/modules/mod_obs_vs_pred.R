# mod_obs_vs_pred.R - Observed vs Predicted scatter plot module
# ----------------------------------------------------------------
# Displays an interactive scatter plot of observed ATT8 scores (y-axis)
# against core-model fitted values (x-axis) for the outcome group
# selected via the global radio buttons.  Users can filter by
# academic year and highlight schools by Local Authority or by
# LA typology cluster.
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

      tags$hr(),

      radioButtons(
        ns("highlight_mode"), "Highlight by",
        choices = c("None" = "none",
                    "LA Cluster" = "cluster"),
        selected = "none", inline = TRUE
      ),

      # Cluster selector — shown when highlight_mode == "cluster"
      conditionalPanel(
        condition = sprintf("input['%s'] == 'cluster'", ns("highlight_mode")),
        uiOutput(ns("cluster_selector"))
      ),

      # LA overlay — available alongside any highlight mode
      tags$hr(),
      checkboxInput(
        ns("la_overlay"), "Overlay Local Authority", value = FALSE
      ),
      conditionalPanel(
        condition = sprintf("input['%s'] == true", ns("la_overlay")),
        selectInput(
          ns("la_overlay_select"),
          label = NULL,
          choices = NULL,
          selected = NULL
        ),
        checkboxInput(
          ns("show_school_names"), "Show school names", value = FALSE
        )
      ),

      tags$hr(),
      tags$p(class = "text-muted small",
             "Highlight by LA to show one authority in orange, ",
             "or by cluster to colour schools by their LA typology group. ",
             "Use 'Overlay Local Authority' to mark schools in a specific LA ",
             "with black dots and dotted residual lines, plus a ",
             "residual bar chart panel on the right. ",
             "Grey dashed line = perfect prediction; ",
             "red line = linear regression fit.")
    ),
    card(
      card_header(textOutput(ns("scatter_title"))),
      uiOutput(ns("scatter_with_residual_panel"))
    )
  )
}


# ---- Server ----

mod_obs_vs_pred_server <- function(id, selected_outcome) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ---- Cluster metadata (from global scope) ----
    has_clusters <- !is.null(la_typology) && !is.null(cluster_meta_data)

    if (has_clusters) {
      chosen_k     <- cluster_meta_data$chosen_k
      cluster_labs <- cluster_meta_data$cluster_labels
      cluster_cols <- cluster_meta_data$cluster_colours

      # Build LA → cluster lookup
      la_cluster_lookup <- la_typology %>%
        select(LANAME, cluster, cluster_label) %>%
        distinct()
    }

    # ---- Populate dropdowns ----
    observe({
      updateSelectInput(session, "year_select",
                        choices = year_choices,
                        selected = latest_year)
      updateSelectInput(session, "la_select",
                        choices = c("(choose)" = "", la_choices),
                        selected = "")
      updateSelectInput(session, "la_overlay_select",
                        choices = c("(choose)" = "", la_choices),
                        selected = "")
    })

    # ---- Cluster radio buttons with coloured labels ----
    output$cluster_selector <- renderUI({
      if (!has_clusters) {
        return(tags$p(class = "text-muted small",
                      "Cluster data not available. Run R/07_la_typology.R."))
      }

      # Build radio buttons with cluster-coloured squares
      choice_labels <- purrr::map(1:chosen_k, function(k) {
        tags$span(
          tags$span(style = paste0(
            "display:inline-block;width:12px;height:12px;border-radius:2px;",
            "background:", cluster_cols[k], ";margin-right:5px;vertical-align:middle;"
          )),
          tags$span(cluster_labs[k], style = "font-size:12px;")
        )
      })
      names(choice_labels) <- as.character(1:chosen_k)

      # "All clusters" option
      all_label <- tags$span(
        tags$span(style = paste0(
          "display:inline-block;width:12px;height:12px;border-radius:2px;",
          "background:linear-gradient(135deg,", cluster_cols[1], ",", cluster_cols[min(chosen_k,3)],
          ");margin-right:5px;vertical-align:middle;"
        )),
        tags$span("All clusters", style = "font-size:12px;font-weight:bold;")
      )
      choice_labels <- c(list("all" = all_label), choice_labels)

      radioButtons(
        ns("cluster_select"), label = NULL,
        choiceNames  = unname(choice_labels),
        choiceValues = c("all", as.character(1:chosen_k)),
        selected     = "all"
      )
    })

    # ---- Reactive: filtered data for the selected year ----
    year_data <- reactive({
      req(input$year_select)
      panel_data %>%
        filter(year_label == input$year_select)
    })

    # ---- Helper: build the scatter plot ----
    make_scatter <- function(obs_var, pred_var, highlight_mode,
                             la_name = NULL, cluster_sel = NULL,
                             la_overlay_name = NULL,
                             show_school_names = FALSE) {

      d <- year_data() %>%
        filter(!is.na(!!sym(obs_var)), !is.na(!!sym(pred_var)))

      if (nrow(d) == 0) return(plotly_empty() %>% layout(title = "No data"))

      # R-squared
      r2_val <- cor(d[[obs_var]], d[[pred_var]], use = "complete.obs")^2

      # --- Highlighting logic ---
      if (highlight_mode == "la" && !is.null(la_name) && nzchar(la_name)) {
        # Single LA highlight (original behaviour)
        d <- d %>%
          mutate(
            highlight = ifelse(as.character(LANAME) == la_name,
                               "Selected LA", "Other"),
            highlight = factor(highlight, levels = c("Other", "Selected LA"))
          ) %>%
          arrange(highlight)

        colour_map <- c("Other" = "#1565c0", "Selected LA" = "#e65100")
        alpha_map  <- c("Other" = 0.25, "Selected LA" = 0.85)
        show_legend <- TRUE

      } else if (highlight_mode == "cluster" && has_clusters) {
        # Cluster highlighting
        d <- d %>%
          left_join(la_cluster_lookup, by = "LANAME")

        if (is.null(cluster_sel) || cluster_sel == "all") {
          # Show all clusters in their colours
          d <- d %>%
            mutate(
              highlight = ifelse(is.na(cluster_label), "Unassigned",
                                 cluster_label),
              highlight = factor(highlight,
                                 levels = c(cluster_labs, "Unassigned"))
            )
          colour_map <- setNames(c(cluster_cols, "#cccccc"),
                                 c(cluster_labs, "Unassigned"))
          alpha_map  <- setNames(c(rep(0.55, chosen_k), 0.15),
                                 c(cluster_labs, "Unassigned"))
          show_legend <- TRUE
        } else {
          # Single cluster highlighted
          k <- as.integer(cluster_sel)
          sel_label <- cluster_labs[k]
          d <- d %>%
            mutate(
              highlight = ifelse(!is.na(cluster) & cluster == k,
                                 sel_label, "Other"),
              highlight = factor(highlight, levels = c("Other", sel_label))
            ) %>%
            arrange(highlight)
          colour_map <- setNames(c("#bbbbbb", cluster_cols[k]),
                                 c("Other", sel_label))
          alpha_map  <- setNames(c(0.15, 0.85),
                                 c("Other", sel_label))
          show_legend <- TRUE
        }

      } else {
        # No highlighting
        d <- d %>%
          mutate(highlight = factor("All schools"))
        colour_map <- c("All schools" = "#1565c0")
        alpha_map  <- c("All schools" = 0.25)
        show_legend <- FALSE
      }

      # Tooltip text (include cluster label if available)
      d <- d %>%
        mutate(
          tooltip = paste0(
            SCHNAME, " (", LANAME, ")",
            if ("cluster_label" %in% names(d))
              paste0("\nCluster: ", ifelse(is.na(cluster_label), "—", cluster_label))
            else "",
            "\nObserved: ", round(.data[[obs_var]], 1),
            "\nPredicted: ", round(.data[[pred_var]], 1),
            "\nResidual: ", round(.data[[obs_var]] - .data[[pred_var]], 1)
          )
        )

      # Axis range
      rng <- range(c(d[[obs_var]], d[[pred_var]]), na.rm = TRUE)

      # Regression line
      fit <- lm(y ~ x, data = data.frame(y = d[[obs_var]], x = d[[pred_var]]))
      line_df <- data.frame(x = rng)
      line_df$y <- predict(fit, newdata = line_df)

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
             title = paste0("R\u00b2 = ", round(r2_val, 3),
                            "   n = ", scales::comma(nrow(d)))) +
        theme_minimal(base_size = 11) +
        theme(
          legend.position = if (show_legend) "bottom" else "none",
          plot.title = element_text(size = 10, colour = "grey40")
        )

      # LA overlay: add black dots + residual lines for schools in the selected LA
      if (!is.null(la_overlay_name) && nzchar(la_overlay_name)) {
        overlay_d <- d %>% filter(as.character(LANAME) == la_overlay_name)
        if (nrow(overlay_d) > 0) {
          # Dotted vertical residual lines (predicted → observed)
          p <- p +
            geom_segment(
              data = overlay_d,
              aes(x = .data[[pred_var]], xend = .data[[pred_var]],
                  y = .data[[pred_var]], yend = .data[[obs_var]]),
              colour = "black", alpha = 0.5,
              linetype = "dotted", linewidth = 0.4,
              inherit.aes = FALSE,
              show.legend = FALSE
            ) +
            geom_point(
              data = overlay_d,
              aes(x = .data[[pred_var]], y = .data[[obs_var]]),
              shape = 16, size = 2.5,
              colour = "black", alpha = 0.85,
              inherit.aes = FALSE,
              show.legend = FALSE
            )
        }
      }

      plt <- ggplotly(p, tooltip = "text") %>%
        layout(legend = list(orientation = "h", x = 0.5,
                             xanchor = "center", y = -0.15))

      # Add school name annotations via plotly (ggrepel doesn't survive ggplotly)
      if (show_school_names && !is.null(la_overlay_name) && nzchar(la_overlay_name)) {
        overlay_d <- d %>% filter(as.character(LANAME) == la_overlay_name)
        if (nrow(overlay_d) > 0) {
          annotations <- lapply(seq_len(nrow(overlay_d)), function(i) {
            list(
              x = overlay_d[[pred_var]][i],
              y = overlay_d[[obs_var]][i],
              text = as.character(overlay_d$SCHNAME[i]),
              xanchor = "left",
              yanchor = "middle",
              xshift = 8,
              showarrow = FALSE,
              font = list(size = 9, color = "black")
            )
          })
          plt <- plt %>% layout(annotations = annotations)
        }
      }

      plt
    }

    # ---- Get config for the selected outcome ----
    outcome_cfg <- reactive({
      req(selected_outcome())
      OUTCOME_CONFIG[[selected_outcome()]]
    })

    # ---- Dynamic card header ----
    output$scatter_title <- renderText({
      paste0(outcome_cfg()$label, ": Observed vs Predicted ATT8")
    })

    # ---- Reactive: current LA overlay name ----
    la_overlay_name_r <- reactive({
      if (isTRUE(input$la_overlay) &&
          !is.null(input$la_overlay_select) &&
          nzchar(input$la_overlay_select)) {
        input$la_overlay_select
      } else {
        NULL
      }
    })

    # ---- Dynamic layout: scatter + optional residual panel ----
    output$scatter_with_residual_panel <- renderUI({
      la_ovl <- la_overlay_name_r()
      if (!is.null(la_ovl)) {
        fluidRow(
          column(8, plotlyOutput(ns("scatter_plot"), height = "550px")),
          column(4,
            plotlyOutput(ns("residual_panel"), height = "480px"),
            tags$div(
              style = "padding: 4px 8px; font-size: 11px; color: #555; line-height: 1.4;",
              tags$strong("Residuals"),
              " show how much better (", tags$span(style = "color:#2e7d32;", "green"),
              ") or worse (", tags$span(style = "color:#c62828;", "red"),
              ") a school performs compared to its model prediction, ",
              "in raw Attainment 8 / GCSE points. ",
              "A residual of +5 means the school's results are 5 points above ",
              "what the model expects for schools with similar characteristics. ",
              "This contextualises each school's performance against ",
              "the national pattern."
            )
          )
        )
      } else {
        plotlyOutput(ns("scatter_plot"), height = "550px")
      }
    })

    # ---- Render the scatter plot ----
    output$scatter_plot <- renderPlotly({
      cfg  <- outcome_cfg()
      mode <- input$highlight_mode %||% "none"

      make_scatter(
        obs_var          = cfg$var,
        pred_var         = cfg$pred_var,
        highlight_mode   = mode,
        la_name          = if (mode == "la") input$la_select else NULL,
        cluster_sel      = if (mode == "cluster") input$cluster_select else NULL,
        la_overlay_name  = la_overlay_name_r(),
        show_school_names = isTRUE(input$show_school_names) && !is.null(la_overlay_name_r())
      )
    })

    # ---- Render the residual side panel ----
    output$residual_panel <- renderPlotly({
      la_ovl <- la_overlay_name_r()
      req(la_ovl)
      cfg <- outcome_cfg()
      obs_var  <- cfg$var
      pred_var <- cfg$pred_var

      d <- year_data() %>%
        filter(!is.na(!!sym(obs_var)), !is.na(!!sym(pred_var)),
               as.character(LANAME) == la_ovl) %>%
        mutate(
          residual = .data[[obs_var]] - .data[[pred_var]],
          resid_col = ifelse(residual >= 0, "#2e7d32", "#c62828"),
          short_name = ifelse(nchar(as.character(SCHNAME)) > 30,
                              paste0(substr(as.character(SCHNAME), 1, 28), "\u2026"),
                              as.character(SCHNAME)),
          tooltip = paste0(ifelse(residual >= 0, "+", ""),
                           round(residual, 1), " pts")
        ) %>%
        arrange(residual)

      if (nrow(d) == 0) return(plotly_empty() %>% layout(title = "No data"))

      # Order factor for horizontal bar chart
      d$short_name <- factor(d$short_name, levels = d$short_name)

      plot_ly(d, y = ~short_name, x = ~residual, type = "bar",
              orientation = "h",
              marker = list(color = d$resid_col),
              hovertext = ~tooltip,
              hoverinfo = "text",
              textposition = "none") %>%
        layout(
          title = list(text = paste0(la_ovl, " — Residuals"),
                       font = list(size = 12)),
          xaxis = list(title = "Residual (ATT8 pts)", zeroline = TRUE,
                       zerolinecolor = "grey60", zerolinewidth = 1),
          yaxis = list(title = "", tickfont = list(size = 9)),
          margin = list(l = 140, r = 10, t = 35, b = 40),
          plot_bgcolor = "white",
          paper_bgcolor = "white"
        ) %>%
        config(displayModeBar = FALSE)
    })
  })
}
