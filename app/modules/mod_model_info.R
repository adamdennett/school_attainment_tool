# mod_model_info.R - Model Information tab module
# ------------------------------------------------
# Displays:
#   - Fixed effects table for all 3 models side by side
#   - Random effects variance components
#   - R-squared, ICC
#   - Diagnostic plots
#   - Variable descriptions and caveats
# ------------------------------------------------

mod_model_info_ui <- function(id) {
  ns <- NS(id)

  tagList(
    layout_columns(
      col_widths = c(8, 4),

      card(
        card_header("Fixed Effects Comparison (all 3 models)"),
        DTOutput(ns("fixed_effects_table"))
      ),

      card(
        card_header("Model Fit Summary"),
        uiOutput(ns("model_fit_summary"))
      )
    ),

    layout_columns(
      col_widths = c(6, 6),

      card(
        card_header("Random Effects Variance Components"),
        DTOutput(ns("random_effects_table"))
      ),

      card(
        card_header("Residuals vs Fitted"),
        plotlyOutput(ns("resid_plot"), height = "350px")
      )
    ),

    card(
      card_header("Variable Descriptions"),
      uiOutput(ns("variable_descriptions"))
    ),

    card(
      card_header("Caveats and Limitations"),
      uiOutput(ns("caveats"))
    )
  )
}


mod_model_info_server <- function(id, selected_outcome) {
  moduleServer(id, function(input, output, session) {

    # ---- Fixed effects comparison table ----
    output$fixed_effects_table <- renderDT({

      # Extract fixed effects from all models
      fe_list <- map(names(diagnostics), function(mn) {
        fe <- diagnostics[[mn]]$fixed_effects
        fe$model <- mn
        fe
      })

      fe_all <- bind_rows(fe_list)

      # Pivot to wide format for comparison
      fe_wide <- fe_all %>%
        select(Variable, Estimate, model) %>%
        pivot_wider(
          names_from = model,
          values_from = Estimate,
          names_prefix = "est_"
        ) %>%
        mutate(
          Variable = variable_display_name(Variable),
          across(starts_with("est_"), ~ round(., 4))
        )

      # Rename columns
      col_names <- names(fe_wide)
      col_names <- str_replace(col_names, "est_all", "All Pupils")
      col_names <- str_replace(col_names, "est_disadvantaged", "Disadvantaged")
      col_names <- str_replace(col_names, "est_non_disadvantaged", "Non-Disadvantaged")
      names(fe_wide) <- col_names

      datatable(
        fe_wide,
        options = list(dom = "t", paging = FALSE, ordering = FALSE),
        rownames = FALSE,
        class = "compact stripe"
      ) %>%
        formatStyle(
          columns = 2:4,
          color = styleInterval(0, c("#dc3545", "#28a745"))
        )
    })

    # ---- Model fit summary ----
    output$model_fit_summary <- renderUI({
      tags$div(
        class = "p-2",
        map(names(diagnostics), function(mn) {
          d <- diagnostics[[mn]]
          label <- OUTCOME_CONFIG[[mn]]$label

          tags$div(
            class = "mb-3",
            tags$h6(label),
            tags$table(
              class = "table table-sm table-borderless",
              tags$tr(
                tags$td(tags$small("R\u00B2 marginal")),
                tags$td(tags$strong(round(d$r2_marginal, 4)))
              ),
              tags$tr(
                tags$td(tags$small("R\u00B2 conditional")),
                tags$td(tags$strong(round(d$r2_conditional, 4)))
              ),
              tags$tr(
                tags$td(tags$small("Observations")),
                tags$td(tags$strong(format(d$n_obs, big.mark = ",")))
              ),
              tags$tr(
                tags$td(tags$small("Residual \u03C3")),
                tags$td(tags$strong(round(d$sigma, 4)))
              )
            )
          )
        })
      )
    })

    # ---- Random effects table ----
    output$random_effects_table <- renderDT({
      outcome <- selected_outcome()
      d <- diagnostics[[outcome]]

      if (is.null(d$var_components)) return(NULL)

      vc <- d$var_components %>%
        select(grp, var1, vcov, sdcor) %>%
        mutate(
          `Group` = grp,
          `Variable` = ifelse(is.na(var1), "(Intercept)", var1),
          `Variance` = round(vcov, 6),
          `Std.Dev.` = round(sdcor, 4)
        ) %>%
        select(Group, Variable, Variance, `Std.Dev.`)

      datatable(
        vc,
        options = list(dom = "t", paging = FALSE),
        rownames = FALSE,
        class = "compact stripe"
      )
    })

    # ---- Residuals vs Fitted plot ----
    output$resid_plot <- renderPlotly({
      outcome <- selected_outcome()
      oc <- OUTCOME_CONFIG[[outcome]]

      # Use pre-computed residuals from app data bundle
      resid_data <- model_resid[[outcome]]
      if (is.null(resid_data)) return(plotly_empty())

      fitted_vals <- resid_data$fitted
      resid_vals <- resid_data$residual

      plot_ly(
        x = fitted_vals,
        y = resid_vals,
        type = "scatter",
        mode = "markers",
        marker = list(
          color = "#6c757d",
          opacity = 0.3,
          size = 4
        ),
        hoverinfo = "text",
        text = paste0("Fitted: ", round(fitted_vals, 3),
                      "<br>Residual: ", round(resid_vals, 3))
      ) %>%
        add_trace(
          x = range(fitted_vals, na.rm = TRUE),
          y = c(0, 0),
          type = "scatter",
          mode = "lines",
          line = list(color = "#dc3545", dash = "dash"),
          showlegend = FALSE,
          hoverinfo = "none"
        ) %>%
        layout(
          xaxis = list(title = "Fitted values (log scale)"),
          yaxis = list(title = "Residuals"),
          showlegend = FALSE
        ) %>%
        config(displayModeBar = FALSE)
    })

    # ---- Variable descriptions ----
    output$variable_descriptions <- renderUI({
      tags$div(
        class = "p-3",
        tags$table(
          class = "table table-sm",
          tags$thead(
            tags$tr(
              tags$th("Variable"),
              tags$th("Description"),
              tags$th("In Model As")
            )
          ),
          tags$tbody(
            tags$tr(
              tags$td("PTFSM6CLA1A"),
              tags$td("Percentage of pupils who are disadvantaged (eligible for free school meals in last 6 years or looked-after children)"),
              tags$td("log(PTFSM6CLA1A)")
            ),
            tags$tr(
              tags$td("PERCTOT"),
              tags$td("Overall absence rate (% of half-day sessions missed)"),
              tags$td("log(PERCTOT)")
            ),
            tags$tr(
              tags$td("PNUMEAL"),
              tags$td("Percentage of pupils with English as an additional language"),
              tags$td("log(PNUMEAL)")
            ),
            tags$tr(
              tags$td("PTPRIORLO"),
              tags$td("Percentage of pupils with low prior attainment (at Key Stage 2)"),
              tags$td("PTPRIORLO")
            ),
            tags$tr(
              tags$td("ADMPOL_PT"),
              tags$td("School admissions policy type"),
              tags$td("ADMPOL_PT")
            ),
            tags$tr(
              tags$td("Gorard Segregation"),
              tags$td("Local authority-level segregation index (0 = no segregation, higher = more segregated)"),
              tags$td("gorard_segregation")
            ),
            tags$tr(
              tags$td("Teacher Retention"),
              tags$td("Number of teachers who remained in the same school between census years"),
              tags$td("log(remained_in_the_same_school)")
            ),
            tags$tr(
              tags$td("Leadership Pay %"),
              tags$td("Percentage of teachers on the leadership pay range"),
              tags$td("teachers_on_leadership_pay_range_percent")
            ),
            tags$tr(
              tags$td("Teacher Sick Days"),
              tags$td("Average number of sick days taken per teacher"),
              tags$td("log(average_number_of_days_taken)")
            ),
            tags$tr(
              tags$td("Year"),
              tags$td("Academic year as a numeric trend (0 = 2021-22, 3 = 2024-25)"),
              tags$td("year_numeric")
            )
          )
        ),
        tags$p(class = "text-muted mt-2",
               tags$em("Note: log() indicates natural logarithm transformation. ",
                       "Random intercepts are fitted for Ofsted rating and ",
                       "Local Authority (nested within Government Office Region)."))
      )
    })

    # ---- Caveats ----
    output$caveats <- renderUI({
      tags$div(
        class = "p-3",
        tags$ul(
          tags$li("The model captures ", tags$strong("associations"), ", not causal effects. ",
                  "Changing a school's absence rate in reality involves complex interventions ",
                  "that may have other effects not captured here."),
          tags$li("Predictions are based on the ", tags$strong("pooled panel model"),
                  " across 4 academic years (2021-22 to 2024-25). This assumes the ",
                  "relationships between predictors and outcomes are stable over time."),
          tags$li("For 2024-25, absence data covers ", tags$strong("autumn and spring terms only"),
                  " (not the full academic year), which may slightly underestimate full-year absence rates."),
          tags$li("Ofsted ratings are based on the ", tags$strong("last inspection before September 2024"),
                  " when Ofsted abolished single-word overall effectiveness grades."),
          tags$li("2023-24 and 2024-25 use ", tags$strong("provisional"), " KS4 results, ",
                  "which may differ slightly from final published figures."),
          tags$li("The model explains variation ", tags$strong("between schools"),
                  " but cannot predict what would happen to a specific school if its ",
                  "characteristics changed. The policy simulator provides indicative estimates only."),
          tags$li("Schools with very small cohorts, unusual characteristics, or missing data ",
                  "may have less reliable predictions.")
        ),
        tags$p(class = "text-muted",
               tags$em("Data sources: DfE Performance Tables, Explore Education Statistics, ",
                       "Ofsted Official Statistics, GIAS (Edubase)."))
      )
    })
  })
}
