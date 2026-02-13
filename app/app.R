# app.R - Main Shiny application: School Attainment Policy Simulator
# -------------------------------------------------------------------
# Assembles all modules into a tabbed interface with a global
# outcome model switcher.
# -------------------------------------------------------------------

# Load data and models
source("global.R")

# Load modules
source("modules/mod_school_selector.R")
source("modules/mod_simulator.R")
source("modules/mod_la_overview.R")
source("modules/mod_model_info.R")
source("modules/mod_obs_vs_pred.R")


# ---- UI ----

ui <- page_navbar(
  title = "School Attainment Policy Simulator",
  id = "main_nav",

  theme = bs_theme(
    version = 5,
    bootswatch = "flatly",
    primary = "#0d6efd",
    "navbar-bg" = "#2c3e50"
  ),

  # Global outcome model switcher in the navbar
  header = tags$div(
    class = "container-fluid bg-light border-bottom py-2",
    layout_columns(
      col_widths = c(4, 8),
      tags$div(
        class = "d-flex align-items-center",
        tags$label(class = "me-2 fw-bold", "Outcome:"),
        radioButtons(
          "outcome_select",
          label = NULL,
          choices = c(
            "All Pupils" = "all",
            "Disadvantaged" = "disadvantaged",
            "Non-Disadvantaged" = "non_disadvantaged"
          ),
          selected = "all",
          inline = TRUE
        )
      ),
      tags$div(
        class = "d-flex align-items-center justify-content-end text-muted",
        tags$small(
          "Multilevel model: schools nested in Local Authorities | ",
          "Data: DfE 2021-22 to 2024-25"
        )
      )
    )
  ),

  # ---- Tab 1: School Finder ----
  nav_panel(
    title = "School Finder",
    icon = icon("search"),
    mod_school_selector_ui("school_finder")
  ),

  # ---- Tab 2: Policy Simulator ----
  nav_panel(
    title = "Policy Simulator",
    icon = icon("sliders-h"),
    mod_simulator_ui("simulator")
  ),

  # ---- Tab 3: LA Overview ----
  nav_panel(
    title = "LA Overview",
    icon = icon("map"),
    mod_la_overview_ui("la_overview")
  ),

  # ---- Tab 4: Observed vs Predicted ----
  nav_panel(
    title = "Observed vs Predicted",
    icon = icon("braille"),
    mod_obs_vs_pred_ui("obs_vs_pred")
  ),

  # ---- Tab 5: Model Information ----
  nav_panel(
    title = "Model Info",
    icon = icon("info-circle"),
    mod_model_info_ui("model_info")
  ),

  # ---- About ----
  nav_spacer(),
  nav_item(
    tags$a(
      href = "https://www.compare-school-performance.service.gov.uk/",
      target = "_blank",
      icon("external-link-alt"),
      "DfE Data"
    )
  )
)


# ---- Server ----

server <- function(input, output, session) {

  # Shared reactive: which outcome model is selected
  selected_outcome <- reactive({
    input$outcome_select
  })

  # Shared reactive: which school URN is selected
  selected_school_urn <- reactiveVal(NULL)

  # ---- Module servers ----

  go_simulator <- mod_school_selector_server(
    "school_finder",
    selected_outcome = selected_outcome,
    selected_school_urn = selected_school_urn
  )

  mod_simulator_server(
    "simulator",
    selected_outcome = selected_outcome,
    selected_school_urn = selected_school_urn
  )

  mod_la_overview_server(
    "la_overview",
    selected_outcome = selected_outcome
  )

  mod_obs_vs_pred_server("obs_vs_pred")

  mod_model_info_server(
    "model_info",
    selected_outcome = selected_outcome
  )

  # ---- Navigate to simulator when button clicked ----
  observeEvent(go_simulator(), {
    updateNavbarPage(session, "main_nav", selected = "Policy Simulator")
  })
}


# ---- Run App ----

shinyApp(ui = ui, server = server)
