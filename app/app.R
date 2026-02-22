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
source("modules/mod_la_typology.R")


# ---- UI ----

ui <- page_navbar(
  title = "School Attainment Policy Simulator",
  id = "main_nav",

  # Load custom CSS
  tags$head(
    tags$link(rel = "stylesheet", href = "custom.css")
  ),

  theme = bs_theme(
    version = 5,
    bootswatch = "flatly",
    primary = "#0070ba",
    secondary = "#1C1B3A",
    success = "#2ca02c",
    danger = "#E20164",
    "navbar-bg" = "#001022",
    "navbar-dark-color" = "rgba(255,255,255,0.9)",
    "navbar-dark-hover-color" = "#E20164",
    "navbar-dark-active-color" = "#FFFFFF",
    "body-color" = "#333333",
    "headings-color" = "#1C1B3A",
    "link-color" = "#0070ba",
    "link-hover-color" = "#E20164",
    "card-border-color" = "rgba(0,112,186,0.15)"
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

  # ---- Tab 0: Welcome / About ----
  nav_panel(
    title = "About",
    icon = icon("home"),
    # ---- Full-width hero banner (outside container) ----
    tags$div(
      class = "ai4ci-hero",
      tags$div(
        class = "ai4ci-hero-overlay",
        tags$h1("School Attainment Policy Simulator"),
        tags$p(
          class = "ai4ci-hero-subtitle",
          "Moving beyond raw league tables to understand what drives attainment"
        ),
        tags$p(
          class = "ai4ci-hero-credit",
          "Designed by ",
          tags$strong("Prof Adam Dennett"),
          " | ",
          tags$a(href = "https://ai4ci.ac.uk/", target = "_blank",
                 "UKRI AI for Collective Intelligence Research Hub"),
          " | with assistance from Claude AI"
        ),
        # ---- Logos ----
        tags$div(
          class = "ai4ci-hero-logos",
          tags$a(
            href = "https://ai4ci.ac.uk/", target = "_blank",
            tags$img(src = "ai4ci_logo.svg", alt = "AI4CI: AI for Collective Intelligence",
                     style = "height: 50px; filter: brightness(0) invert(1);")
          ),
          tags$a(
            href = "https://www.ukri.org/", target = "_blank",
            tags$img(src = "ukri_logo_white.png", alt = "UKRI: UK Research and Innovation",
                     style = "height: 45px;")
          )
        )
      )
    ),

    tags$div(
      class = "container py-4", style = "max-width: 960px;",

      # ---- Purpose ----
      card(
        class = "mb-3",
        card_body(
          tags$h5(icon("bullseye"), " What is this tool?"),
          tags$p(
            "This simulator is designed for school leaders, governors and policy makers ",
            "who want to move beyond raw attainment and progress statistics and understand their drivers."),
          tags$p(
            "It allows you to compare individual schools against others with similar characteristics across ",
            "England and to see how each school compares with what we would ",
            tags$em("expect"), " its attainment to be, given those characteristics."
          )
        )
      ),

      # ---- Model ----
      card(
        class = "mb-3",
        card_body(
          tags$h5(icon("chart-line"), " The model behind the numbers"),
          tags$p(
            "Underpinning the expected attainment figures is a linear mixed effects (multilevel) regression model ",
            "(schools nested within Local Authorities and Regions) that explains ",
            tags$strong("up to 85% of the variation"), " in Attainment 8 scores across state secondary ",
            "in England over four years."),
          tags$p("This is a very good model. And means that, on average, school-level attainment is both predictable and to some extent, controllable. The sliders in the policy simulator allow you to experiment with this control."),
          tags$p("Where schools do better or worse than expected, this is attributable to other unobserved factors ",
            "which might include things like leadership quality, teaching practice, culture, and ethos."
          ),
          tags$p(
            "Separate models are fitted for all pupils, disadvantaged pupils, and non-disadvantaged ",
            "pupils. Use the ", tags$strong("Outcome"), " selector in the header bar to switch between them."
          ),
          tags$p(
            "For a fuller explanation of the modelling methodology, diagnostics, and results, see the ",
            tags$a(href = "https://adamdennett.github.io/school_attainment_tool/",
                   target = "_blank",
                   icon("book-open"),
                   " detailed model documentation"),
            "."
          )
        )
      ),

      # ---- Tabs guide ----
      card(
        class = "mb-3",
        card_body(
          tags$h5(icon("compass"), " Guide to the tabs"),
          tags$div(
            class = "table-responsive",
            tags$table(
              class = "table table-sm align-middle mb-0",
              tags$thead(
                tags$tr(
                  tags$th(style = "width: 180px;", "Tab"),
                  tags$th("What it does")
                )
              ),
              tags$tbody(
                tags$tr(
                  tags$td(icon("search"), " School Finder"),
                  tags$td(
                    "Search for a school by name or Local Authority. View it on a map, see ",
                    "key statistics, and send it to the Policy Simulator for deeper analysis."
                  )
                ),
                tags$tr(
                  tags$td(icon("braille"), " Observed vs Predicted"),
                  tags$td(
                    "An interactive scatter plot comparing every school's actual attainment ",
                    "against its model-predicted value. Schools above the line are outperforming; ",
                    "those below are underperforming relative to expectations."
                  )
                ),
                tags$tr(
                  tags$td(icon("sliders-h"), " Policy Simulator"),
                  tags$td(
                    "The core tool. Adjust school-level variables \u2014 absence rates, ",
                    "pupil composition, teacher retention, leadership pay, and more \u2014 to see ",
                    "the predicted impact on attainment. Crucially, some of these relationships ",
                    "are non-linear, so the simulator reveals where small changes matter most ",
                    "and where returns diminish."
                  )
                ),
                tags$tr(
                  tags$td(icon("map"), " LA Overview"),
                  tags$td(
                    "A Local Authority-level view showing how schools within an LA perform ",
                    "relative to expectations, with a distribution chart comparing the LA ",
                    "against the national picture."
                  )
                ),
                tags$tr(
                  tags$td(icon("layer-group"), " LA Typology"),
                  tags$td(
                    "A data-driven classification of Local Authorities into clusters based ",
                    "on over 200 indicators. Explore which LAs face similar challenges ",
                    "and compare their profiles on an interactive map."
                  )
                ),
                tags$tr(
                  tags$td(icon("chart-area"), " Cluster Analysis"),
                  tags$td(
                    "Radar charts and summary statistics for each LA cluster, making it easy ",
                    "to compare groups and identify shared characteristics."
                  )
                ),
                tags$tr(
                  tags$td(icon("info-circle"), " Model Info"),
                  tags$td(
                    "Technical detail: model coefficients, fit statistics, diagnostics, ",
                    "and caveats. Useful for understanding the model's strengths and limitations."
                  )
                )
              )
            )
          )
        )
      ),

      # ---- How to use ----
      card(
        class = "mb-3",
        card_body(
          tags$h5(icon("rocket"), " Getting started"),
          tags$ol(
            tags$li(
              tags$strong("Find your school"), " \u2014 use the ",
              tags$em("School Finder"), " tab to locate it by name or LA."
            ),
            tags$li(
              tags$strong("Check performance"), " \u2014 see how it compares to its predicted ",
              "attainment in ", tags$em("Observed vs Predicted"), "."
            ),
            tags$li(
              tags$strong("Simulate change"), " \u2014 open the ",
              tags$em("Policy Simulator"), " to explore which levers could improve outcomes, ",
              "paying particular attention to where non-linear effects mean small changes ",
              "can have outsized impact."
            ),
            tags$li(
              tags$strong("Explore context"), " \u2014 use ", tags$em("LA Overview"),
              " and ", tags$em("LA Typology"),
              " to understand the wider environment your school operates in."
            )
          )
        )
      ),

      # ---- Data note ----
      tags$div(
        class = "text-muted text-center small mt-3",
        tags$p(
          "Data: DfE school performance tables, 2021-22 to 2024-25. ",
          "Model: multilevel linear regression (lme4). ",
          "All figures are modelled estimates and should be interpreted alongside professional judgement."
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

  # ---- Tab 2: Observed vs Predicted ----
  nav_panel(
    title = "Observed vs Predicted",
    icon = icon("braille"),
    mod_obs_vs_pred_ui("obs_vs_pred")
  ),

  # ---- Tab 3: Policy Simulator ----
  nav_panel(
    title = "Policy Simulator",
    icon = icon("sliders-h"),
    mod_simulator_ui("simulator")
  ),

  # ---- Tab 4: LA Overview ----
  nav_panel(
    title = "LA Overview",
    icon = icon("map"),
    mod_la_overview_ui("la_overview")
  ),

  # ---- Tab 5: LA Typology Map ----
  nav_panel(
    title = "LA Typology",
    icon = icon("layer-group"),
    if (!is.null(la_typology) && !is.null(cluster_meta_data)) {
      mod_la_typology_ui("la_typology")
    } else {
      card(
        card_body(
          tags$div(
            class = "text-center p-5",
            icon("triangle-exclamation", style = "font-size:48px; color:#f0ad4e;"),
            tags$h4("Typology data not yet generated"),
            tags$p("Run ", tags$code("source('R/07_la_typology.R')"),
                   " from the project root to build the LA typology."),
            tags$p("This will create the necessary data files in ", tags$code("data/"), ".")
          )
        )
      )
    }
  ),

  # ---- Tab 6: Cluster Analysis ----
  nav_panel(
    title = "Cluster Analysis",
    icon = icon("chart-area"),
    if (!is.null(la_typology) && !is.null(cluster_meta_data)) {
      mod_la_cluster_ui("la_typology")
    } else {
      card(
        card_body(
          tags$div(
            class = "text-center p-5",
            icon("triangle-exclamation", style = "font-size:48px; color:#f0ad4e;"),
            tags$h4("Typology data not yet generated"),
            tags$p("Run ", tags$code("source('R/07_la_typology.R')"),
                   " from the project root to build the LA typology."),
            tags$p("This will create the necessary data files in ", tags$code("data/"), ".")
          )
        )
      )
    }
  ),

  # ---- Tab 7: Model Information ----
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

  mod_obs_vs_pred_server(
    "obs_vs_pred",
    selected_outcome = selected_outcome
  )

  mod_model_info_server(
    "model_info",
    selected_outcome = selected_outcome
  )

  if (!is.null(la_typology) && !is.null(cluster_meta_data)) {
    mod_la_typology_server("la_typology")
  }

  # ---- Navigate to simulator when button clicked ----
  observeEvent(go_simulator(), {
    updateNavbarPage(session, "main_nav", selected = "Policy Simulator")
  })
}


# ---- Run App ----

shinyApp(ui = ui, server = server)
