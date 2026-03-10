# mod_school_twin.R — "Find My Twin" school matching module
# ----------------------------------------------------------
# Finds schools with similar contextual characteristics using
# Gower distance (handles mixed continuous/categorical data).
# Attainment and progress outcomes are deliberately excluded
# from matching but shown side-by-side for comparison.
# ----------------------------------------------------------


# ---- Constants ----

# Variable groups available for matching (all context/input, NOT outcomes)
TWIN_VAR_GROUPS <- list(
  "Pupil Intake" = c(
    "PTFSM6CLA1A",    # % disadvantaged
    "PNUMEAL",        # % EAL
    "PTPRIORLO",      # % low prior attainment (KS2)
    "PTPRIORHI",      # % high prior attainment (KS2)
    "PNUMFSMEVER",    # % FSM ever
    "PSENELK"         # % SEN support
  ),
  "School Characteristics" = c(
    "TOTPUPS",        # total pupils
    "ADMPOL_PT",      # admissions policy
    "SCHOOLTYPE",     # school type
    "gender_name",    # mixed / single-sex
    "RELCHAR"         # religious character
  ),
  "Absence" = c(
    "PERCTOT",        # % overall absence
    "PPERSABS10"      # % persistent absence
  ),
  "Workforce" = c(
    "remained_in_the_same_school",
    "teachers_on_leadership_pay_range_percent",
    "average_number_of_days_taken"
  ),
  "Context" = c(
    "gorard_segregation",  # LA-level segregation index
    "OFSTEDRATING_1"       # Ofsted rating (ordinal)
  )
)

# Human-readable labels for matching and outcome variables
TWIN_LABELS <- c(
  PTFSM6CLA1A = "% Disadvantaged",
  PNUMEAL = "% EAL",
  PTPRIORLO = "% Low Prior (KS2)",
  PTPRIORHI = "% High Prior (KS2)",
  PNUMFSMEVER = "% FSM Ever",
  PSENELK = "% SEN Support",
  TOTPUPS = "Total Pupils",
  ADMPOL_PT = "Admissions Policy",
  SCHOOLTYPE = "School Type",
  gender_name = "Gender",
  RELCHAR = "Religious Character",
  PERCTOT = "% Overall Absence",
  PPERSABS10 = "% Persistent Absence",
  remained_in_the_same_school = "Teacher Retention (FTE)",
  teachers_on_leadership_pay_range_percent = "% Leadership Pay",
  average_number_of_days_taken = "Avg. Sickness Days",
  gorard_segregation = "Segregation Index",
  OFSTEDRATING_1 = "Ofsted Rating",
  ATT8SCR = "Attainment 8",
  ATT8SCR_FSM6CLA1A = "Attainment 8 (Disadv.)",
  ATT8SCR_NFSM6CLA1A = "Attainment 8 (Non-Disadv.)",
  P8MEA = "Progress 8",
  P8MEA_FSM6CLA1A = "Progress 8 (Disadv.)",
  P8MEA_NFSM6CLA1A = "Progress 8 (Non-Disadv.)"
)

# Categorical variable names (need factor conversion for Gower)
TWIN_CAT_VARS <- c("ADMPOL_PT", "SCHOOLTYPE", "gender_name", "RELCHAR")


# ---- Helper functions ----

#' Format a value for display — NA-safe
twin_fmt <- function(x, digits = 1) {
  if (is.null(x) || length(x) == 0 || all(is.na(x))) return("N/A")
  if (is.numeric(x)) return(as.character(round(x, digits)))
  as.character(x)
}

#' Get a display label for a variable
twin_label <- function(var_name) {
  if (var_name %in% names(TWIN_LABELS)) TWIN_LABELS[[var_name]] else var_name
}


#' Compute Gower distance from a focal school to all others
#'
#' Shared helper: prepares data, converts types, computes pairwise Gower
#' distance via cluster::daisy, and returns the focal school's distance
#' vector. Both selection strategies (closest / divergent) use this.
#'
#' @param focal_urn Character URN of the focal school
#' @param year_data Data frame filtered to one academic year
#' @param match_vars Character vector of variable names to match on
#' @return List with focal_dists (named numeric vector) and n_schools, or NULL
prepare_gower_data <- function(focal_urn, year_data, match_vars) {

  available_vars <- intersect(match_vars, names(year_data))
  if (length(available_vars) < 2) return(NULL)

  match_data <- year_data %>%
    select(URN, all_of(available_vars)) %>%
    distinct(URN, .keep_all = TRUE)

  # Convert nominal categoricals to factor
  for (v in intersect(TWIN_CAT_VARS, available_vars)) {
    match_data[[v]] <- as.factor(match_data[[v]])
  }

  # OFSTEDRATING_1: ordered factor (so Gower uses ordinal distance)
  if ("OFSTEDRATING_1" %in% available_vars) {
    ofsted_order <- c("Inadequate", "Requires improvement", "Good", "Outstanding")
    current_vals <- unique(as.character(match_data$OFSTEDRATING_1))
    valid_levels <- ofsted_order[ofsted_order %in% current_vals]
    extra_levels <- setdiff(current_vals[!is.na(current_vals)], valid_levels)
    all_levels <- c(valid_levels, extra_levels)
    if (length(all_levels) > 0) {
      match_data$OFSTEDRATING_1 <- factor(
        match_data$OFSTEDRATING_1,
        levels = all_levels,
        ordered = TRUE
      )
    }
  }

  # Require at least 50% non-missing matching vars per school
  n_vars <- length(available_vars)
  non_na_count <- rowSums(!is.na(match_data %>% select(-URN)))
  match_data <- match_data[non_na_count >= n_vars * 0.5, ]

  if (!(focal_urn %in% match_data$URN)) return(NULL)
  if (nrow(match_data) < 2) return(NULL)

  match_matrix <- match_data %>% column_to_rownames("URN")
  gower_dist <- cluster::daisy(match_matrix, metric = "gower")
  gower_mat <- as.matrix(gower_dist)

  focal_dists <- gower_mat[as.character(focal_urn), ]
  focal_dists[as.character(focal_urn)] <- NA

  list(focal_dists = focal_dists, n_schools = nrow(match_data))
}


#' Select the N closest twins (smallest Gower distance)
#'
#' @param focal_dists Named numeric vector from prepare_gower_data
#' @param n_twins Integer number of twins to return
#' @return Tibble with URN, gower_distance, similarity_pct
select_closest_twins <- function(focal_dists, n_twins = 5) {
  n_available <- sum(!is.na(focal_dists))
  if (n_available == 0) return(NULL)

  ordered_idx <- order(focal_dists, na.last = TRUE)
  top_n <- ordered_idx[seq_len(min(n_twins, n_available))]

  tibble(
    URN = names(focal_dists)[top_n],
    gower_distance = unname(focal_dists[top_n]),
    similarity_pct = round((1 - unname(focal_dists[top_n])) * 100, 1)
  )
}


#' Select divergent twins: contextually similar but biggest attainment gap
#'
#' Filters to schools above a similarity threshold, then ranks by the
#' absolute difference in attainment compared to the focal school.
#'
#' @param focal_dists Named numeric vector from prepare_gower_data
#' @param focal_urn Character URN of the focal school
#' @param year_data Data frame filtered to one academic year
#' @param outcome_var Character name of the attainment variable (e.g. "ATT8SCR")
#' @param min_similarity Numeric minimum similarity percentage (0-100)
#' @param n_twins Integer number of twins to return
#' @return Tibble with URN, gower_distance, similarity_pct, att_gap
select_divergent_twins <- function(focal_dists, focal_urn, year_data,
                                   outcome_var, min_similarity = 80,
                                   n_twins = 5) {

  max_gower <- 1 - (min_similarity / 100)
  candidate_mask <- !is.na(focal_dists) & focal_dists <= max_gower
  if (sum(candidate_mask) == 0) return(NULL)

  candidate_urns  <- names(focal_dists)[candidate_mask]
  candidate_dists <- focal_dists[candidate_mask]

  # Focal school attainment
  focal_att <- year_data %>%
    filter(URN == focal_urn) %>%
    pull(!!sym(outcome_var)) %>%
    first()
  if (is.na(focal_att)) return(NULL)

  # Candidate attainment values
  candidate_att <- year_data %>%
    filter(URN %in% candidate_urns) %>%
    select(URN, twin_att_val = !!sym(outcome_var)) %>%
    distinct(URN, .keep_all = TRUE)

  tibble(
    URN = candidate_urns,
    gower_distance = unname(candidate_dists),
    similarity_pct = round((1 - unname(candidate_dists)) * 100, 1)
  ) %>%
    left_join(candidate_att, by = "URN") %>%
    mutate(att_gap = round(twin_att_val - focal_att, 1)) %>%
    filter(!is.na(att_gap)) %>%
    arrange(desc(abs(att_gap))) %>%
    slice_head(n = n_twins) %>%
    select(-twin_att_val)
}


# ---- UI ----

mod_school_twin_ui <- function(id) {
  ns <- NS(id)

  layout_sidebar(
    sidebar = sidebar(
      width = 350,
      title = "Find My Twin",

      selectizeInput(
        ns("school_search"),
        "Select a School",
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

      tags$label(class = "fw-bold mb-2", "Matching Dimensions"),
      tags$small(
        class = "text-muted d-block mb-2",
        "Select which variable groups to use for finding similar schools. ",
        "Attainment and progress are always excluded from matching."
      ),
      checkboxGroupInput(
        ns("var_groups"),
        label = NULL,
        choices = names(TWIN_VAR_GROUPS),
        selected = names(TWIN_VAR_GROUPS)
      ),

      hr(),

      radioButtons(
        ns("allow_cross_selective"),
        tags$span(
          "Allow selective / non-selective comparison?",
          tags$small(
            class = "text-muted d-block mt-1",
            "When 'No', only matches selective with selective ",
            "and non-selective with non-selective."
          )
        ),
        choices = c("Yes" = "yes", "No" = "no"),
        selected = "no",
        inline = TRUE
      ),

      hr(),

      tags$label(class = "fw-bold mb-2", "Match Mode"),
      radioButtons(
        ns("match_mode"),
        label = NULL,
        choices = c(
          "Closest twins" = "closest",
          "Divergent twins" = "divergent"
        ),
        selected = "closest"
      ),
      uiOutput(ns("mode_description")),

      conditionalPanel(
        condition = paste0("input['", ns("match_mode"), "'] == 'divergent'"),
        sliderInput(
          ns("sim_threshold"),
          "Minimum similarity (%)",
          min = 60, max = 95, value = 80, step = 5,
          post = "%"
        )
      ),

      sliderInput(
        ns("n_twins"),
        "Number of twins",
        min = 1, max = 10, value = 5, step = 1
      ),

      actionButton(
        ns("find_twins"),
        "Find Twins",
        class = "btn-primary w-100 mt-2",
        icon = icon("users")
      )
    ),

    # ---- Main panel ----

    # Focal school fact card
    uiOutput(ns("focal_card")),

    # Twin results table
    card(
      card_header(
        class = "d-flex justify-content-between align-items-center",
        tags$span("Twin Schools"),
        uiOutput(ns("match_info"), inline = TRUE)
      ),
      card_body(
        DTOutput(ns("twin_table")),
        uiOutput(ns("no_results_msg"))
      )
    ),

    # Profile comparison: radar + detailed diff
    layout_columns(
      col_widths = c(6, 6),
      card(
        card_header("Profile Comparison"),
        card_body(
          uiOutput(ns("radar_twin_label")),
          plotlyOutput(ns("radar_chart"), height = "450px")
        )
      ),
      card(
        card_header(uiOutput(ns("diff_header"), inline = TRUE)),
        card_body(
          tags$small(
            class = "text-muted d-block mb-2",
            HTML("<span style='background:#fff3cd;padding:1px 4px;'>Yellow</span> = outcomes &nbsp; ",
                 "<span style='background:#d6eaf8;padding:1px 4px;'>Blue</span> = not used for matching &nbsp; ",
                 "White = matched dimensions. ",
                 "Click a row in the twin table to compare a different school.")
          ),
          DTOutput(ns("diff_table"))
        )
      )
    )
  )
}


# ---- Server ----

mod_school_twin_server <- function(id, selected_outcome, selected_school_urn) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ---- Populate school search (server-side for performance) ----
    observe({
      updateSelectizeInput(
        session, "school_search",
        choices = setNames(school_choices$URN, school_choices$label),
        server = TRUE
      )
    })

    # ---- Sync with shared selected school URN ----
    observeEvent(selected_school_urn(), {
      urn <- selected_school_urn()
      if (!is.null(urn) && urn != "" && !identical(urn, input$school_search)) {
        updateSelectizeInput(session, "school_search", selected = urn)
      }
    })

    # ---- Focal school data (single row) ----
    focal_data <- reactive({
      urn <- input$school_search
      yr  <- input$year_select
      if (is.null(urn) || urn == "") return(NULL)

      panel_data %>%
        filter(URN == urn, year_label == yr) %>%
        slice(1)
    })

    # ---- Focal school fact card ----
    output$focal_card <- renderUI({
      fd <- focal_data()
      if (is.null(fd) || nrow(fd) == 0) {
        return(card(
          card_body(
            tags$div(
              class = "text-center text-muted py-4",
              icon("school", style = "font-size: 32px;"),
              tags$p(class = "mt-2", "Select a school and click 'Find Twins'")
            )
          )
        ))
      }

      oc <- OUTCOME_CONFIG[[selected_outcome()]]
      actual    <- fd[[oc$var]]
      predicted <- fd[[oc$pred_var]]
      p8        <- fd[["P8MEA"]]

      card(
        class = "mb-3 border-primary",
        card_body(
          # Header row
          tags$div(
            class = "d-flex justify-content-between align-items-start",
            tags$div(
              tags$h4(class = "mb-1", fd$SCHNAME),
              tags$p(
                class = "text-muted mb-1",
                paste0(fd$LANAME, " | ", fd$gor_name)
              ),
              tags$p(
                class = "text-muted mb-0 small",
                paste0(
                  "Type: ", twin_fmt(fd$SCHOOLTYPE),
                  " | Ofsted: ", twin_fmt(fd$OFSTEDRATING_1),
                  " | ", twin_fmt(fd$gender_name),
                  " | Admissions: ", twin_fmt(fd$ADMPOL_PT)
                )
              )
            ),
            tags$div(
              class = "text-end",
              tags$span(class = "badge bg-primary", paste("URN:", fd$URN)),
              tags$br(),
              tags$span(class = "badge bg-secondary mt-1", fd$year_label)
            )
          ),

          tags$hr(class = "my-2"),

          # Key metrics row
          layout_columns(
            col_widths = c(3, 3, 3, 3),
            tags$div(
              class = "text-center",
              tags$small(class = "text-muted d-block", oc$label),
              tags$h4(class = "mb-0 text-primary", twin_fmt(actual))
            ),
            tags$div(
              class = "text-center",
              tags$small(class = "text-muted d-block", "Predicted"),
              tags$h4(class = "mb-0", twin_fmt(predicted))
            ),
            tags$div(
              class = "text-center",
              tags$small(class = "text-muted d-block", "Progress 8"),
              tags$h4(class = "mb-0", twin_fmt(p8, 2))
            ),
            tags$div(
              class = "text-center",
              tags$small(class = "text-muted d-block", "Pupils"),
              tags$h4(class = "mb-0",
                      ifelse(is.na(fd$TOTPUPS), "N/A", comma(fd$TOTPUPS)))
            )
          ),

          tags$hr(class = "my-2"),

          # Context metrics row
          layout_columns(
            col_widths = c(2, 2, 2, 2, 2, 2),
            tags$div(
              class = "text-center",
              tags$small(class = "text-muted d-block", "% Disadv."),
              tags$strong(twin_fmt(fd$PTFSM6CLA1A))
            ),
            tags$div(
              class = "text-center",
              tags$small(class = "text-muted d-block", "% EAL"),
              tags$strong(twin_fmt(fd$PNUMEAL))
            ),
            tags$div(
              class = "text-center",
              tags$small(class = "text-muted d-block", "% Absence"),
              tags$strong(twin_fmt(fd$PERCTOT))
            ),
            tags$div(
              class = "text-center",
              tags$small(class = "text-muted d-block", "% Low Prior"),
              tags$strong(twin_fmt(fd$PTPRIORLO))
            ),
            tags$div(
              class = "text-center",
              tags$small(class = "text-muted d-block", "Retention"),
              tags$strong(twin_fmt(fd$remained_in_the_same_school))
            ),
            tags$div(
              class = "text-center",
              tags$small(class = "text-muted d-block", "Segregation"),
              tags$strong(twin_fmt(fd$gorard_segregation, 3))
            )
          )
        )
      )
    })


    # ---- Mode description (reactive text below radio buttons) ----
    output$mode_description <- renderUI({
      mode <- input$match_mode
      if (is.null(mode) || mode == "closest") {
        tags$small(class = "text-muted d-block mb-2",
          "Find schools most similar across all selected dimensions.")
      } else {
        tags$small(class = "text-muted d-block mb-2",
          "Find contextually similar schools with the biggest attainment ",
          "gaps \u2014 useful for identifying what drives different outcomes.")
      }
    })


    # ---- Twin computation (triggered by button) ----
    twin_results <- eventReactive(input$find_twins, {
      fd <- focal_data()
      if (is.null(fd) || nrow(fd) == 0) return(NULL)

      groups <- input$var_groups
      if (length(groups) == 0) return(NULL)

      yr   <- input$year_select
      n    <- input$n_twins
      mode <- input$match_mode

      # Collect matching variables from selected groups
      match_vars <- unlist(TWIN_VAR_GROUPS[groups], use.names = FALSE)
      match_vars <- intersect(match_vars, names(panel_data))
      if (length(match_vars) < 2) return(NULL)

      year_data <- panel_data %>% filter(year_label == yr)

      # Pre-filter: restrict to same selectivity group if toggle is "No"
      if (!is.null(input$allow_cross_selective) &&
          input$allow_cross_selective == "no" &&
          "ADMPOL_PT" %in% names(year_data)) {
        focal_sel <- fd$ADMPOL_PT
        # Treat "SEL" as selective; everything else as non-selective
        focal_is_sel <- !is.na(focal_sel) && focal_sel == "SEL"
        if (focal_is_sel) {
          year_data <- year_data %>%
            filter(ADMPOL_PT == "SEL" | URN == fd$URN)
        } else {
          year_data <- year_data %>%
            filter(ADMPOL_PT != "SEL" | is.na(ADMPOL_PT) | URN == fd$URN)
        }
      }

      oc <- OUTCOME_CONFIG[[selected_outcome()]]

      n_pool <- NULL

      withProgress(message = "Computing Gower distance ...", value = 0.3, {
        gower <- prepare_gower_data(fd$URN, year_data, match_vars)
        setProgress(0.7)

        if (is.null(gower)) { setProgress(1); return(NULL) }

        if (mode == "divergent") {
          threshold <- input$sim_threshold
          twins <- select_divergent_twins(
            gower$focal_dists, fd$URN, year_data, oc$var, threshold, n
          )
          # Count how many schools are in the similarity pool
          max_gower <- 1 - (threshold / 100)
          n_pool <<- sum(
            !is.na(gower$focal_dists) & gower$focal_dists <= max_gower
          )
        } else {
          twins <- select_closest_twins(gower$focal_dists, n)
        }
        setProgress(1)
      })

      if (is.null(twins) || nrow(twins) == 0) return(NULL)

      # Join with full data for display
      display_cols <- c(
        "URN", "SCHNAME", "LANAME", "gor_name",
        "TOTPUPS", "PTFSM6CLA1A", "PNUMEAL", "PTPRIORLO", "PTPRIORHI",
        "PERCTOT", "PPERSABS10", "OFSTEDRATING_1", "SCHOOLTYPE",
        "ADMPOL_PT", "gender_name", "RELCHAR",
        "remained_in_the_same_school",
        "teachers_on_leadership_pay_range_percent",
        "average_number_of_days_taken",
        "gorard_segregation", "PNUMFSMEVER", "PSENELK",
        oc$var, oc$pred_var,
        "P8MEA", "ATT8SCR", "ATT8SCR_FSM6CLA1A", "ATT8SCR_NFSM6CLA1A",
        "P8MEA_FSM6CLA1A", "P8MEA_NFSM6CLA1A"
      )
      display_cols <- intersect(display_cols, names(year_data))

      display_data <- year_data %>%
        filter(URN %in% twins$URN) %>%
        select(all_of(display_cols))

      twins <- twins %>%
        left_join(display_data, by = "URN")

      # Sort: closest mode by distance, divergent mode by abs attainment gap
      if (mode == "divergent" && "att_gap" %in% names(twins)) {
        twins <- twins %>% arrange(desc(abs(att_gap)))
      } else {
        twins <- twins %>% arrange(gower_distance)
      }

      # Track whether selectivity filter was applied
      sel_filter <- !is.null(input$allow_cross_selective) &&
                    input$allow_cross_selective == "no"

      list(
        twins = twins,
        match_vars = match_vars,
        n_schools = gower$n_schools,
        mode = mode,
        n_pool = n_pool,
        selectivity_filtered = sel_filter
      )
    }, ignoreNULL = TRUE)


    # ---- Match info badge ----
    output$match_info <- renderUI({
      res <- twin_results()
      if (is.null(res)) return(NULL)

      sel_tag <- if (isTRUE(res$selectivity_filtered)) " | like-for-like" else ""

      if (res$mode == "divergent") {
        tags$span(
          class = "badge bg-warning text-dark",
          paste0(
            length(res$match_vars), " vars | ",
            res$n_pool, " schools \u2265 ", input$sim_threshold,
            "% similar | sorted by attainment gap", sel_tag
          )
        )
      } else {
        tags$span(
          class = "badge bg-info",
          paste0(
            length(res$match_vars), " vars | ",
            res$n_schools, " schools compared", sel_tag
          )
        )
      }
    })

    # ---- No results message ----
    output$no_results_msg <- renderUI({
      # Only show after button is pressed
      if (input$find_twins == 0) return(NULL)
      res <- twin_results()
      if (!is.null(res)) return(NULL)

      tags$div(
        class = "text-center text-muted py-3",
        icon("triangle-exclamation", style = "color: #f0ad4e;"),
        tags$p(
          "No twins found. ",
          if (!is.null(input$match_mode) && input$match_mode == "divergent") {
            "Try lowering the minimum similarity threshold, or check that a school is selected."
          } else {
            "Check that a school is selected and at least two matching dimension groups are ticked."
          }
        )
      )
    })


    # ---- Twin comparison table ----
    output$twin_table <- renderDT({
      res <- twin_results()
      if (is.null(res)) return(NULL)

      fd  <- focal_data()
      oc  <- OUTCOME_CONFIG[[selected_outcome()]]
      att_var <- oc$var

      # Compute attainment gap vs focal school (available in both modes)
      focal_att <- if (!is.null(fd) && nrow(fd) > 0) fd[[att_var]] else NA_real_

      display <- res$twins %>%
        transmute(
          Rank       = row_number(),
          School     = SCHNAME,
          LA         = LANAME,
          Similarity = paste0(similarity_pct, "%"),
          Pupils     = TOTPUPS,
          `% Disadv.`   = round(PTFSM6CLA1A, 1),
          `% Absence`   = round(PERCTOT, 1),
          Ofsted        = as.character(OFSTEDRATING_1),
          `Att. 8`      = round(.data[[att_var]], 1),
          `Att. Gap`    = round(.data[[att_var]] - focal_att, 1),
          `Prog. 8`     = round(P8MEA, 2)
        )

      dt <- datatable(
        display,
        selection = list(mode = "single", selected = 1),
        options = list(
          dom = "t",
          paging = FALSE,
          ordering = FALSE,
          scrollX = TRUE,
          columnDefs = list(
            list(className = "dt-center", targets = "_all"),
            list(className = "dt-left",   targets = c(1, 2))
          )
        ),
        rownames = FALSE,
        class = "compact stripe hover"
      ) %>%
        formatStyle(
          columns = c("Att. 8", "Prog. 8"),
          backgroundColor = "#fff3cd",
          fontWeight = "bold"
        ) %>%
        formatStyle(
          "Att. Gap",
          color = styleInterval(
            c(-0.01, 0.01),
            c("#dc3545", "#6c757d", "#198754")
          ),
          fontWeight = "bold"
        )

      dt
    })


    # ---- Selected twin (click a row in the table to change) ----
    selected_twin_idx <- reactive({
      sel <- input$twin_table_rows_selected
      if (is.null(sel) || length(sel) == 0) 1L else sel[1]
    })


    # ---- Radar chart: focal vs selected twin ----
    output$radar_twin_label <- renderUI({
      res <- twin_results()
      if (is.null(res) || nrow(res$twins) == 0) return(NULL)
      idx <- min(selected_twin_idx(), nrow(res$twins))
      tags$small(
        class = "text-muted",
        paste0("Percentile rank comparison vs #", idx, " twin: ",
               res$twins$SCHNAME[idx],
               " (", res$twins$similarity_pct[idx], "% similar)")
      )
    })

    output$radar_chart <- renderPlotly({
      res <- twin_results()
      fd  <- focal_data()
      if (is.null(res) || is.null(fd) || nrow(fd) == 0) return(NULL)
      if (nrow(res$twins) == 0) return(NULL)

      # Use only continuous matching vars for the radar
      match_vars <- res$match_vars
      continuous_vars <- setdiff(match_vars, c(TWIN_CAT_VARS, "OFSTEDRATING_1"))
      continuous_vars <- intersect(continuous_vars, names(panel_data))
      if (length(continuous_vars) < 3) return(NULL)

      yr <- input$year_select
      year_data <- panel_data %>% filter(year_label == yr)

      # Compute percentile ranks (0-100) for normalisation
      pct_data <- year_data %>%
        select(URN, all_of(continuous_vars)) %>%
        mutate(across(-URN, ~ percent_rank(.) * 100))

      focal_pct <- pct_data %>% filter(URN == fd$URN)
      if (nrow(focal_pct) == 0) return(NULL)

      idx <- min(selected_twin_idx(), nrow(res$twins))
      sel_twin_urn <- res$twins$URN[idx]
      twin_pct <- pct_data %>% filter(URN == sel_twin_urn)
      if (nrow(twin_pct) == 0) return(NULL)

      # Build labels (short versions)
      labels <- vapply(continuous_vars, twin_label, character(1))
      labels <- str_wrap(labels, width = 18)

      # Extract values and close the polygon
      focal_vals <- c(as.numeric(focal_pct[1, continuous_vars]),
                      as.numeric(focal_pct[1, continuous_vars[1]]))
      twin_vals  <- c(as.numeric(twin_pct[1, continuous_vars]),
                      as.numeric(twin_pct[1, continuous_vars[1]]))
      theta <- c(labels, labels[1])

      plot_ly(type = "scatterpolar", fill = "toself") %>%
        add_trace(
          r = focal_vals, theta = theta,
          name = fd$SCHNAME,
          fillcolor = "rgba(0, 112, 186, 0.15)",
          line = list(color = "#0070ba", width = 2),
          marker = list(color = "#0070ba", size = 5)
        ) %>%
        add_trace(
          r = twin_vals, theta = theta,
          name = res$twins$SCHNAME[idx],
          fillcolor = "rgba(226, 1, 100, 0.15)",
          line = list(color = "#E20164", width = 2),
          marker = list(color = "#E20164", size = 5)
        ) %>%
        layout(
          polar = list(
            radialaxis = list(
              visible = TRUE,
              range = c(0, 100),
              ticksuffix = "%ile"
            )
          ),
          legend = list(
            orientation = "h", x = 0, y = -0.15,
            font = list(size = 11)
          ),
          margin = list(t = 30, b = 60)
        )
    })


    # ---- Dynamic header for detailed comparison card ----
    output$diff_header <- renderUI({
      res <- twin_results()
      if (is.null(res) || nrow(res$twins) == 0) {
        return(tags$span("Detailed Comparison"))
      }
      idx <- min(selected_twin_idx(), nrow(res$twins))
      tags$span(
        paste0("Detailed Comparison vs #", idx, ": ", res$twins$SCHNAME[idx])
      )
    })

    # ---- Detailed comparison table: focal vs selected twin ----
    output$diff_table <- renderDT({
      res <- twin_results()
      fd  <- focal_data()
      if (is.null(res) || is.null(fd) || nrow(fd) == 0) return(NULL)
      if (nrow(res$twins) == 0) return(NULL)

      yr  <- input$year_select
      idx <- min(selected_twin_idx(), nrow(res$twins))
      sel_twin_urn <- res$twins$URN[idx]
      twin_row <- panel_data %>%
        filter(URN == sel_twin_urn, year_label == yr) %>%
        slice(1)
      if (nrow(twin_row) == 0) return(NULL)

      oc <- OUTCOME_CONFIG[[selected_outcome()]]

      # ALL context vars (from every group, whether selected or not)
      all_context_vars <- unlist(TWIN_VAR_GROUPS, use.names = FALSE)
      all_context_vars <- intersect(all_context_vars, names(panel_data))

      # Which were actually used for matching?
      used_for_matching <- res$match_vars

      # Outcome vars (always shown, always highlighted)
      outcome_vars <- intersect(
        c(oc$var, "P8MEA"),
        names(panel_data)
      )

      # Context-only vars: in all groups but NOT used for matching
      context_only_vars <- setdiff(all_context_vars, used_for_matching)

      # Assemble in order: matched vars → context-only vars → outcomes
      all_vars <- c(used_for_matching, context_only_vars, outcome_vars)

      # Build comparison rows
      rows <- lapply(all_vars, function(v) {
        f_val <- fd[[v]]
        t_val <- twin_row[[v]]
        is_num <- is.numeric(f_val)
        is_out <- v %in% outcome_vars
        is_ctx <- v %in% context_only_vars

        f_str <- twin_fmt(f_val, if (v == "P8MEA") 2 else 1)
        t_str <- twin_fmt(t_val, if (v == "P8MEA") 2 else 1)

        if (is_num && !is.na(f_val) && !is.na(t_val)) {
          d <- t_val - f_val
          diff_str <- paste0(ifelse(d >= 0, "+", ""), round(d, 2))
        } else if (!is_num) {
          same <- !is.na(f_val) && !is.na(t_val) && as.character(f_val) == as.character(t_val)
          diff_str <- if (same) "\u2714" else "\u2718"
        } else {
          diff_str <- "\u2014"
        }

        # Tag: "match", "context", or "outcome"
        row_type <- if (is_out) "outcome" else if (is_ctx) "context" else "match"

        tibble(
          Variable  = twin_label(v),
          `Your School` = f_str,
          `Selected Twin` = t_str,
          Gap       = diff_str,
          row_type  = row_type
        )
      })

      comparison <- bind_rows(rows)

      # Collect labels for conditional row highlighting
      outcome_labels <- comparison$Variable[comparison$row_type == "outcome"]
      context_labels <- comparison$Variable[comparison$row_type == "context"]

      dt <- datatable(
        comparison %>% select(-row_type),
        options = list(
          dom = "t",
          paging = FALSE,
          ordering = FALSE,
          columnDefs = list(
            list(className = "dt-center", targets = c(1, 2, 3))
          )
        ),
        rownames = FALSE,
        class = "compact stripe hover",
        escape = FALSE
      )

      # Row colours: yellow = outcomes, light blue = context-only (not matched)
      all_styled_labels <- c(outcome_labels, context_labels)
      all_styled_colors <- c(
        rep("#fff3cd", length(outcome_labels)),   # yellow for outcomes
        rep("#d6eaf8", length(context_labels))     # light blue for unmatched context
      )

      if (length(all_styled_labels) > 0) {
        dt <- dt %>%
          formatStyle(
            "Variable",
            target = "row",
            backgroundColor = styleEqual(
              all_styled_labels,
              all_styled_colors
            )
          )
      }

      dt
    })

  })
}
