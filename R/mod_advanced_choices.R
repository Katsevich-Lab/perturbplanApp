#' advanced_choices UI Function
#'
#' @description Creates the Advanced Settings section for technical parameters
#' that advanced users may want to modify. Contains gRNA variability and mapping efficiency.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList tags div strong numericInput selectInput moduleServer reactive updateSelectInput updateNumericInput req uiOutput renderUI
#' @importFrom shinyjs show hide disable enable toggleState
mod_advanced_choices_ui <- function(id) {
  ns <- NS(id)

  # Advanced settings (collapsible, initially collapsed)
  tagList(
    tags$div(
      style = "border-radius: 4px; margin-bottom: 5px;",
      tags$div(
        id = ns("advanced-header"),
        style = "padding: 10px 15px; cursor: pointer; border-radius: 4px 4px 0 0;",
        onclick = paste0("toggleSection('", ns("advanced-content"), "', '", ns("advanced-chevron"), "')"),
        tags$i(id = ns("advanced-chevron"), class = "fa fa-chevron-right", style = "margin-right: 8px;"),
        tags$strong("Advanced settings (optional)")
      ),
      tags$div(
        id = ns("advanced-content"),
        style = "padding: 15px;",

        # gRNA variability
        numericInput(ns("gRNA_variability"),
                    add_tooltip("gRNA variability:", "grna_variability", use_icon = TRUE),
                    value = 0.15,
                    min = 0.1,
                    max = 5,
                    step = 0.05),

        # Mapping efficiency
        numericInput(ns("mapping_efficiency"),
                    add_tooltip("Mapping efficiency:", "mapping_efficiency", use_icon = TRUE),
                    value = 0.72,
                    min = 0.1,
                    max = 1.0,
                    step = 0.01),

        # Mapping efficiency message (auto-fill notification)
        uiOutput(ns("mapping_efficiency_message")),

        # Control group
        selectInput(ns("control_group"),
                    add_tooltip("Control group:", "control_group", use_icon = TRUE),
                    choices = c("Complement cells" = "complement",
                               "Non-targeting cells" = "nt_cells"),
                    selected = "complement"),

        # Explanatory text for MOI>1 control group restriction
        tags$div(
          id = ns("moi_explanation"),
          style = "display: none; margin-top: 5px;",
          tags$small("(automatically selected when MOI>1)",
                    style = "color: #888; font-style: italic;")
        ),

        # FDR target level
        numericInput(ns("fdr_target"),
                    add_tooltip("FDR target level:", "fdr_target", use_icon = TRUE),
                    0.1, 0.001, 0.5, 0.001)
      )
    )
  )
}

#' advanced_choices Server Functions
#'
#' @description Server logic for advanced settings parameters including MOI-based
#' control group restrictions and input state management for dual-phase workflow
#'
#' @param id Module namespace ID
#' @param app_state Reactive containing application phase state for input freezing
#' @param experimental_config Reactive containing experimental setup for MOI logic
#'
#' @return Reactive list containing advanced parameter configuration
#'
#' @noRd
mod_advanced_choices_server <- function(id, app_state = NULL, experimental_config = NULL){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    # ============================================================================
    # MOI-BASED CONTROL GROUP RESTRICTIONS
    # ============================================================================
    observeEvent(experimental_config(), {
      if (!is.null(experimental_config()) && !is.null(experimental_config()$MOI)) {
        moi_value <- experimental_config()$MOI

        if (moi_value > 1 && !is.na(moi_value)) {
          # Force to complement cells and disable input
          updateSelectInput(session, "control_group", selected = "complement")
          shinyjs::disable("control_group")
          shinyjs::show("moi_explanation")
        } else {
          # Enable input for user selection
          shinyjs::enable("control_group")
          shinyjs::hide("moi_explanation")
        }
      }
    }, ignoreInit = FALSE)

    # ============================================================================
    # AUTO-FILL MAPPING EFFICIENCY FROM CUSTOM PILOT DATA
    # ============================================================================
    observeEvent(experimental_config(), {
      req(experimental_config)
      pilot_data <- experimental_config()$pilot_data

      # Check if custom data with mapping efficiency is available
      if (!is.null(pilot_data$type) && pilot_data$type == "custom" &&
          !is.null(pilot_data$data) &&
          !is.null(pilot_data$data$mapping_efficiency)) {

        # Custom data: use uploaded mapping efficiency
        mapping_eff <- pilot_data$data$mapping_efficiency
        updateNumericInput(session, "mapping_efficiency", value = mapping_eff)

      } else if (!is.null(pilot_data$type) && pilot_data$type == "default") {
        # Built-in data: reset to default value (0.72)
        updateNumericInput(session, "mapping_efficiency", value = 0.72)
      }
    }, ignoreNULL = TRUE, ignoreInit = TRUE)

    # Render informational message when mapping efficiency is auto-filled
    output$mapping_efficiency_message <- renderUI({
      req(experimental_config)
      pilot_data <- experimental_config()$pilot_data

      # Only show message when custom data with mapping efficiency is loaded
      if (!is.null(pilot_data$type) && pilot_data$type == "custom" &&
          !is.null(pilot_data$data) &&
          !is.null(pilot_data$data$mapping_efficiency)) {

        mapping_eff <- pilot_data$data$mapping_efficiency

        tags$div(
          class = "parameter-info-note",
          style = "margin-top: 5px;",
          tags$i(class = "fa fa-info-circle"),
          sprintf(" Mapping efficiency (%.2f) auto-filled from custom reference data.",
                  mapping_eff)
        )
      } else {
        NULL  # No message for built-in data
      }
    })

    # ============================================================================
    # CONFIGURATION OUTPUT
    # ============================================================================
    advanced_choices_config <- reactive({
      list(
        gRNA_variability = input$gRNA_variability,
        mapping_efficiency = input$mapping_efficiency,
        control_group = input$control_group,
        fdr_target = input$fdr_target,
        timestamp = Sys.time()
      )
    })

    # ============================================================================
    # INPUT STATE MANAGEMENT
    # ============================================================================
    observeEvent(app_state$phase, {
      if (!is.null(app_state)) {
        inputs_disabled <- (app_state$phase == 2)

        # Core advanced inputs that should be disabled in Phase 2
        shinyjs::toggleState("gRNA_variability", condition = !inputs_disabled)
        shinyjs::toggleState("mapping_efficiency", condition = !inputs_disabled)

        shinyjs::toggleState("control_group", condition = !inputs_disabled)

        shinyjs::toggleState("fdr_target", condition = !inputs_disabled)

        # Note: Section headers remain functional for collapse/expand
      }
    }, ignoreInit = TRUE, ignoreNULL = TRUE)

    return(advanced_choices_config)
  })
}

