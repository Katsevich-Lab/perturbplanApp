#' effect_sizes UI Function
#'
#' @description Creates the Effect Sizes section for assumed effect size parameters
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList tags div strong numericInput moduleServer reactive observe updateNumericInput
#' @importFrom shinyjs show hide
mod_effect_sizes_ui <- function(id) {
  ns <- NS(id)
  
  # Effect sizes section (collapsible)
  tagList(
    tags$div(
      style = "border-radius: 4px; margin-bottom: 5px;",
      tags$div(
        id = ns("effects-header"),
        style = "padding: 10px 15px; cursor: pointer; border-radius: 4px 4px 0 0;",
        onclick = paste0("toggleSection('", ns("effects-content"), "', '", ns("effects-chevron"), "')"),
        tags$i(id = ns("effects-chevron"), class = "fa fa-chevron-right", style = "margin-right: 8px;"),
        tags$strong("Effect sizes")
      ),
      tags$div(
        id = ns("effects-content"),
        style = "padding: 15px; display: none;",
        
        # Fixed value input for fold change (conditional)
        tags$div(
          id = ns("minimum_fold_change_fixed_div"),
          style = "display: none; margin-bottom: 15px;",
          numericInput(ns("minimum_fold_change_fixed"), "Fold change:",
                      value = 0.8, min = 0.3, max = 2, step = 0.1)
        ),

        # Proportion of non-null pairs
        numericInput(ns("prop_non_null"), "Proportion of non-null pairs:", 0.1, 0, 1, 0.01)
      )
    )
  )
}
    
#' effect_sizes Server Functions
#'
#' @description Server logic for effect size parameters including conditional
#' fold change input display and input state management for dual-phase workflow
#'
#' @param design_config Reactive containing design options configuration
#' @param app_state Reactive containing application phase state for input freezing
#'
#' @noRd
#' @importFrom shinyjs toggleState
mod_effect_sizes_server <- function(id, design_config, app_state = NULL){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    # ============================================================================
    # CONDITIONAL DISPLAY LOGIC
    # ============================================================================
    observe({
      config <- design_config()

      if (!is.null(config) && !is.null(config$parameter_controls)) {
        fc_type <- config$parameter_controls$minimum_fold_change$type

        # Show FC fixed input only when FC parameter is set to "fixed"
        if (!is.null(fc_type) && fc_type == "fixed") {
          shinyjs::show("minimum_fold_change_fixed_div")
        } else {
          shinyjs::hide("minimum_fold_change_fixed_div")
        }
      } else {
        shinyjs::hide("minimum_fold_change_fixed_div")
      }
    })

    # ============================================================================
    # CONFIGURATION OUTPUT
    # ============================================================================
    effect_sizes_config <- reactive({
      list(
        # Fixed value input (only default if actually hidden)
        minimum_fold_change_fixed = input$minimum_fold_change_fixed,
        # Proportion of non-null pairs
        prop_non_null = input$prop_non_null,
        timestamp = Sys.time()
      )
    })

    # ============================================================================
    # INPUT STATE MANAGEMENT
    # ============================================================================
    observeEvent(app_state$phase, {
      if (!is.null(app_state)) {
        inputs_disabled <- (app_state$phase == 2)

        # Core effect size inputs that should be disabled in Phase 2
        shinyjs::toggleState("minimum_fold_change_fixed", condition = !inputs_disabled)
        shinyjs::toggleState("prop_non_null", condition = !inputs_disabled)

        # Note: Section headers remain functional for collapse/expand
      }
    }, ignoreInit = TRUE, ignoreNULL = TRUE)

    return(effect_sizes_config)
  })
}
    
