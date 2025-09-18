#' advanced_choices UI Function
#'
#' @description Creates the Advanced Settings section for technical parameters
#' that advanced users may want to modify. Contains gRNA variability and mapping efficiency.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList tags div strong numericInput selectInput moduleServer reactive updateSelectInput
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
        tags$strong("Advanced settings")
      ),
      tags$div(
        id = ns("advanced-content"),
        style = "padding: 15px;",
        
        # gRNA variability
        numericInput(ns("gRNA_variability"),
                    "gRNA variability:",
                    value = 0.15,
                    min = 0.1,
                    max = 5,
                    step = 0.05),

        # Mapping efficiency
        numericInput(ns("mapping_efficiency"),
                    "Mapping efficiency:",
                    value = 0.72,
                    min = 0.1,
                    max = 1.0,
                    step = 0.01),

        # Control group
        selectInput(ns("control_group"), "Control group:",
                    choices = c("Complement cells" = "complement",
                               "Non-targeting cells" = "nt_cells"),
                    selected = "complement"),

        # Explanatory text for MOI=1 control group restriction
        tags$div(
          id = ns("moi_explanation"),
          style = "display: none; margin-top: 5px;",
          tags$small("(automatically selected when MOI=1)",
                    style = "color: #888; font-style: italic;")
        ),

        # FDR target level
        numericInput(ns("fdr_target"), "FDR target level:", 0.1, 0.001, 0.5, 0.001)
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

        if (moi_value == 1) {
          # Force to non-targeting cells and disable input
          updateSelectInput(session, "control_group", selected = "nt_cells")
          shinyjs::disable("control_group")
          shinyjs::show("moi_explanation")
        } else {
          # Enable input for user selection
          shinyjs::enable("control_group")
          shinyjs::hide("moi_explanation")
        }
      }
    }, ignoreInit = TRUE)

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
    
