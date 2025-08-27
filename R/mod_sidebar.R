#' sidebar UI Function
#'
#' @description Creates the left sidebar using modular components
#' for constraint-driven parameter inputs
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList tags div actionButton observeEvent showNotification uiOutput renderUI
#' @importFrom shinydashboard dashboardSidebar
#' @importFrom shinyjs show hide
mod_sidebar_ui <- function(id) {
  ns <- NS(id)
  
  dashboardSidebar(
    # Parameter panels - make scrollable with collapsible sections
    tags$div(
      style = "padding: 10px; max-height: 90vh; overflow-y: auto;",
      
      # Design problem
      mod_design_options_ui(ns("design_options")),
      
      # Experimental choices (now includes perturbation choices)
      mod_experimental_setup_ui(ns("experimental_setup")),
      
      # Analysis choices
      mod_analysis_choices_ui(ns("analysis_choices")),
      
      # Effect sizes (always visible - contains non-null proportion)
      mod_effect_sizes_ui(ns("effect_sizes")),
      
      # Advanced settings
      mod_advanced_choices_ui(ns("advanced_choices")),
      
      # Horizontal separator line
      tags$hr(class = "sidebar-separator"),
      
      # Plan button
      tags$div(
        style = "text-align: center; padding: 0 20px;",
        actionButton(ns("plan_btn"), "Plan", class = "btn-success", style = "width: 200px; max-width: 90%;")
      )
    )
  )
}
    
#' sidebar Server Functions
#'
#' @description Server logic for sidebar using modular components
#' with central parameter manager integration
#'
#' @param id Module namespace ID
#' @param param_manager Parameter manager instance (central hub)
#' @param external_updates Reactive containing parameter updates from sliders (DEPRECATED)
#'
#' @noRd 
mod_sidebar_server <- function(id, param_manager){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    # Initialize module servers with parameter manager integration
    design_config <- mod_design_options_server("design_options")
    experimental_config <- mod_experimental_setup_server("experimental_setup", design_config, param_manager)
    analysis_config <- mod_analysis_choices_server("analysis_choices", design_config, param_manager)
    advanced_config <- mod_advanced_choices_server("advanced_choices")
    
    # Initialize effect sizes server with parameter manager integration
    effect_sizes_config <- mod_effect_sizes_server("effect_sizes", design_config, param_manager)
    
    # Plan button logic
    observeEvent(input$plan_btn, {
      # TODO: Trigger analysis with combined configuration
      showNotification("Plan button clicked - analysis will be implemented", type = "message")
    })
    
    # Return configuration from central parameter manager (TEMPORARY: with compatibility layer)
    combined_config <- reactive({
      # Use parameter manager's unified config but add sidebar-specific data
      base_config <- param_manager$combined_config()
      
      # Add sidebar-only configuration that's not parameter-related
      base_config$advanced_choices <- advanced_config()
      base_config$plan_clicked <- input$plan_btn
      base_config$timestamp <- Sys.time()
      
      return(base_config)
    })
    
    return(combined_config)
  })
}
    
## To be copied in the UI
# mod_sidebar_ui("sidebar_1")
    
## To be copied in the server
# mod_sidebar_server("sidebar_1")
