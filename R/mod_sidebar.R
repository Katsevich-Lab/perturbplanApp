#' sidebar UI Function
#'
#' @description Creates the left sidebar using modular components
#' for constraint-driven parameter inputs
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList tags div actionButton observeEvent showNotification
#' @importFrom shinydashboard dashboardSidebar
mod_sidebar_ui <- function(id) {
  ns <- NS(id)
  
  dashboardSidebar(
    # Parameter panels - make scrollable with collapsible sections
    tags$div(
      style = "padding: 10px; max-height: 90vh; overflow-y: auto;",
      
      # Design Options Module
      mod_design_options_ui(ns("design_options")),
      
      # Cost Information Module (conditional)
      mod_cost_info_ui(ns("cost_info")),
      
      # Experimental Setup Module  
      mod_experimental_setup_ui(ns("experimental_setup")),
      
      # Analysis Choices Module
      mod_analysis_choices_ui(ns("analysis_choices")),
      
      # Effect Sizes Module
      mod_effect_sizes_ui(ns("effect_sizes")),
      
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
#'
#' @noRd 
mod_sidebar_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    # Initialize module servers
    design_config <- mod_design_options_server("design_options")
    cost_config <- mod_cost_info_server("cost_info", design_config)
    experimental_config <- mod_experimental_setup_server("experimental_setup")
    analysis_config <- mod_analysis_choices_server("analysis_choices")
    effect_sizes_config <- mod_effect_sizes_server("effect_sizes")
    
    # Plan button logic
    observeEvent(input$plan_btn, {
      # TODO: Trigger analysis with combined configuration
      showNotification("Plan button clicked - analysis will be implemented", type = "message")
    })
    
    # Return combined configuration from all modules
    combined_config <- reactive({
      list(
        design_options = design_config(),
        cost_info = cost_config(),
        experimental_setup = experimental_config(),
        analysis_choices = analysis_config(),
        effect_sizes = effect_sizes_config(),
        plan_clicked = input$plan_btn,
        timestamp = Sys.time()
      )
    })
    
    return(combined_config)
  })
}
    
## To be copied in the UI
# mod_sidebar_ui("sidebar_1")
    
## To be copied in the server
# mod_sidebar_server("sidebar_1")
