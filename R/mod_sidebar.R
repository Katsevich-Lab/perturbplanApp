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
    
    # Initialize module servers without parameter manager (completely independent from sliders)
    design_config <- mod_design_options_server("design_options")
    experimental_config <- mod_experimental_setup_server("experimental_setup", design_config)
    analysis_config <- mod_analysis_choices_server("analysis_choices", design_config)
    advanced_config <- mod_advanced_choices_server("advanced_choices")
    
    # Initialize effect sizes server without parameter manager (completely independent from sliders)
    effect_sizes_config <- mod_effect_sizes_server("effect_sizes", design_config)
    
    # Track optimization mode changes to reset plan button
    previous_mode <- reactiveVal(NULL)
    plan_count_adjustment <- reactiveVal(0)  # Adjustment to subtract from plan button count
    
    observe({
      current_mode <- design_config()$optimization_type
      if (!is.null(current_mode) && current_mode != "" && 
          !is.null(previous_mode()) && previous_mode() != current_mode) {
        # Mode changed - reset plan by adjusting the count
        plan_count_adjustment(input$plan_btn)  # Store current count to subtract
      }
      if (!is.null(current_mode)) {
        previous_mode(current_mode)
      }
    })
    
    # Plan button logic
    observeEvent(input$plan_btn, {
      # TODO: Trigger analysis with combined configuration
      showNotification("Plan button clicked - analysis will be implemented", type = "message")
    })
    
    # Return configuration from parameter manager (now safe with isolate() patterns)
    combined_config <- reactive({
      # Get design and non-parameter configurations from sidebar modules
      design_opts <- design_config()
      
      # Get parameter values from parameter manager (single source of truth)
      config <- list(
        # Design options with complete configuration from design module
        design_options = design_opts,
        
        # Experimental setup from sidebar modules (independent of sliders)
        experimental_setup = experimental_config(),
        
        # Analysis choices from sidebar modules (independent of sliders)
        analysis_choices = analysis_config(),
        
        # Effect sizes from sidebar modules (independent of sliders)
        effect_sizes = effect_sizes_config(),
        
        # Sidebar-only configuration (not parameters)
        advanced_choices = advanced_config(),
        plan_clicked = input$plan_btn - plan_count_adjustment(),  # Reset plan count on mode change
        timestamp = Sys.time()
      )
      
      # Note: Parameter controls now use their own fixed values (independent of sliders)
      # No sync with parameter manager to maintain sidebar/slider independence
      
      return(config)
    })
    
    return(combined_config)
  })
}
    
## To be copied in the UI
# mod_sidebar_ui("sidebar_1")
    
## To be copied in the server
# mod_sidebar_server("sidebar_1")
