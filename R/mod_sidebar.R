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
      
      # Effect sizes
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
    experimental_config <- mod_experimental_setup_server("experimental_setup", design_config)
    analysis_config <- mod_analysis_choices_server("analysis_choices", design_config)
    effect_sizes_config <- mod_effect_sizes_server("effect_sizes", design_config)
    
    # Plan button logic
    observeEvent(input$plan_btn, {
      # TODO: Trigger analysis with combined configuration
      showNotification("Plan button clicked - analysis will be implemented", type = "message")
    })
    
    # Return combined configuration from all modules
    combined_config <- reactive({
      # Get configurations from all modules
      design_opts <- design_config()
      experimental_opts <- experimental_config()
      analysis_opts <- analysis_config()
      effect_sizes_opts <- effect_sizes_config()
      
      # Merge fixed values from logical sidebar sections into design options
      if (!is.null(design_opts) && !is.null(design_opts$parameter_controls)) {
        # Update fixed values from their logical locations
        if (!is.null(experimental_opts$cells_fixed)) {
          design_opts$parameter_controls$cells_per_target$fixed_value <- experimental_opts$cells_fixed
        }
        if (!is.null(experimental_opts$reads_fixed)) {
          design_opts$parameter_controls$reads_per_cell$fixed_value <- experimental_opts$reads_fixed
        }
        if (!is.null(analysis_opts$tpm_fixed)) {
          design_opts$parameter_controls$tpm_threshold$fixed_value <- analysis_opts$tpm_fixed
        }
        if (!is.null(effect_sizes_opts$fc_fixed)) {
          design_opts$parameter_controls$min_fold_change$fixed_value <- effect_sizes_opts$fc_fixed
        }
      }
      
      list(
        design_options = design_opts,
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
