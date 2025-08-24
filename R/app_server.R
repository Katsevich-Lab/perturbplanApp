#' The application server-side
#'
#' Clean 3-module architecture: Input → Analysis → Plotting → Display
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @importFrom shiny bindCache
#' @importFrom magrittr %>%
#' @noRd
app_server <- function(input, output, session) {
  
  # ========================================================================
  # MODULE 1: INPUT COLLECTION
  # ========================================================================
  # Collect all user inputs through sidebar (unchanged)
  user_workflow_config <- mod_sidebar_server("sidebar")
  
  # ========================================================================  
  # MODULE 2: ANALYSIS ENGINE (Perturbplan Integration)
  # ========================================================================
  # Generate real analysis results using perturbplan package functions
  analysis_results_raw <- mod_analysis_engine_server("analysis", user_workflow_config)
  
  # ========================================================================
  # MODULE 3: PLOTTING ENGINE (Always Same)
  # ========================================================================
  # Convert analysis data into plot objects
  plot_objects <- mod_plotting_engine_server("plotting", analysis_results_raw)
  
  # ========================================================================
  # MODULE 4: RESULTS DISPLAY (Always Same)  
  # ========================================================================
  # Handle UI presentation of plots and tables
  mod_results_display_server("display", plot_objects, analysis_results_raw)
  
  # ========================================================================
  # APP STATE MANAGEMENT
  # ========================================================================
  
  # Combined observer for loading states and error handling
  observe({
    config <- user_workflow_config()
    analysis <- analysis_results_raw()
    
    # Handle loading states
    if (!is.null(config) && config$plan_clicked > 0 && is.null(analysis)) {
      # Show loading notification when Plan clicked but analysis not ready
      showNotification(
        "Analyzing your experimental design...", 
        type = "message", 
        duration = 2
      )
    }
    
    # Handle analysis errors  
    if (!is.null(analysis) && !is.null(analysis$error)) {
      showNotification(
        paste("Analysis Error:", analysis$error), 
        type = "error",
        duration = 5
      )
    }
  })
  
  # Development debug output disabled to reduce console output
}
