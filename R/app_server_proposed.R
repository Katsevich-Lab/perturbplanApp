#' The application server-side
#'
#' Clean 3-module architecture: Input → Analysis → Plotting → Display
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  
  # ========================================================================
  # MODULE 1: INPUT COLLECTION
  # ========================================================================
  # Collect all user inputs through sidebar (unchanged)
  user_workflow_config <- mod_sidebar_server("sidebar")
  
  # ========================================================================  
  # MODULE 2: ANALYSIS ENGINE (Placeholder vs Real Swap Point)
  # ========================================================================
  # Generate analysis results data (this is where placeholder/real happens)
  analysis_results <- mod_analysis_engine_server("analysis", user_workflow_config)
  
  # ========================================================================
  # MODULE 3: PLOTTING ENGINE (Always Same)
  # ========================================================================
  # Convert analysis data into plot objects
  plot_objects <- mod_plotting_engine_server("plotting", analysis_results)
  
  # ========================================================================
  # MODULE 4: RESULTS DISPLAY (Always Same)  
  # ========================================================================
  # Handle UI presentation of plots and tables
  mod_results_display_server("display", plot_objects, analysis_results)
  
  # ========================================================================
  # APP STATE MANAGEMENT
  # ========================================================================
  
  # Handle loading states
  observe({
    config <- user_workflow_config()
    analysis <- analysis_results()
    
    if (!is.null(config) && config$plan_clicked > 0 && is.null(analysis)) {
      # Show loading notification when Plan clicked but analysis not ready
      showNotification(
        "Analyzing your experimental design...", 
        type = "message", 
        duration = 2
      )
    }
  })
  
  # Handle analysis errors
  observe({
    analysis <- analysis_results()
    if (!is.null(analysis) && !is.null(analysis$error)) {
      showNotification(
        paste("Analysis Error:", analysis$error), 
        type = "error",
        duration = 5
      )
    }
  })
  
  # Development debug output (optional - can remove in production)
  if (getOption("perturbplan.debug", FALSE)) {
    observe({
      config <- user_workflow_config()
      if (!is.null(config) && config$plan_clicked > 0) {
        cat("=== Plan Executed ===\n")
        cat("Workflow Type:", config$design_options$workflow_type, "\n")
        cat("Target Power:", config$design_options$target_power, "\n")
        cat("Analysis Mode:", ifelse(use_placeholder_mode(), "Placeholder", "Real"), "\n")
        cat("Timestamp:", as.character(Sys.time()), "\n")
        cat("--- End Debug ---\n")
      }
    })
  }
}