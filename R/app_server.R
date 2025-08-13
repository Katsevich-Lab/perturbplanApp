#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Sidebar Module - Contains all parameter inputs including constraint-driven design options
  design_config <- mod_sidebar_server("sidebar")
  
  # Debug: Print design config when it changes (for development)
  observe({
    if (!is.null(design_config()) && length(design_config()) > 0) {
      cat("=== PerturbPlan v2 Configuration Updated ===\n")
      
      # Design Options
      if (!is.null(design_config()$optimization_type)) {
        cat("Optimization Type:", design_config()$optimization_type, "\n")
      }
      if (!is.null(design_config()$minimization_target)) {
        cat("Minimization Target:", design_config()$minimization_target, "\n")
      }
      
      # Parameter Controls
      if (!is.null(design_config()$parameter_controls)) {
        cat("Parameter Controls:\n")
        for (param_name in names(design_config()$parameter_controls)) {
          param_info <- design_config()$parameter_controls[[param_name]]
          cat("  ", param_name, ":", param_info$type)
          if (!is.null(param_info$fixed_value)) {
            cat(" (fixed =", param_info$fixed_value, ")")
          }
          cat("\n")
        }
      }
      
      # Other parameters
      if (!is.null(design_config()$biological_system)) {
        cat("Biological System:", design_config()$biological_system, "\n")
      }
      if (!is.null(design_config()$side)) {
        cat("Test Side:", design_config()$side, "\n")
      }
      if (!is.null(design_config()$fdr_target)) {
        cat("FDR Target:", design_config()$fdr_target, "\n")
      }
      
      cat("--- End Configuration ---\n")
    }
  })
  
  # Placeholder for Plan button logic
  # When the Plan button is clicked, this will trigger the analysis
  # For now, we'll just show that the configuration is complete
  
  # Future modules for implementation:
  # analysis_results_module <- mod_analysis_results_server("analysis_results", design_config)
  # export_results_module <- mod_export_results_server("export_results", design_config)
}
