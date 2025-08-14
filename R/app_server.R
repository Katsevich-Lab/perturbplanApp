#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Sidebar Module - Contains all parameter inputs using modular components
  combined_config <- mod_sidebar_server("sidebar")
  
  # Debug: Print combined config when it changes (for development)
  observe({
    config <- combined_config()
    if (!is.null(config) && length(config) > 0) {
      cat("=== PerturbPlan Modular Configuration Updated ===\n")
      
      # Design Options
      if (!is.null(config$design_options)) {
        design <- config$design_options
        if (!is.null(design$optimization_type)) {
          cat("Optimization Type:", design$optimization_type, "\n")
        }
        if (!is.null(design$minimization_target)) {
          cat("Minimization Target:", design$minimization_target, "\n")
        }
        
        # Parameter Controls
        if (!is.null(design$parameter_controls)) {
          cat("Parameter Controls:\n")
          for (param_name in names(design$parameter_controls)) {
            param_info <- design$parameter_controls[[param_name]]
            cat("  ", param_name, ":", param_info$type)
            if (!is.null(param_info$fixed_value)) {
              cat(" (fixed =", param_info$fixed_value, ")")
            }
            cat("\n")
          }
        }
      }
      
      # Experimental Setup
      if (!is.null(config$experimental_setup)) {
        exp <- config$experimental_setup
        if (!is.null(exp$biological_system)) {
          cat("Biological System:", exp$biological_system, "\n")
        }
        if (!is.null(exp$pilot_data_choice)) {
          cat("Pilot Data Choice:", exp$pilot_data_choice, "\n")
        }
      }
      
      # Analysis Choices
      if (!is.null(config$analysis_choices)) {
        analysis <- config$analysis_choices
        if (!is.null(analysis$side)) {
          cat("Test Side:", analysis$side, "\n")
        }
        if (!is.null(analysis$fdr_target)) {
          cat("FDR Target:", analysis$fdr_target, "\n")
        }
      }
      
      # Effect Sizes
      if (!is.null(config$effect_sizes)) {
        effects <- config$effect_sizes
        if (!is.null(effects$fc_sd)) {
          cat("gRNA Variability:", effects$fc_sd, "\n")
        }
      }
      
      # Plan button status
      if (!is.null(config$plan_clicked) && config$plan_clicked > 0) {
        cat("Plan Button Clicked:", config$plan_clicked, "times\n")
      }
      
      cat("--- End Modular Configuration ---\n")
    }
  })
  
  # Future modules for implementation:
  # analysis_results_module <- mod_analysis_results_server("analysis_results", combined_config)
  # export_results_module <- mod_export_results_server("export_results", combined_config)
}
