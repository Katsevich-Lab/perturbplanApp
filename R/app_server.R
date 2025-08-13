#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic
  
  # Design Options Module - Core constraint-driven workflow
  design_config <- mod_design_options_server("design_options")
  
  # Debug: Print design config when it changes (for development)
  observe({
    if (!is.null(design_config())) {
      cat("Design Configuration Updated:\n")
      cat("Optimization Type:", design_config()$optimization_type, "\n")
      cat("Minimization Target:", design_config()$minimization_target, "\n")
      cat("Validation Status:", design_config()$validation_status$is_valid, "\n")
      if (length(design_config()$validation_status$errors) > 0) {
        cat("Errors:", paste(design_config()$validation_status$errors, collapse = ", "), "\n")
      }
      cat("---\n")
    }
  })
  
  # Placeholder modules for future implementation:
  # experimental_setup_module <- mod_experimental_setup_server("experimental_setup", design_config)
  # analysis_results_module <- mod_analysis_results_server("analysis_results", design_config) 
  # export_results_module <- mod_export_results_server("export_results", design_config)
}
