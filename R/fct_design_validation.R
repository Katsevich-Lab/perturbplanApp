#' Validate Design Configuration
#'
#' @description Validates that all required fields are present and properly configured
#' for experimental design analysis.
#'
#' @param design_config Design configuration from sidebar modules
#' @return List with `valid` (logical) and `message` (character) fields
#' @noRd
validate_design_configuration <- function(design_config) {

  # Check optimization type
  if (is.null(design_config$optimization_type) || design_config$optimization_type == "") {
    return(list(
      valid = FALSE,
      message = "Please select an optimization type (Power-only or Power + Cost)"
    ))
  }

  # Check minimization target
  if (is.null(design_config$minimization_target) || design_config$minimization_target == "") {
    return(list(
      valid = FALSE,
      message = "Please select what parameter to minimize"
    ))
  }

  # Check parameter controls exist
  if (is.null(design_config$parameter_controls)) {
    return(list(
      valid = FALSE,
      message = "Parameter controls not configured. Please set parameters to Varying, Fixed, or Minimizing."
    ))
  }

  # Check that parameter controls have valid states
  param_controls <- design_config$parameter_controls
  required_params <- c("cells_per_target", "reads_per_cell", "TPM_threshold", "minimum_fold_change")

  for (param in required_params) {
    if (!param %in% names(param_controls)) {
      return(list(
        valid = FALSE,
        message = paste("Missing configuration for", param, "parameter")
      ))
    }

    param_state <- param_controls[[param]]
    if (is.null(param_state$type) || !param_state$type %in% c("varying", "fixed", "minimizing")) {
      param_display <- switch(param,
        "cells_per_target" = "Cells per Target",
        "reads_per_cell" = "Reads per Cell",
        "TPM_threshold" = "TPM Threshold",
        "minimum_fold_change" = "Fold Change"
      )
      return(list(
        valid = FALSE,
        message = paste("Please set", param_display, "to Varying, Fixed, or Minimizing")
      ))
    }
  }

  # Check that exactly one parameter is set to minimizing
  minimizing_count <- sum(sapply(param_controls, function(x) x$type == "minimizing"))
  if (minimizing_count == 0) {
    return(list(
      valid = FALSE,
      message = "Please select one parameter to minimize"
    ))
  }

  if (minimizing_count > 1) {
    return(list(
      valid = FALSE,
      message = "Only one parameter can be set to 'Minimizing' at a time"
    ))
  }

  # All validation passed
  return(list(
    valid = TRUE,
    message = "Configuration is valid"
  ))
}