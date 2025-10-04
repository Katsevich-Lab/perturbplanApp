#' Validate Design Configuration
#'
#' @description Validates that all required fields are present and properly configured
#' for experimental design analysis.
#'
#' @param design_config Design configuration from sidebar modules
#' @return List with `valid` (logical) and `message` (character) fields
#' @noRd
validate_design_configuration <- function(design_config) {

  # Check assay type
  if (is.null(design_config$assay_type) || design_config$assay_type == "") {
    return(list(
      valid = FALSE,
      message = "Please select an assay type (Perturb-seq or TAP-seq)"
    ))
  }

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

  # All validation passed
  return(list(
    valid = TRUE,
    message = "Configuration is valid"
  ))
}
