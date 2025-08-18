#' Detect Workflow Type from Design Configuration
#'
#' @description Analyzes design configuration to determine which of the 11 
#' supported workflows the user has configured.
#'
#' @param design_config Reactive list containing design configuration
#'
#' @return List with workflow_id (1-11), workflow_name, and workflow_type
#'
#' @noRd
detect_workflow <- function(design_config) {
  optimization_type <- design_config$optimization_type
  minimization_target <- design_config$minimization_target
  
  if (is.null(optimization_type) || is.null(minimization_target)) {
    return(list(
      workflow_id = NA,
      workflow_name = "Incomplete configuration",
      workflow_type = "invalid"
    ))
  }
  
  # Power-only optimization workflows (1-5)
  if (optimization_type == "power_only") {
    workflow_map <- list(
      "cells" = list(id = 1, name = "Cells per target", type = "power_only"),
      "reads" = list(id = 2, name = "Reads per cell", type = "power_only"),
      "tpm_threshold" = list(id = 3, name = "TPM threshold", type = "power_only"),
      "fold_change" = list(id = 4, name = "Fold change threshold", type = "power_only"),
      "cost" = list(id = 5, name = "Total cost", type = "power_only")
    )
  }
  
  # Power + cost optimization workflows (6-11)  
  else if (optimization_type == "power_cost") {
    workflow_map <- list(
      "tpm_threshold" = list(id = 6, name = "TPM threshold (with cost)", type = "power_cost"),
      "fold_change" = list(id = 7, name = "Fold change threshold (with cost)", type = "power_cost")
      # Note: workflows 8-11 depend on parameter control combinations
    )
  }
  
  workflow <- workflow_map[[minimization_target]]
  if (is.null(workflow)) {
    return(list(
      workflow_id = NA,
      workflow_name = "Unsupported configuration",
      workflow_type = "invalid"
    ))
  }
  
  return(workflow)
}

#' Validate Design Configuration Completeness
#'
#' @description Checks if design configuration has all required elements
#'
#' @param design_config Design configuration list
#'
#' @return List with is_valid (logical) and errors (character vector)
#'
#' @noRd
validate_design_config <- function(design_config) {
  errors <- character()
  
  # Check required fields
  if (is.null(design_config$optimization_type)) {
    errors <- c(errors, "Optimization type not selected")
  }
  
  if (is.null(design_config$minimization_target)) {
    errors <- c(errors, "Minimization target not selected")
  }
  
  # Check business rules
  if (!is.null(design_config$optimization_type) && 
      design_config$optimization_type == "power_cost" &&
      !is.null(design_config$minimization_target) &&
      design_config$minimization_target == "cost") {
    errors <- c(errors, "Cost minimization not available with Power + cost optimization")
  }
  
  # Check parameter controls
  if (!is.null(design_config$parameter_controls)) {
    minimizing_count <- sum(sapply(design_config$parameter_controls, function(x) x$type == "minimizing"), na.rm = TRUE)
    if (minimizing_count > 1) {
      errors <- c(errors, "Only one parameter can be set to 'Minimizing'")
    }
  }
  
  return(list(
    is_valid = length(errors) == 0,
    errors = errors
  ))
}
