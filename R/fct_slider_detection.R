#' Detect if sliders should be shown based on design configuration only
#'
#' @description Lightweight workflow detection that doesn't require analysis results.
#' Used to show sliders immediately when user configures design options.
#'
#' @param design_config Design configuration from sidebar
#'
#' @return List with workflow_id and should_show_sliders flag
#' @noRd
detect_slider_workflow <- function(design_config) {
  if (is.null(design_config)) {
    return(list(workflow_id = NULL, should_show_sliders = FALSE))
  }
  
  opt_type <- design_config$optimization_type
  target <- design_config$minimization_target
  
  if (is.null(opt_type) || is.null(target)) {
    return(list(workflow_id = NULL, should_show_sliders = FALSE))
  }
  
  # Translate UI names to backend names (same as analysis engine)
  target_translated <- switch(target,
    "cells" = "cells_per_target",
    "reads" = "reads_per_cell", 
    "fold_change" = "minimum_fold_change",
    target  # No change for TPM_threshold, cost
  )
  
  # Generate workflow ID based on optimization type and target
  if (opt_type == "power_only") {
    if (target_translated %in% c("cells_per_target", "reads_per_cell", "TPM_threshold", "minimum_fold_change")) {
      workflow_id <- paste0("power_single_", target_translated)
    } else if (target == "cost") {
      workflow_id <- "power_cost_minimization"
    } else {
      workflow_id <- NULL
    }
  } else if (opt_type == "power_cost") {
    if (target_translated == "TPM_threshold") {
      workflow_id <- "power_cost_TPM_cells"  # Default to cells varying
    } else if (target_translated == "minimum_fold_change") {
      workflow_id <- "power_cost_fc_cells"   # Default to cells varying  
    } else {
      workflow_id <- NULL
    }
  } else {
    workflow_id <- NULL
  }
  
  # Define workflows that should show sliders
  slider_workflows <- c(
    # Power-only workflows
    "power_single_cells_per_target", 
    "power_single_reads_per_cell", 
    "power_single_TPM_threshold", 
    "power_single_minimum_fold_change",
    
    # Cost minimization workflow
    "power_cost_minimization",
    
    # Power+cost workflows  
    "power_cost_TPM_cells",
    "power_cost_TPM_reads", 
    "power_cost_fc_cells",
    "power_cost_fc_reads"
  )
  
  should_show <- !is.null(workflow_id) && workflow_id %in% slider_workflows
  
  return(list(
    workflow_id = workflow_id,
    should_show_sliders = should_show
  ))
}
