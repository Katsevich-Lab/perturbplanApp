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
      "TPM_threshold" = list(id = 3, name = "TPM threshold", type = "power_only"),
      "fold_change" = list(id = 4, name = "Fold change threshold", type = "power_only"),
      "cost" = list(id = 5, name = "Total cost", type = "power_only")
    )
  }
  
  # Power + cost optimization workflows (6-11)  
  else if (optimization_type == "power_cost") {
    workflow_map <- list(
      "TPM_threshold" = list(id = "power_cost_TPM_cells_reads", name = "TPM threshold (with cost)", type = "power_cost", minimizing_parameter = "TPM_threshold"),
      "fold_change" = list(id = "power_cost_fc_cells_reads", name = "Fold change threshold (with cost)", type = "power_cost", minimizing_parameter = "minimum_fold_change")
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
  
  # Convert workflow structure to expected format
  return(list(
    workflow_id = workflow$id,
    workflow_name = workflow$name,
    workflow_type = workflow$type,
    minimizing_parameter = workflow$minimizing_parameter
  ))
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

#' Detect workflow scenario from user configuration
#'
#' @param workflow_config List containing user's complete configuration
#' @return List with workflow type and metadata
#' @noRd
detect_workflow_scenario <- function(workflow_config) {
  design_config <- workflow_config$design_options

  opt_type <- design_config$optimization_type
  target <- design_config$minimization_target
  param_controls <- design_config$parameter_controls
  cost_budget <- design_config$cost_budget


  # VALIDATION: Check for missing essential values
  if (is.null(opt_type) || opt_type == "" || is.null(target) || target == "") {
    return(list(
      workflow_id = "incomplete_configuration",
      plot_type = "single_parameter_curve",
      category = "unknown",
      minimizing_parameter = "cells",
      title = "Incomplete Configuration",
      description = "Missing optimization type or minimization target"
    ))
  }

  # FALLBACK: If optimization_type is missing but cost_budget is present, assume power_cost
  if ((is.null(opt_type) || opt_type == "") && !is.null(cost_budget) && cost_budget > 0) {
    opt_type <- "power_cost"
  }

  # Power-only workflows (1-5)
  if (opt_type == "power_only") {
    # UI now sends perturbplan names directly - no translation needed
    if (target %in% c("cells_per_target", "reads_per_cell", "TPM_threshold", "minimum_fold_change")) {
      return(list(
        workflow_id = paste0("power_single_", target),
        plot_type = "single_parameter_curve",
        category = "power_only_single",
        minimizing_parameter = target,
        title = paste("Minimize", format_parameter_name(target)),
        description = paste("Power-only optimization minimizing", format_parameter_name(target))
      ))
    } else if (target == "cost") {
      return(list(
        workflow_id = "power_cost_minimization",
        plot_type = "cost_tradeoff_curves",
        category = "power_only_cost",
        minimizing_parameter = "cost",
        title = "Power Optimization: Minimize Total Cost",
        description = "Power-only optimization with cost minimization"
      ))
    }
  }

  # Power+cost workflows (6-11)
  if (opt_type == "power_cost") {
    # Add null check for param_controls
    if (is.null(param_controls)) {
      param_controls <- list()
    }

    varying_params <- get_varying_parameters(param_controls)

    if (target == "TPM_threshold") {
      if (length(varying_params) == 2 && all(c("cells_per_target", "reads_per_cell") %in% varying_params)) {
        # Workflow 8: TPM + cells + reads varying
        return(list(
          workflow_id = "power_cost_TPM_cells_reads",
          plot_type = "cost_tradeoff_curves",
          category = "power_cost_multi",
          minimizing_parameter = "TPM_threshold",
          title = "Cost + Power Optimization: Minimize TPM Threshold",
          description = "optimization over TPM, with cells and reads varying"
        ))
      } else if (length(varying_params) == 1) {
        # Workflows 6-7: TPM + one other parameter
        other_param_ui <- varying_params[1]
        # Map UI control name to internal name
        other_param <- switch(other_param_ui,
          "cells_per_target" = "cells",
          "reads_per_cell" = "reads",
          other_param_ui  # fallback
        )
        return(list(
          workflow_id = paste0("power_cost_TPM_", other_param),
          plot_type = "single_parameter_curve",
          category = "power_cost_single",
          minimizing_parameter = "TPM_threshold",
          varying_parameter = other_param,  # Track which cells/reads parameter is varying
          title = "Minimize TPM Threshold",  # Clean title without varying parameter info
          description = paste("Power+cost optimization with TPM and", format_parameter_name(other_param), "varying")
        ))
      }
    } else if (target %in% c("minimum_fold_change", "fold_change")) {
      # Similar logic for fold change workflows 9-11
      if (length(varying_params) == 2 && all(c("cells_per_target", "reads_per_cell") %in% varying_params)) {
        return(list(
          workflow_id = "power_cost_fc_cells_reads",
          plot_type = "cost_tradeoff_curves",
          category = "power_cost_multi",
          minimizing_parameter = "minimum_fold_change",
          title = "Cost + Power Optimization: Minimize Fold Change",
          description = "optimization over fold change, with cells and reads varying"
        ))
      } else if (length(varying_params) == 1) {
        other_param_ui <- varying_params[1]
        # Map UI control name to internal name
        other_param <- switch(other_param_ui,
          "cells_per_target" = "cells",
          "reads_per_cell" = "reads",
          other_param_ui  # fallback
        )
        return(list(
          workflow_id = paste0("power_cost_fc_", other_param),
          plot_type = "single_parameter_curve",
          category = "power_cost_single",
          minimizing_parameter = "minimum_fold_change",
          varying_parameter = other_param,  # Track which cells/reads parameter is varying
          title = "Minimize Fold Change",  # Clean title without varying parameter info
          description = paste("Power+cost optimization with fold change and", format_parameter_name(other_param), "varying")
        ))
      }
    }
  }

  # Fallback for unknown configurations
  # Choose a safer default minimizing parameter based on optimization type
  safe_minimizing_param <- if (!is.null(opt_type) && opt_type == "power_cost") {
    "TPM_threshold"  # Safe default for power+cost mode
  } else {
    "cells_per_target"  # Safe default for power_only mode
  }

  return(list(
    workflow_id = "unknown",
    plot_type = "single_parameter_curve",  # Default to safe plot type
    category = "unknown",
    minimizing_parameter = safe_minimizing_param,  # Context-aware safe default
    title = "Unknown Workflow Configuration",
    description = "Unable to detect workflow from configuration"
  ))
}

#' Get parameters that are set to varying
#'
#' @param param_controls List of parameter control configurations
#' @return Character vector of varying parameter names
#' @noRd
get_varying_parameters <- function(param_controls) {
  varying_params <- character()

  for (param_name in names(param_controls)) {
    param_info <- param_controls[[param_name]]
    if (!is.null(param_info$type) && param_info$type == "varying") {
      varying_params <- c(varying_params, param_name)
    }
  }

  return(varying_params)
}

#' Format parameter names for display
#'
#' @param parameter_name Character parameter name from config
#' @return Character formatted name for display
#' @noRd
format_parameter_name <- function(parameter_name) {
  switch(parameter_name,
    # Full parameter names (preferred)
    "cells_per_target" = "Cells per Target",
    "reads_per_cell" = "Sequenced Reads per Cell",
    "TPM_threshold" = "TPM Threshold",
    "minimum_fold_change" = "Fold Change",
    # Legacy abbreviated names (backward compatibility)
    "cells" = "Cells per Target",
    "reads" = "Reads per Cell",
    "fold_change" = "Fold Change",
    "cost" = "Total Cost",
    # Fallback: capitalize first letter
    stringr::str_to_title(gsub("_", " ", parameter_name))
  )
}
