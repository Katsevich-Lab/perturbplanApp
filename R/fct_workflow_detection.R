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
      minimizing_parameter = "cells"
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
        minimizing_parameter = target
      ))
    } else if (target == "cost") {
      return(list(
        workflow_id = "power_cost_minimization",
        plot_type = "cost_tradeoff_curves",
        minimizing_parameter = "cost"
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
          minimizing_parameter = "TPM_threshold"
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
          minimizing_parameter = "TPM_threshold"
        ))
      }
    } else if (target %in% c("minimum_fold_change", "fold_change")) {
      # Similar logic for fold change workflows 9-11
      if (length(varying_params) == 2 && all(c("cells_per_target", "reads_per_cell") %in% varying_params)) {
        return(list(
          workflow_id = "power_cost_fc_cells_reads",
          plot_type = "cost_tradeoff_curves",
          minimizing_parameter = "minimum_fold_change"
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
          minimizing_parameter = "minimum_fold_change"
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
    minimizing_parameter = safe_minimizing_param  # Context-aware safe default
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
