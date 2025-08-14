#' Analysis Configuration and Utilities
#'
#' @description Configuration functions that control analysis behavior
#' and provide utilities for processing workflow configurations.
#'
#' @noRd

# ============================================================================
# PLACEHOLDER VS REAL MODE CONTROL
# ============================================================================

#' Check if app should use placeholder data
#'
#' @description Controls whether analysis uses placeholder calculations
#' or real perturbplan package calls. This is THE master switch.
#'
#' @return Logical indicating if placeholder mode is active
#' @noRd
use_placeholder_mode <- function() {
  # Priority order for configuration:
  # 1. Environment variable (for deployment)
  # 2. Golem config file (for different environments)  
  # 3. Default to TRUE (safe for development)
  
  # Check environment variable first
  env_setting <- Sys.getenv("PERTURBPLAN_USE_PLACEHOLDER", "")
  if (env_setting != "") {
    return(tolower(env_setting) %in% c("true", "1", "yes"))
  }
  
  # Check golem config
  tryCatch({
    config_setting <- config::get("use_placeholder_data", config = golem::get_golem_name())
    if (!is.null(config_setting)) {
      return(isTRUE(config_setting))
    }
  }, error = function(e) {
    # Config not found, continue to default
  })
  
  # Default to placeholder mode (safe for development)
  return(TRUE)
}

#' Get current analysis mode description
#'
#' @return Character string describing current mode
#' @noRd
get_analysis_mode <- function() {
  if (use_placeholder_mode()) {
    return("Placeholder Analysis (Simulated Results)")
  } else {
    return("Real Analysis (perturbplan Package)")
  }
}

# ============================================================================
# WORKFLOW CONFIGURATION PROCESSING
# ============================================================================

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
  
  # Power-only workflows (1-5)
  if (opt_type == "power_only") {
    if (target %in% c("cells", "reads", "tpm_threshold", "fold_change")) {
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
        title = "Minimize Total Cost",
        description = "Power-only optimization with cost minimization"
      ))
    }
  }
  
  # Power+cost workflows (6-11)
  if (opt_type == "power_cost") {
    varying_params <- get_varying_parameters(param_controls)
    
    if (target == "tpm_threshold") {
      if (length(varying_params) == 2 && all(c("cells", "reads") %in% varying_params)) {
        # Workflow 8: TPM + cells + reads varying
        return(list(
          workflow_id = "power_cost_tpm_cells_reads",
          plot_type = "cost_tradeoff_curves",
          category = "power_cost_multi",
          minimizing_parameter = "tpm_threshold",
          title = "Minimize TPM Threshold (Cells + Reads Varying)",
          description = "Power+cost optimization with TPM, cells, and reads varying"
        ))
      } else if (length(varying_params) == 1) {
        # Workflows 6-7: TPM + one other parameter
        other_param <- varying_params[1]
        return(list(
          workflow_id = paste0("power_cost_tmp_", other_param),
          plot_type = "single_parameter_curve",
          category = "power_cost_single", 
          minimizing_parameter = "tpm_threshold",
          title = paste("Minimize TPM Threshold (", format_parameter_name(other_param), "Varying)"),
          description = paste("Power+cost optimization with TPM and", format_parameter_name(other_param), "varying")
        ))
      }
    } else if (target == "fold_change") {
      # Similar logic for fold change workflows 9-11
      if (length(varying_params) == 2 && all(c("cells", "reads") %in% varying_params)) {
        return(list(
          workflow_id = "power_cost_fc_cells_reads",
          plot_type = "cost_tradeoff_curves",
          category = "power_cost_multi",
          minimizing_parameter = "fold_change", 
          title = "Minimize Fold Change (Cells + Reads Varying)",
          description = "Power+cost optimization with fold change, cells, and reads varying"
        ))
      } else if (length(varying_params) == 1) {
        other_param <- varying_params[1]
        return(list(
          workflow_id = paste0("power_cost_fc_", other_param),
          plot_type = "single_parameter_curve",
          category = "power_cost_single",
          minimizing_parameter = "fold_change",
          title = paste("Minimize Fold Change (", format_parameter_name(other_param), "Varying)"),
          description = paste("Power+cost optimization with fold change and", format_parameter_name(other_param), "varying")
        ))
      }
    }
  }
  
  # Fallback for unknown configurations
  return(list(
    workflow_id = "unknown",
    plot_type = "error",
    category = "unknown",
    title = "Unknown Workflow Configuration",
    description = "Unable to detect workflow from configuration"
  ))
}

#' Validate workflow configuration completeness
#'
#' @param workflow_config List containing user configuration
#' @return List with is_valid flag and error messages
#' @noRd
validate_workflow_config <- function(workflow_config) {
  errors <- character()
  
  # Check design options
  design_config <- workflow_config$design_options
  if (is.null(design_config$optimization_type) || design_config$optimization_type == "") {
    errors <- c(errors, "Optimization type not selected")
  }
  
  if (is.null(design_config$target_power) || !is.numeric(design_config$target_power)) {
    errors <- c(errors, "Target power not specified")
  } else if (design_config$target_power <= 0 || design_config$target_power >= 1) {
    errors <- c(errors, "Target power must be between 0 and 1")
  }
  
  if (is.null(design_config$minimization_target) || design_config$minimization_target == "") {
    errors <- c(errors, "Minimization target not selected")
  }
  
  # Check cost budget for power+cost optimization
  if (!is.null(design_config$optimization_type) && design_config$optimization_type == "power_cost") {
    if (is.null(design_config$cost_budget) || !is.numeric(design_config$cost_budget)) {
      errors <- c(errors, "Cost budget required for power+cost optimization")
    } else if (design_config$cost_budget <= 0) {
      errors <- c(errors, "Cost budget must be positive")
    }
  }
  
  # Check parameter controls
  param_controls <- design_config$parameter_controls
  if (is.null(param_controls)) {
    errors <- c(errors, "Parameter controls not configured")
  }
  
  # Check for required experimental setup
  if (is.null(workflow_config$experimental_setup)) {
    errors <- c(errors, "Experimental setup not configured")
  }
  
  return(list(
    is_valid = length(errors) == 0,
    errors = errors,
    validation_timestamp = Sys.time()
  ))
}

# ============================================================================
# PARAMETER EXTRACTION UTILITIES  
# ============================================================================

#' Extract parameter ranges from workflow configuration
#'
#' @param workflow_config List containing user configuration
#' @param parameter_name Character name of parameter
#' @return Numeric vector of parameter range or single value
#' @noRd
extract_parameter_range <- function(workflow_config, parameter_name) {
  param_control <- workflow_config$design_options$parameter_controls[[parameter_name]]
  
  if (is.null(param_control)) {
    # Return default ranges
    return(get_default_parameter_range(parameter_name))
  }
  
  if (param_control$type == "fixed") {
    # Return fixed value
    return(param_control$fixed_value)
  } else if (param_control$type == "varying") {
    # Return reasonable range for varying parameter
    return(get_default_parameter_range(parameter_name))
  } else if (param_control$type %in% c("minimizing", "optimizing")) {
    # Return range that will be optimized over
    return(get_default_parameter_range(parameter_name))
  }
  
  # Fallback
  return(get_default_parameter_range(parameter_name))
}

#' Get default parameter ranges
#'
#' @param parameter_name Character name of parameter
#' @return Numeric vector of default range
#' @noRd
get_default_parameter_range <- function(parameter_name) {
  switch(parameter_name,
    "cells" = seq(100, 2000, 50),
    "reads" = seq(1000, 10000, 200),
    "tpm_threshold" = seq(5, 50, 1),
    "fold_change" = seq(1.2, 3.0, 0.1),
    # Fallback
    c(1, 10, 1)
  )
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

# ============================================================================
# FORMATTING AND DISPLAY UTILITIES
# ============================================================================

#' Format parameter names for display
#'
#' @param parameter_name Character parameter name from config
#' @return Character formatted name for display
#' @noRd
format_parameter_name <- function(parameter_name) {
  switch(parameter_name,
    "cells" = "Cells per Target",
    "reads" = "Reads per Cell", 
    "tpm_threshold" = "TPM Threshold",
    "fold_change" = "Minimum Fold Change",
    "cost" = "Total Cost",
    # Fallback: capitalize first letter
    stringr::str_to_title(gsub("_", " ", parameter_name))
  )
}

#' Create analysis metadata for results
#'
#' @param workflow_config User configuration
#' @param workflow_info Detected workflow information  
#' @return List with analysis metadata
#' @noRd
create_analysis_metadata <- function(workflow_config, workflow_info) {
  list(
    analysis_mode = get_analysis_mode(),
    workflow_type = workflow_info$workflow_id,
    workflow_category = workflow_info$category,
    target_power = workflow_config$design_options$target_power,
    cost_budget = workflow_config$design_options$cost_budget,
    analysis_timestamp = Sys.time(),
    app_version = golem::get_golem_version(),
    user_session_id = session$token  # If available
  )
}