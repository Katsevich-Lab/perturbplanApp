# ============================================================================
# ANALYSIS CONFIGURATION AND UTILITIES
# ============================================================================
# ANALYSIS MODE CONTROL
# ============================================================================

#' Check if app should use real analysis
#'
#' @description Always returns FALSE - app uses real perturbplan integration exclusively.
#'
#' @return Logical - always FALSE (real analysis mode)
#' @noRd
use_placeholder_mode <- function() {
  return(FALSE)
}

#' Get current analysis mode description
#'
#' @return Character string describing current mode
#' @noRd
get_analysis_mode <- function() {
  return("Real Analysis (perturbplan Package)")
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
        title = "Minimize Total Cost",
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
      if (length(varying_params) == 2 && all(c("cells_per_target", "mapped_reads_per_cell") %in% varying_params)) {
        # Workflow 8: TPM + cells + reads varying
        return(list(
          workflow_id = "power_cost_TPM_cells_reads",
          plot_type = "cost_tradeoff_curves",
          category = "power_cost_multi",
          minimizing_parameter = "TPM_threshold",
          title = "Minimize TPM Threshold",
          description = "Power+cost optimization with TPM, cells, and reads varying"
        ))
      } else if (length(varying_params) == 1) {
        # Workflows 6-7: TPM + one other parameter
        other_param_ui <- varying_params[1]
        # Map UI control name to internal name
        other_param <- switch(other_param_ui,
          "cells_per_target" = "cells",
          "mapped_reads_per_cell" = "reads",
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
      if (length(varying_params) == 2 && all(c("cells_per_target", "mapped_reads_per_cell") %in% varying_params)) {
        return(list(
          workflow_id = "power_cost_fc_cells_reads",
          plot_type = "cost_tradeoff_curves",
          category = "power_cost_multi",
          minimizing_parameter = "minimum_fold_change", 
          title = "Minimize Fold Change",
          description = "Power+cost optimization with fold change, cells, and reads varying"
        ))
      } else if (length(varying_params) == 1) {
        other_param_ui <- varying_params[1]
        # Map UI control name to internal name
        other_param <- switch(other_param_ui,
          "cells_per_target" = "cells",
          "mapped_reads_per_cell" = "reads",
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
  
  # Allow empty minimization target during UI transitions to prevent premature validation errors
  # Only validate if both optimization type and minimization target are properly set
  if (!is.null(design_config$optimization_type) && design_config$optimization_type != "" &&
      (is.null(design_config$minimization_target) || design_config$minimization_target == "")) {
    # During mode switching, minimization target gets temporarily cleared
    # Only report as error if this appears to be a stable configuration attempt
    errors <- c(errors, "Please select a minimization target to continue analysis")
  }
  
  # Validate minimization target compatibility with optimization type
  if (!is.null(design_config$optimization_type) && !is.null(design_config$minimization_target) &&
      design_config$optimization_type != "" && design_config$minimization_target != "") {
    
    opt_type <- design_config$optimization_type
    target <- design_config$minimization_target
    
    # Power+cost mode can only minimize TPM_threshold or minimum_fold_change
    if (opt_type == "power_cost" && !target %in% c("TPM_threshold", "minimum_fold_change")) {
      errors <- c(errors, paste("Power+cost optimization can only minimize TPM threshold or fold change, not", target))
    }
    
    # Power-only mode supports all targets
    if (opt_type == "power_only" && !target %in% c("cells_per_target", "reads_per_cell", "TPM_threshold", "minimum_fold_change", "cost")) {
      errors <- c(errors, paste("Invalid minimization target for power-only optimization:", target))
    }
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
    "TPM_threshold" = seq(5, 50, 1),
    "fold_change" = seq(1.2, 2.0, 0.1),
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
    # Full parameter names (preferred)
    "cells_per_target" = "Cells per Target",
    "mapped_reads_per_cell" = "Mapped Reads per Cell",
    "TPM_threshold" = "TPM Threshold",
    "minimum_fold_change" = "Fold Change",
    # Legacy abbreviated names (backward compatibility)
    "cells" = "Cells per Target",
    "reads" = "Reads per Cell", 
    "TPM_threshold" = "TPM Threshold",
    "fold_change" = "Fold Change",
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
    user_session_id = "placeholder_session"  # Session tracking placeholder
  )
}