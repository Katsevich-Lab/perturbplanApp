#' Design Options Business Logic Functions
#'
#' @description This file contains extracted business logic functions for the
#' design_options module, following Golem best practices by separating pure
#' functions from reactive module code.
#'
#' @noRd
NULL

#' Generate Design Problem Summary Text
#'
#' @description Creates a human-readable summary of the user's optimization
#' objective based on their design choices.
#'
#' @param opt_type Character. Optimization type: "power_only" or "power_cost"
#' @param target Character. Minimization target parameter
#' @param power Numeric. Target power threshold (0-1)
#' @param cost_budget Numeric. Cost budget (for power_cost optimization)
#' @param param_configs List. Parameter configuration from get_param_configs()
#' @param cells_per_target_control Character. User control choice for cells parameter
#' @param reads_per_cell_control Character. User control choice for reads parameter
#' @param TPM_control Character. User control choice for TPM parameter
#' @param fc_control Character. User control choice for fold change parameter
#'
#' @return Character string with HTML formatting describing the optimization objective
#'
#' @noRd
generate_design_summary <- function(opt_type, target, power, cost_budget, param_configs = NULL,
                                   cells_per_target_control = NULL, reads_per_cell_control = NULL,
                                   TPM_control = NULL, fc_control = NULL) {
  # Base text
  if (opt_type == "power_only") {
    if (target == "cost") {
      return(paste0(
        "Find the minimum <strong>total cost</strong> for which power is at least <strong>",
        power * 100, "%</strong>, while varying cells per target and reads per cell, keeping TPM threshold and fold change fixed."
      ))
    } else {
      target_name <- switch(target,
        "cells_per_target" = "cells per target",
        "reads_per_cell" = "reads per cell",
        "TPM_threshold" = "TPM threshold",
        "minimum_fold_change" = "fold change"
      )
      return(paste0(
        "Find the minimum <strong>", target_name, "</strong> for which power is at least <strong>",
        power * 100,
        "%</strong>, keeping all other parameters fixed."
      ))
    }
  } else if (opt_type == "power_cost") {
    target_name <- switch(target,
      "TPM_threshold" = "TPM threshold",
      "minimum_fold_change" = "fold change"
    )

    # Generate specific parameter description based on actual Step 3 input states
    param_desc <- ""

    # Use actual control input values if available, otherwise fall back to param_configs
    actual_cells_type <- cells_per_target_control
    actual_reads_type <- reads_per_cell_control

    # If inputs not available, use resolved configs
    if (is.null(actual_cells_type) && !is.null(param_configs)) {
      actual_cells_type <- param_configs$cells_per_target$type
    }
    if (is.null(actual_reads_type) && !is.null(param_configs)) {
      actual_reads_type <- param_configs$reads_per_cell$type
    }

    if (!is.null(actual_cells_type) && !is.null(actual_reads_type)) {
      # Build cells/reads description
      cells_reads_desc <- ""
      if (actual_cells_type == "varying" && actual_reads_type == "varying") {
        cells_reads_desc <- "varying cells per target and reads per cell"
      } else if (actual_cells_type == "fixed" && actual_reads_type == "varying") {
        cells_reads_desc <- "keeping cells per target fixed and varying reads per cell"
      } else if (actual_cells_type == "varying" && actual_reads_type == "fixed") {
        cells_reads_desc <- "varying cells per target and keeping reads per cell fixed"
      } else if (actual_cells_type == "fixed" && actual_reads_type == "fixed") {
        cells_reads_desc <- "keeping both cells per target and reads per cell fixed"
      } else {
        cells_reads_desc <- "optimizing cells per target and reads per cell parameters"
      }

      # Add TPM/FC information for power+cost workflows
      TPM_fc_desc <- ""
      if (target == "TPM_threshold") {
        TPM_fc_desc <- "keeping fold change fixed"
      } else if (target == "minimum_fold_change") {
        TPM_fc_desc <- "keeping TPM threshold fixed"
      }

      # Combine descriptions - always include TPM/FC info for power+cost
      if (TPM_fc_desc != "") {
        param_desc <- paste0("while ", cells_reads_desc, " and ", TPM_fc_desc)
      } else {
        param_desc <- paste0("while ", cells_reads_desc)
      }
    } else {
      param_desc <- "while configuring parameter constraints"
    }

    return(paste0(
      "Find the minimum <strong>", target_name, "</strong> for which power is at least <strong>",
      power * 100,
      "%</strong> and cost is at most <strong>$",
      format(cost_budget, big.mark = ",", scientific = FALSE),
      "</strong>, ", param_desc, "."
    ))
  }

  return("Please complete all design options to see your optimization objective.")
}

#' Get Parameter Configurations for Each Workflow
#'
#' @description Defines parameter types (varying/fixed/minimizing/optimizing)
#' for each of the 11 supported optimization workflows.
#'
#' @param opt_type Character. Optimization type: "power_only" or "power_cost"
#' @param target Character. Minimization target parameter
#'
#' @return List with parameter configurations for cells_per_target, reads_per_cell,
#' TPM_threshold, and minimum_fold_change
#'
#' @noRd
get_param_configs <- function(opt_type, target) {
  configs <- list(
    cells_per_target = list(type = "varying", enabled = TRUE),
    reads_per_cell = list(type = "varying", enabled = TRUE),
    TPM_threshold = list(type = "varying", enabled = TRUE),
    minimum_fold_change = list(type = "varying", enabled = TRUE)
  )

  if (opt_type == "power_only") {
    if (target %in% c("cells_per_target", "reads_per_cell", "TPM_threshold", "minimum_fold_change")) {
      # Power-only + single parameter minimization: minimize target, fix all others
      configs$cells_per_target$type <- if (target == "cells_per_target") "minimizing" else "fixed"
      configs$reads_per_cell$type <- if (target == "reads_per_cell") "minimizing" else "fixed"
      configs$TPM_threshold$type <- if (target == "TPM_threshold") "minimizing" else "fixed"
      configs$minimum_fold_change$type <- if (target == "minimum_fold_change") "minimizing" else "fixed"
    } else if (target == "cost") {
      # Cost minimization: cells/reads vary simultaneously (omit both), TPM/fc fixed
      configs$cells_per_target$type <- "optimizing"
      configs$reads_per_cell$type <- "optimizing"
      configs$TPM_threshold$type <- "fixed"
      configs$minimum_fold_change$type <- "fixed"
    }
  } else if (opt_type == "power_cost") {
    if (target == "TPM_threshold") {
      # Power+cost + TPM minimization: TPM minimizing, FC fixed, cells/reads constrained varying/fixed
      configs$cells_per_target$type <- "varying"
      configs$reads_per_cell$type <- "varying"
      configs$TPM_threshold$type <- "minimizing"
      configs$minimum_fold_change$type <- "fixed"
    } else if (target == "minimum_fold_change") {
      # Power+cost + FC minimization: FC minimizing, TPM fixed, cells/reads constrained varying/fixed
      configs$cells_per_target$type <- "varying"
      configs$reads_per_cell$type <- "varying"
      configs$TPM_threshold$type <- "fixed"
      configs$minimum_fold_change$type <- "minimizing"
    }
  }

  return(configs)
}

#' Create Parameter UI Control Based on Type
#'
#' @description Generates UI elements for parameter controls in Step 3 based
#' on the parameter configuration type.
#'
#' @param ns Function. Namespace function from the module
#' @param param_id Character. Parameter identifier (e.g., "cells_per_target")
#' @param label Character. Display label for the parameter
#' @param config List. Parameter configuration with type and enabled status
#' @param default_val Numeric. Default value for the parameter
#' @param min_val Numeric. Minimum allowed value
#' @param max_val Numeric. Maximum allowed value
#' @param step_val Numeric. Step size for numeric inputs
#'
#' @return Shiny UI element or NULL if parameter should not be displayed
#'
#' @noRd
#'
#' @importFrom shiny tags div selectInput
create_param_ui <- function(ns, param_id, label, config, default_val, min_val, max_val, step_val) {
  if (config$type == "varying") {
    # For varying parameters: show dropdown with varying/fixed options
    # Fixed value inputs are now in their logical sidebar sections
    tags$div(
      style = "margin-bottom: 15px;",
      selectInput(ns(paste0(param_id, "_control")), label,
                 choices = list("Varying" = "varying", "Fixed" = "fixed"),
                 selected = "varying")
    )
  } else if (config$type == "fixed") {
    # For fixed parameters: Don't show redundant "(Fixed)" labels
    # Users can see fixed values in the summary section
    NULL
  }
  # Note: minimizing parameters are completely omitted (return NULL)
}

#' Get Resolved Parameter Controls
#'
#' @description Merges business logic parameter configurations with user input
#' choices to determine final parameter control states.
#'
#' @param opt_type Character. Optimization type: "power_only" or "power_cost"
#' @param target Character. Minimization target parameter
#' @param input_vals List. Input values from the Shiny session
#'
#' @return List with resolved parameter configurations including types and fixed values
#'
#' @noRd
get_resolved_param_controls <- function(opt_type, target, input_vals) {
  # Get the base parameter configs using business logic
  param_configs <- get_param_configs(opt_type, target)

  # Override with actual user input from Step 3 controls when available
  # But ONLY allow user input to override "varying" types (user has a choice)
  # Never allow override of "minimizing", "optimizing", or "fixed" types (business logic)
  cells_type <- param_configs$cells_per_target$type
  if (!is.null(input_vals$cells_per_target_control) &&
      param_configs$cells_per_target$type == "varying") {
    cells_type <- input_vals$cells_per_target_control
  }

  reads_type <- param_configs$reads_per_cell$type
  if (!is.null(input_vals$reads_per_cell_control) &&
      param_configs$reads_per_cell$type == "varying") {
    reads_type <- input_vals$reads_per_cell_control
  }

  TPM_type <- param_configs$TPM_threshold$type
  # Only allow user override if base config allows varying/fixed choice
  if (!is.null(input_vals$TPM_control) &&
      param_configs$TPM_threshold$type == "varying") {
    TPM_type <- input_vals$TPM_control
  }

  fc_type <- param_configs$minimum_fold_change$type
  # Only allow user override if base config allows varying/fixed choice
  if (!is.null(input_vals$fc_control) &&
      param_configs$minimum_fold_change$type == "varying") {
    fc_type <- input_vals$fc_control
  }

  list(
    cells_per_target = list(
      type = cells_type,
      fixed_value = if(!is.null(input_vals$cells_fixed)) input_vals$cells_fixed else NULL
    ),
    reads_per_cell = list(
      type = reads_type,
      fixed_value = if(!is.null(input_vals$reads_per_cell_fixed)) input_vals$reads_per_cell_fixed else NULL
    ),
    TPM_threshold = list(
      type = TPM_type,
      fixed_value = if(!is.null(input_vals$TPM_fixed)) input_vals$TPM_fixed else NULL
    ),
    minimum_fold_change = list(
      type = fc_type,
      fixed_value = if(!is.null(input_vals$minimum_fold_change_fixed)) input_vals$minimum_fold_change_fixed else NULL
    )
  )
}

#' Create Cost Parameters UI
#'
#' @description Creates a standardized cost parameters input section with
#' cost per cell and cost per million reads inputs.
#'
#' @param ns Function. Namespace function from the module
#' @param id_prefix Character. Prefix for input IDs (e.g., "cost_per_cell", "cost_per_cell_min")
#' @param cost_per_cell_default Numeric. Default value for cost per cell input
#' @param cost_per_million_reads_default Numeric. Default value for cost per million reads input
#'
#' @return Shiny UI element with cost parameter inputs
#'
#' @noRd
#'
#' @importFrom shiny tags div span numericInput
create_cost_inputs_ui <- function(ns, id_prefix, cost_per_cell_default = 0.086, cost_per_million_reads_default = 0.374) {
  cost_per_cell_id <- paste0(id_prefix, "_cost_per_cell")
  cost_per_million_reads_id <- paste0(id_prefix, "_cost_per_million_reads")

  # Remove prefix from cost_per_cell_id if it's redundant
  if (id_prefix == "cost_per_cell") {
    cost_per_cell_id <- "cost_per_cell"
    cost_per_million_reads_id <- "cost_per_million_reads"
  } else if (id_prefix == "cost_per_cell_min") {
    cost_per_cell_id <- "cost_per_cell_min"
    cost_per_million_reads_id <- "cost_per_million_reads_min"
  }

  tags$div(
    # Two row cost inputs layout
    tags$div(
      # Cost per cell row
      tags$div(
        style = "margin-bottom: 10px;",
        tags$span("Cost/cell ($): ", style = "font-weight: normal; margin-right: 5px;"),
        tags$div(
          style = "display: inline-block; width: 80px;",
          numericInput(ns(cost_per_cell_id),
                      label = NULL,
                      value = cost_per_cell_default,
                      min = 0,
                      step = 0.001)
        )
      ),
      # Cost per million reads row
      tags$div(
        tags$span("Cost/million reads ($): ", style = "font-weight: normal; margin-right: 5px;"),
        tags$div(
          style = "display: inline-block; width: 80px;",
          numericInput(ns(cost_per_million_reads_id),
                      label = NULL,
                      value = cost_per_million_reads_default,
                      min = 0,
                      step = 0.001)
        )
      )
    ),
    # CSS to override Shiny's default input width
    tags$style(paste0(
      "#", ns(cost_per_cell_id), " { width: 80px !important; }",
      "#", ns(cost_per_million_reads_id), " { width: 80px !important; }"
    ))
  )
}

# =============================================================================
# Progressive Disclosure Helper Functions
# =============================================================================
#
# These functions support the unified progressive disclosure controller
# by extracting validation logic and UI control operations from the
# complex observer chain in mod_design_options_server().

#' Validate Step 1 Completion
#'
#' @description Checks if Step 1 (optimization type selection) is complete
#'
#' @param optimization_type Character. The selected optimization type
#'
#' @return Logical. TRUE if Step 1 is complete, FALSE otherwise
#'
#' @noRd
is_assay_type_complete <- function(assay_type) {
  !is.null(assay_type) && assay_type != ""
}

is_step2_complete <- function(optimization_type) {
  !is.null(optimization_type) && optimization_type != ""
}

#' Validate Power Input Readiness
#'
#' @description Checks if power input is ready for Step 2 visibility
#'
#' @param target_power Numeric. The target power value
#'
#' @return Logical. TRUE if power input is valid, FALSE otherwise
#'
#' @noRd
is_power_input_ready <- function(target_power) {
  # Stub implementation
  !is.null(target_power) && is.numeric(target_power) && target_power > 0
}

#' Validate Cost Budget Readiness
#'
#' @description Checks if cost budget is ready when required for power+cost optimization
#'
#' @param cost_budget Numeric. The cost budget value
#' @param optimization_type Character. The optimization type
#'
#' @return Logical. TRUE if cost budget is valid or not required, FALSE otherwise
#'
#' @noRd
is_cost_budget_ready <- function(cost_budget, optimization_type) {
  # Stub implementation
  if (optimization_type == "power_cost") {
    !is.null(cost_budget) && is.numeric(cost_budget) && cost_budget > 0
  } else {
    TRUE  # Not required for power-only optimization
  }
}

#' Validate Step 2 Completion
#'
#' @description Checks if Step 2 (minimization target selection) is complete
#'
#' @param minimization_target Character. The selected minimization target
#'
#' @return Logical. TRUE if Step 2 is complete, FALSE otherwise
#'
#' @noRd
is_step3_complete <- function(minimization_target) {
  # Stub implementation
  !is.null(minimization_target) && minimization_target != ""
}

#' Check if Workflow Has Varying Parameters
#'
#' @description Determines if the current workflow has any varying parameters
#' that require Step 3 controls
#'
#' @param optimization_type Character. The optimization type
#' @param minimization_target Character. The minimization target
#'
#' @return Logical. TRUE if workflow has varying parameters, FALSE otherwise
#'
#' @noRd
workflow_has_varying_parameters <- function(optimization_type, minimization_target) {
  # Stub implementation - will use existing get_param_configs logic
  if (is.null(optimization_type) || optimization_type == "" ||
      is.null(minimization_target) || minimization_target == "") {
    return(FALSE)
  }

  param_configs <- get_param_configs(optimization_type, minimization_target)

  # Check if any parameters are varying type
  any(
    param_configs$cells_per_target$type == "varying",
    param_configs$reads_per_cell$type == "varying",
    param_configs$TPM_threshold$type == "varying",
    param_configs$minimum_fold_change$type == "varying"
  )
}

#' Check if All Parameter Controls Are Set
#'
#' @description Validates that all required parameter controls have been set
#' for summary generation
#'
#' @param optimization_type Character. The optimization type
#' @param minimization_target Character. The minimization target
#' @param parameter_controls List. The current parameter control inputs
#'
#' @return Logical. TRUE if all required controls are set, FALSE otherwise
#'
#' @noRd
are_all_parameter_controls_set <- function(optimization_type, minimization_target, parameter_controls) {
  # Real implementation
  if (is.null(optimization_type) || optimization_type == "" ||
      is.null(minimization_target) || minimization_target == "") {
    return(FALSE)
  }

  # For workflows without varying parameters, all controls are "set" by default
  if (!workflow_has_varying_parameters(optimization_type, minimization_target)) {
    return(TRUE)
  }

  # For workflows with varying parameters, check that control inputs exist
  param_configs <- get_param_configs(optimization_type, minimization_target)

  # Check each varying parameter has a control input set
  if (param_configs$cells_per_target$type == "varying" &&
      (is.null(parameter_controls$cells_per_target_control) || parameter_controls$cells_per_target_control == "")) {
    return(FALSE)
  }

  if (param_configs$reads_per_cell$type == "varying" &&
      (is.null(parameter_controls$reads_per_cell_control) || parameter_controls$reads_per_cell_control == "")) {
    return(FALSE)
  }

  if (param_configs$TPM_threshold$type == "varying" &&
      (is.null(parameter_controls$TPM_control) || parameter_controls$TPM_control == "")) {
    return(FALSE)
  }

  if (param_configs$minimum_fold_change$type == "varying" &&
      (is.null(parameter_controls$fc_control) || parameter_controls$fc_control == "")) {
    return(FALSE)
  }

  return(TRUE)
}

#' Toggle Power/Cost Inputs Section
#'
#' @description Shows or hides the power/cost inputs section and handles
#' conditional cost budget visibility
#'
#' @param session Shiny session object
#' @param show Logical. Whether to show the section
#' @param optimization_type Character. The optimization type (for cost budget logic)
#'
#' @return NULL (side effects only)
#'
#' @noRd
#'
#' @importFrom shinyjs show hide
toggle_power_cost_inputs <- function(session, show, optimization_type = NULL) {
  # Real implementation
  if (show) {
    shinyjs::show("power_cost_inputs")
    # Handle cost budget conditional visibility
    if (!is.null(optimization_type) && optimization_type == "power_cost") {
      shinyjs::show("cost_budget_div")
    } else {
      shinyjs::hide("cost_budget_div")
    }
  } else {
    shinyjs::hide("power_cost_inputs")
    # Cascade hide downstream sections when power/cost inputs are hidden
    shinyjs::hide("step3")
    shinyjs::hide("step4")
  }
}

#' Toggle Step 2 Section
#'
#' @description Shows or hides Step 2 and cascades to Step 3 when hiding
#'
#' @param session Shiny session object
#' @param show Logical. Whether to show Step 2
#'
#' @return NULL (side effects only)
#'
#' @noRd
#'
#' @importFrom shinyjs show hide
toggle_step2_section <- function(session, show) {
  # Real implementation
  if (show) {
    shinyjs::show("step2")
  } else {
    shinyjs::hide("step2")
    shinyjs::hide("power_cost_inputs")
    shinyjs::hide("step3")  # Cascade hide Step 3
    shinyjs::hide("design_summary")  # Also hide summary
  }
}

#' Toggle Step 3 Section
#'
#' @description Shows or hides Step 3 based on varying parameters availability
#'
#' @param session Shiny session object
#' @param show Logical. Whether to show Step 3
#'
#' @return NULL (side effects only)
#'
#' @noRd
#'
#' @importFrom shinyjs show hide
toggle_step3_section <- function(session, show) {
  # Real implementation
  if (show) {
    shinyjs::show("step3")
  } else {
    shinyjs::hide("step3")
    shinyjs::hide("step4")  # Cascade hide Step 4
    shinyjs::hide("design_summary")  # Hide summary when Step 3 is hidden
  }
}

toggle_step4_section <- function(session, show) {
  if (show) {
    shinyjs::show("step4")
  } else {
    shinyjs::hide("step4")
    shinyjs::hide("design_summary")  # Hide summary when Step 4 is hidden
  }
}

#' Toggle Cost Minimization Parameters
#'
#' @description Shows or hides cost minimization parameters based on target
#'
#' @param session Shiny session object
#' @param show Logical. Whether to show cost minimization parameters
#'
#' @return NULL (side effects only)
#'
#' @noRd
#'
#' @importFrom shinyjs show hide
toggle_cost_minimization_params <- function(session, show) {
  # Real implementation
  if (show) {
    shinyjs::show("cost_minimization_params")
  } else {
    shinyjs::hide("cost_minimization_params")
  }
}

#' Toggle Design Summary Section
#'
#' @description Shows or hides the design problem summary and updates content
#'
#' @param session Shiny session object
#' @param show Logical. Whether to show the summary
#' @param summary_text Character. The summary text to display (if showing)
#'
#' @return NULL (side effects only)
#'
#' @noRd
#'
#' @importFrom shinyjs show hide html
toggle_design_summary <- function(session, show, summary_text = NULL) {
  # Real implementation
  if (show && !is.null(summary_text)) {
    shinyjs::html("summary_text", summary_text)
    shinyjs::show("design_summary")
  } else {
    shinyjs::hide("design_summary")
  }
}

#' Reset Input Values
#'
#' @description Resets target power and cost budget to default values
#'
#' @param session Shiny session object
#'
#' @return NULL (side effects only)
#'
#' @noRd
#'
#' @importFrom shiny updateNumericInput
reset_input_values <- function(session) {
  # Real implementation
  updateNumericInput(session, "target_power", value = 0.8)
  updateNumericInput(session, "cost_budget", value = 10000)
}
