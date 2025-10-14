#' Clean Solution Table Functions
#'
#' @description Clean implementation of solution table with proper two-row header
#' architecture and simplified parameter extraction logic.
#'

# ============================================================================
# MAIN ENTRY POINT - ONLY FUNCTION CALLED BY MODULE
# ============================================================================

#' Create Enhanced Solutions Table (Clean Implementation)
#'
#' @param results Analysis results object containing optimal_design, user_config, workflow_info
#' @return Shiny UI tagList with clean solution table
#' @export
create_enhanced_solutions_table <- function(results) {
  # Handle both legacy single results and new cached results format
  if (!is.null(results$current_result) || !is.null(results$all_results)) {
    # New cached results format
    solutions_data <- extract_cached_solutions_data(results)

    # Get workflow_info from current result or first pinned result
    workflow_info <- if (!is.null(results$current_result)) {
      results$current_result$user_config$workflow_info
    } else if (length(results$pinned_solutions) > 0) {
      results$pinned_solutions[[1]]$user_config$workflow_info
    } else {
      return(create_empty_solutions_table())
    }

  }

  # Create clean table UI with proper two-row header
  create_clean_solutions_table_ui(solutions_data, workflow_info)
}

#' Extract Cached Solutions Data
#'
#' @description Handles cached_results format with current and pinned solutions
#' @param cached_results Cached results from mod_results_cache
#' @return List with structured solutions data for all solutions
extract_cached_solutions_data <- function(cached_results) {
  solutions_list <- list()
  solution_counter <- 1

  # Add current result if available
  if (!is.null(cached_results$current_result)) {
    current <- cached_results$current_result
    # Extract assay_type for assay-aware formatting
    assay_type <- current$user_config$design_options$assay_type

    solutions_list[[solution_counter]] <- list(
      solution_id = solution_counter,
      solution_name = "Current",
      achieved_power = round(current$optimal_design$achieved_power %||% NA, 3),
      total_cost = if (!is.null(current$optimal_design$total_cost)) current$optimal_design$total_cost else NULL,
      optimal_value = extract_optimal_parameter_value(current$optimal_design, current$user_config$workflow_info$minimizing_parameter, assay_type),
      experimental_params = extract_experimental_parameters(current$optimal_design, current$user_config, current$user_config$workflow_info$minimizing_parameter, current$user_config$workflow_info),
      TPM_threshold = extract_TPM_threshold(current$optimal_design, current$user_config, current$user_config$workflow_info$minimizing_parameter, assay_type),
      effect_sizes = extract_effect_sizes_clean(current$optimal_design, current$user_config, current$user_config$workflow_info$minimizing_parameter),
      minimizing_param = current$user_config$workflow_info$minimizing_parameter,
      assay_type = assay_type,
      user_config = current$user_config,
      cost_params = extract_cost_parameters(current$user_config),
      advanced_params = extract_advanced_parameters(current$user_config)
    )
    solution_counter <- solution_counter + 1
  }

  # Add pinned solutions if available
  if (!is.null(cached_results$pinned_solutions) && length(cached_results$pinned_solutions) > 0) {
    for (i in seq_along(cached_results$pinned_solutions)) {
      solution_name <- names(cached_results$pinned_solutions)[i]
      pinned <- cached_results$pinned_solutions[[i]]
      # Extract assay_type for assay-aware formatting
      assay_type <- pinned$user_config$design_options$assay_type

      solutions_list[[solution_counter]] <- list(
        solution_id = solution_counter,
        solution_name = solution_name,
        achieved_power = round(pinned$optimal_design$achieved_power %||% NA, 3),
        total_cost = if (!is.null(pinned$optimal_design$total_cost)) pinned$optimal_design$total_cost else NULL,
        optimal_value = extract_optimal_parameter_value(pinned$optimal_design, pinned$user_config$workflow_info$minimizing_parameter, assay_type),
        experimental_params = extract_experimental_parameters(pinned$optimal_design, pinned$user_config, pinned$user_config$workflow_info$minimizing_parameter, pinned$user_config$workflow_info),
        TPM_threshold = extract_TPM_threshold(pinned$optimal_design, pinned$user_config, pinned$user_config$workflow_info$minimizing_parameter, assay_type),
        effect_sizes = extract_effect_sizes_clean(pinned$optimal_design, pinned$user_config, pinned$user_config$workflow_info$minimizing_parameter),
        minimizing_param = pinned$user_config$workflow_info$minimizing_parameter,
        assay_type = assay_type,
        user_config = pinned$user_config,
        cost_params = extract_cost_parameters(pinned$user_config),
        advanced_params = extract_advanced_parameters(pinned$user_config)
      )
      solution_counter <- solution_counter + 1
    }
  }

  return(solutions_list)
}

#' Extract Cost Parameters from User Config
#'
#' @description Extracts cost parameters for Excel export (not shown in UI table)
#' @param user_config User configuration data from sidebar
#' @return List with cost_per_cell and cost_per_million_reads
#' @noRd
extract_cost_parameters <- function(user_config) {
  list(
    cost_per_cell = user_config$design_options$cost_per_cell %||% NULL,
    cost_per_million_reads = user_config$design_options$cost_per_million_reads %||% NULL
  )
}

#' Extract Advanced Parameters from User Config
#'
#' @description Extracts advanced parameters for Excel export (not shown in UI table)
#' @param user_config User configuration data from sidebar
#' @return List with fdr_target and other advanced settings
#' @noRd
extract_advanced_parameters <- function(user_config) {
  list(
    fdr_target = user_config$advanced_choices$fdr_target %||% NULL,
    grna_variability = user_config$advanced_choices$gRNA_variability %||% NULL,
    mapping_efficiency = user_config$advanced_choices$mapping_efficiency %||% NULL,
    control_group = user_config$advanced_choices$control_group %||% NULL
  )
}

#' Extract Optimal Parameter Value
#'
#' @param optimal Optimal design results containing parameter values
#' @param minimizing_param Name of the parameter being minimized
#' @param assay_type Character: "tap_seq" or "perturb_seq" (optional, for TPM_threshold rounding)
#' @noRd
extract_optimal_parameter_value <- function(optimal, minimizing_param, assay_type = NULL) {
  switch(minimizing_param,
    "cells_per_target" = format_number(optimal$cells_per_target %||% NA),
    "reads_per_cell" = format_number(optimal$sequenced_reads_per_cell %||% optimal$reads_per_cell %||% NA),
    "sequenced_reads_per_cell" = format_number(optimal$sequenced_reads_per_cell %||% NA),
    "TPM_threshold" = {
      # Use Expression_threshold if available (transformed value), otherwise TPM_threshold
      value <- optimal$Expression_threshold %||% optimal$TPM_threshold %||% NA
      # Round based on assay type
      if (!is.null(assay_type) && assay_type == "tap_seq") {
        round(value, 2)  # TAP-seq: 2 decimals
      } else {
        round(value, 0)  # Perturb-seq: integer
      }
    },
    "minimum_fold_change" = round(optimal$minimum_fold_change %||% NA, 2),
    "cost" = if (!is.null(optimal$total_cost)) paste0("$", format_number(optimal$total_cost)) else "N/A",
    "N/A"
  )
}

#' Extract Experimental Parameters
#'
#' @param optimal Optimal design results containing parameter values
#' @param user_config User configuration data from sidebar
#' @param minimizing_param Name of the parameter being minimized
#' @param workflow_info Workflow information containing workflow details
#' @noRd
extract_experimental_parameters <- function(optimal, user_config, minimizing_param, workflow_info = NULL) {

  # Get all experimental parameter values
  params <- list(
    moi = get_param_value("MOI", optimal, user_config, minimizing_param, workflow_info),
    num_targets = get_param_value("num_targets", optimal, user_config, minimizing_param, workflow_info),
    grnas_per_target = get_param_value("gRNAs_per_target", optimal, user_config, minimizing_param, workflow_info),
    cells_per_target = get_param_value("cells_per_target", optimal, user_config, minimizing_param, workflow_info),
    reads_per_cell = get_param_value("reads_per_cell", optimal, user_config, minimizing_param, workflow_info)
  )

  # Exclude minimizing parameter to avoid duplication with optimal column
  if (minimizing_param == "cells_per_target") {
    params$cells_per_target <- NULL  # Will show "—" in table
  } else if (minimizing_param == "reads_per_cell") {
    params$reads_per_cell <- NULL  # Will show "—" in table
  }

  return(params)
}

#' Extract TPM Threshold
#'
#' @param optimal Optimal design results containing parameter values
#' @param user_config User configuration data from sidebar
#' @param minimizing_param Name of the parameter being minimized
#' @param assay_type Assay type ("tap_seq" or "perturb_seq")
#' @noRd
extract_TPM_threshold <- function(optimal, user_config, minimizing_param, assay_type = NULL) {
  if (minimizing_param == "TPM_threshold") {
    # TPM is minimizing parameter - exclude from this column (will appear in optimal column)
    return(NULL)  # Will show "—" in table
  } else {
    # Use Expression_threshold_fixed from user config (already transformed for display)
    value <- user_config$analysis_choices$Expression_threshold_fixed %||%
             user_config$analysis_choices$TPM_threshold_fixed %||% NA
    if (!is.na(value)) {
      # Apply assay-specific rounding
      if (!is.null(assay_type) && assay_type == "tap_seq") {
        round(value, 2)  # TAP-seq: 2 decimals
      } else {
        round(value, 0)  # Perturb-seq: integer
      }
    } else {
      "N/A"
    }
  }
}

#' Extract Effect Sizes (Clean)
#'
#' @param optimal Optimal design results containing parameter values
#' @param user_config User configuration data from sidebar
#' @param minimizing_param Name of the parameter being minimized
#' @noRd
extract_effect_sizes_clean <- function(optimal, user_config, minimizing_param) {
  # Fold change - exclude if it's the minimizing parameter
  if (minimizing_param == "minimum_fold_change") {
    fold_change <- NULL  # Will show "—" in table (appears in optimal column instead)
  } else {
    fold_change <- round(user_config$effect_sizes$minimum_fold_change %||% NA, 2)
    fold_change <- if (is.na(fold_change)) "N/A" else fold_change
  }

  # Non-null proportion (always from user config, never minimized)
  non_null_prop <- round(user_config$effect_sizes$prop_non_null %||% NA, 2)

  list(
    fold_change = fold_change,
    non_null_proportion = if (is.na(non_null_prop)) "N/A" else non_null_prop
  )
}

# ============================================================================
# UTILITY FUNCTIONS
# ============================================================================

#' Get Parameter Value (Optimal vs User Config)
#'
#' @param param_name Name of parameter to extract
#' @param optimal Optimal design results containing parameter values
#' @param user_config User configuration data from sidebar
#' @param minimizing_param Name of the parameter being minimized
#' @param workflow_info Workflow information containing workflow details
#' @noRd
get_param_value <- function(param_name, optimal, user_config, minimizing_param, workflow_info = NULL) {

  # Check if this is a power+cost workflow or cost minimization workflow
  is_multi_param_workflow <- FALSE
  if (!is.null(workflow_info$workflow_id)) {
    # Power+cost workflows (all except power-only single parameter workflows)
    power_only_workflows <- c("power_single_cells_per_target", "power_single_reads_per_cell",
                              "power_single_TPM_threshold", "power_single_minimum_fold_change")
    is_multi_param_workflow <- !workflow_info$workflow_id %in% power_only_workflows
  }

  # For cells and reads parameters in multi-parameter workflows, check if they were varied
  if (is_multi_param_workflow && param_name %in% c("cells_per_target", "reads_per_cell")) {
    # Determine which parameters vary based on workflow type
    cells_varies <- FALSE
    reads_varies <- FALSE

    if (!is.null(workflow_info$workflow_id)) {
      # Cost minimization - both cells and reads vary
      if (workflow_info$workflow_id == "power_cost_minimization") {
        cells_varies <- TRUE
        reads_varies <- TRUE
      }
      # Power+cost workflows with cells varying
      else if (workflow_info$workflow_id %in% c("power_cost_TPM_cells", "power_cost_fc_cells",
                                                "power_cost_TPM_cells_reads", "power_cost_fc_cells_reads")) {
        cells_varies <- TRUE
      }
      # Power+cost workflows with reads varying
      if (workflow_info$workflow_id %in% c("power_cost_TPM_reads", "power_cost_fc_reads",
                                           "power_cost_TPM_cells_reads", "power_cost_fc_cells_reads")) {
        reads_varies <- TRUE
      }
    }

    # Use optimal value if parameter varies, user config if fixed
    if (param_name == "cells_per_target" && cells_varies) {
      format_number(optimal$cells_per_target %||% NA)
    } else if (param_name == "reads_per_cell" && reads_varies) {
      format_number(optimal$sequenced_reads_per_cell %||% optimal$reads_per_cell %||% NA)
    } else {
      # Parameter was fixed - use user config value
      # Map parameter names to user config structure
      config_param <- switch(param_name,
        "cells_per_target" = "cells_fixed",
        "reads_per_cell" = "reads_per_cell_fixed",
        param_name
      )
      value <- user_config$experimental_setup[[config_param]] %||%
               user_config$perturbation_choices[[config_param]] %||% NA
      format_number(value %||% NA)
    }
  } else if (minimizing_param == param_name) {
    # Get optimal value for minimizing parameter
    value <- optimal[[param_name]]
    if (param_name %in% c("cells_per_target", "num_targets")) {
      format_number(value %||% NA)
    } else if (param_name == "reads_per_cell") {
      format_number(optimal$sequenced_reads_per_cell %||% optimal$reads_per_cell %||% NA)
    } else {
      value %||% "N/A"
    }
  } else {
    # Get fixed value from user config
    # Map parameter names to user config structure
    config_param <- switch(param_name,
      "cells_per_target" = "cells_fixed",
      "reads_per_cell" = "reads_per_cell_fixed",
      param_name
    )
    value <- user_config$experimental_setup[[config_param]] %||%
             user_config$perturbation_choices[[config_param]] %||% NA
    if (param_name %in% c("cells_per_target", "num_targets")) {
      format_number(value %||% NA)
    } else if (param_name == "reads_per_cell") {
      format_number(value %||% NA)
    } else {
      value %||% "N/A"
    }
  }
}

#' Format Number Helper
#'
#' @param x Numeric value to format
#' @noRd
format_number <- function(x) {
  if (is.na(x) || is.null(x)) return("N/A")
  if (x >= 1000) {
    scales::comma(round(x))
  } else {
    as.character(round(x))
  }
}

# ============================================================================
# TABLE UI CREATION
# ============================================================================

#' Create Clean Solutions Table UI
#'
#' @param solutions_data Structured solutions data extracted from analysis results
#' @param workflow_info Workflow information containing minimizing parameter and workflow ID
#' @noRd
create_clean_solutions_table_ui <- function(solutions_data, workflow_info) {

  # Extract assay_type from first solution (all solutions have same assay type)
  assay_type <- solutions_data[[1]]$assay_type

  # Get dynamic column name for optimal parameter (assay-aware)
  optimal_col_name <- get_optimal_column_name(workflow_info$minimizing_parameter, assay_type)

  # Check if workflow includes cost (7 workflows have cost, 4 are power-only)
  has_cost <- !is.null(workflow_info$workflow_id) &&
              !workflow_info$workflow_id %in% c(
                "power_single_cells_per_target", "power_single_reads_per_cell",
                "power_single_TPM_threshold", "power_single_minimum_fold_change"
              )

  # Get minimizing parameter from first solution (all solutions have same workflow)
  minimizing_param <- solutions_data[[1]]$minimizing_param

  # Create single-row header (pass assay_type for assay-aware labels)
  header_row <- create_header_row_single(optimal_col_name, has_cost, minimizing_param, assay_type)

  # Create data rows for all solutions
  data_rows <- lapply(solutions_data, function(solution) {
    create_data_row(solution, workflow_info)
  })

  # Assemble table
  tags$div(
    class = "clean-solution-table",
    style = "overflow-x: auto; background: white; border-radius: 8px; box-shadow: 0 2px 4px rgba(0,0,0,0.1); padding: 15px;",
    tags$table(
      class = "table table-striped table-hover",
      style = "margin-bottom: 0; font-size: 16px; width: 100%;",
      tags$thead(
        header_row
      ),
      tags$tbody(
        data_rows
      )
    )
  )
}

#' Create Single Header Row
#'
#' @param optimal_col_name Display name for the optimal parameter column
#' @param has_cost Logical indicating if workflow includes cost calculations
#' @param minimizing_param Name of the parameter being minimized
#' @param assay_type Assay type ("tap_seq" or "perturb_seq")
#' @noRd
create_header_row_single <- function(optimal_col_name, has_cost = NULL, minimizing_param = NULL, assay_type = NULL) {

  # Header cell style - darker gray background
  header_style <- "text-align: center; font-weight: bold; vertical-align: middle; border-bottom: 1px solid #dee2e6; padding: 10px; background-color: #e9ecef;"

  cells <- list(
    tags$th("Setting", style = header_style),
    tags$th("Power", style = header_style)
  )

  # Add cost column if workflow includes cost AND cost is not being minimized
  if (has_cost && minimizing_param != "cost") {
    cells <- append(cells, list(
      tags$th("Cost", style = header_style)
    ))
  }

  # Add optimal parameter column
  cells <- append(cells, list(
    tags$th(optimal_col_name, style = header_style)
  ))

  # Add experimental parameter columns
  cells <- append(cells, list(
    tags$th("MOI", style = header_style),
    tags$th("# Targets", style = header_style),
    tags$th("gRNAs/Target", style = header_style)
  ))

  # Add Cells/Target column only if not being minimized
  if (minimizing_param != "cells_per_target") {
    cells <- append(cells, list(
      tags$th("Cells/Target", style = header_style)
    ))
  }

  # Add Reads/Cell column only if not being minimized
  if (minimizing_param != "reads_per_cell") {
    cells <- append(cells, list(
      tags$th("Reads/Cell", style = header_style)
    ))
  }

  # Add Expression Threshold column if not being minimized
  if (minimizing_param != "TPM_threshold") {
    # Assay-aware header label
    header_label <- if (!is.null(assay_type) && assay_type == "tap_seq") {
      "UMIs/cell threshold"
    } else {
      "TPM threshold"
    }
    cells <- append(cells, list(
      tags$th(header_label, style = header_style)
    ))
  }

  # Add Fold Change column only if not being minimized
  if (minimizing_param != "minimum_fold_change") {
    cells <- append(cells, list(
      tags$th("Fold Change", style = header_style)
    ))
  }

  # Always add Non-null Prop (never minimized)
  cells <- append(cells, list(
    tags$th("Non-null Prop", style = header_style)
  ))

  tags$tr(cells)
}

#' Create Data Row
#'
#' @param solution_data Individual solution data with optimal values and parameters
#' @param workflow_info Workflow information containing minimizing parameter and workflow ID
#' @noRd
create_data_row <- function(solution_data, workflow_info) {

  # Helper function to determine if a parameter should be bold (minimizing or varying)
  is_bold_parameter <- function(param_name) {
    # Always bold if minimizing
    if (solution_data$minimizing_param == param_name) {
      return(TRUE)
    }

    # Bold if varying or optimizing (check user_config parameter_controls)
    if (!is.null(solution_data$user_config) &&
        !is.null(solution_data$user_config$design_options) &&
        !is.null(solution_data$user_config$design_options$parameter_controls)) {

      param_controls <- solution_data$user_config$design_options$parameter_controls

      # Map parameter names to their control keys
      param_mapping <- list(
        "moi" = "MOI",
        "num_targets" = "num_targets",
        "grnas_per_target" = "gRNAs_per_target",
        "cells_per_target" = "cells_per_target",
        "reads_per_cell" = "reads_per_cell",
        "TPM_threshold" = "TPM_threshold",
        "fold_change" = "minimum_fold_change",
        "non_null_proportion" = "prop_non_null"
      )

      control_key <- param_mapping[[param_name]]

      if (!is.null(control_key) && !is.null(param_controls[[control_key]])) {
        param_type <- param_controls[[control_key]]$type
        return(param_type %in% c("varying", "optimizing"))
      }
    }

    return(FALSE)
  }

  cells <- list(
    tags$td(
      solution_data$solution_name %||% paste("Solution", solution_data$solution_id),
      style = "text-align: center; padding: 8px; font-weight: bold;"
    ),
    tags$td(solution_data$achieved_power, style = "text-align: center; padding: 8px;")
  )

  # Add cost if workflow includes cost
  has_cost <- !is.null(workflow_info$workflow_id) &&
              !workflow_info$workflow_id %in% c(
                "power_single_cells_per_target", "power_single_reads_per_cell",
                "power_single_TPM_threshold", "power_single_minimum_fold_change"
              )

  # Add cost column only if has_cost AND cost is not being minimized
  if (has_cost && solution_data$minimizing_param != "cost") {
    if (!is.null(solution_data$total_cost)) {
      cells <- append(cells, list(
        tags$td(paste0("$", format_number(solution_data$total_cost)), style = "text-align: center; padding: 8px;")
      ))
    } else {
      cells <- append(cells, list(
        tags$td("N/A", style = "text-align: center; padding: 8px;")
      ))
    }
  }

  # Add remaining columns
  exp_params <- solution_data$experimental_params
  effect_sizes <- solution_data$effect_sizes

  # Add optimal parameter column
  cells <- append(cells, list(
    tags$td(solution_data$optimal_value, style = "text-align: center; font-weight: bold; padding: 8px;")
  ))

  # Add experimental parameters (always include MOI, # targets, gRNAs/target)
  moi_style <- if (is_bold_parameter("moi")) "text-align: center; padding: 8px; font-weight: bold;" else "text-align: center; padding: 8px;"
  targets_style <- if (is_bold_parameter("num_targets")) "text-align: center; padding: 8px; font-weight: bold;" else "text-align: center; padding: 8px;"
  grnas_style <- if (is_bold_parameter("grnas_per_target")) "text-align: center; padding: 8px; font-weight: bold;" else "text-align: center; padding: 8px;"

  cells <- append(cells, list(
    tags$td(exp_params$moi %||% "N/A", style = moi_style),
    tags$td(exp_params$num_targets %||% "N/A", style = targets_style),
    tags$td(exp_params$grnas_per_target %||% "N/A", style = grnas_style)
  ))

  # Add Cells/Target column only if not being minimized
  if (solution_data$minimizing_param != "cells_per_target") {
    cells_style <- if (is_bold_parameter("cells_per_target")) "text-align: center; padding: 8px; font-weight: bold;" else "text-align: center; padding: 8px;"
    cells <- append(cells, list(
      tags$td(exp_params$cells_per_target %||% "N/A", style = cells_style)
    ))
  }

  # Add Reads/Cell column only if not being minimized
  if (solution_data$minimizing_param != "reads_per_cell") {
    reads_style <- if (is_bold_parameter("reads_per_cell")) "text-align: center; padding: 8px; font-weight: bold;" else "text-align: center; padding: 8px;"
    cells <- append(cells, list(
      tags$td(exp_params$reads_per_cell %||% "N/A", style = reads_style)
    ))
  }

  # Add TPM threshold column only if not being minimized
  if (solution_data$minimizing_param != "TPM_threshold") {
    tpm_style <- if (is_bold_parameter("TPM_threshold")) "text-align: center; padding: 8px; font-weight: bold;" else "text-align: center; padding: 8px;"
    cells <- append(cells, list(
      tags$td(solution_data$TPM_threshold %||% "N/A", style = tpm_style)
    ))
  }

  # Add Fold Change column only if not being minimized
  if (solution_data$minimizing_param != "minimum_fold_change") {
    fc_style <- if (is_bold_parameter("fold_change")) "text-align: center; padding: 8px; font-weight: bold;" else "text-align: center; padding: 8px;"
    cells <- append(cells, list(
      tags$td(effect_sizes$fold_change %||% "N/A", style = fc_style)
    ))
  }

  # Always add Non-null Proportion (never minimized)
  nnp_style <- if (is_bold_parameter("non_null_proportion")) "text-align: center; padding: 8px; font-weight: bold;" else "text-align: center; padding: 8px;"
  cells <- append(cells, list(
    tags$td(effect_sizes$non_null_proportion %||% "N/A", style = nnp_style)
  ))

  tags$tr(cells)
}

# ============================================================================
# HELPER FUNCTIONS
# ============================================================================

#' Get Optimal Column Name
#'
#' @param minimizing_param Name of the parameter being minimized
#' @noRd
get_optimal_column_name <- function(minimizing_param, assay_type = NULL) {
  switch(minimizing_param,
    "cells_per_target" = "Optimal Cells/Target",
    "reads_per_cell" = "Optimal Reads/Cell",
    "sequenced_reads_per_cell" = "Optimal Reads/Cell",
    "TPM_threshold" = {
      # Assay-aware column name
      if (!is.null(assay_type) && assay_type == "tap_seq") {
        "Optimal UMIs/cell threshold"
      } else {
        "Optimal TPM threshold"
      }
    },
    "minimum_fold_change" = "Optimal Fold Change",
    "cost" = "Optimal Cost",
    "Optimal Design"
  )
}

#' Create Empty Table
create_empty_solutions_table <- function() {
  tags$div(
    class = "empty-solution-table",
    style = "text-align: center; padding: 50px; background: white; border-radius: 8px; box-shadow: 0 2px 4px rgba(0,0,0,0.1);",
    tags$h5("No Solution Data Available", style = "color: #6c757d;"),
    tags$p("Run an analysis to see solution results.", style = "color: #6c757d;")
  )
}
