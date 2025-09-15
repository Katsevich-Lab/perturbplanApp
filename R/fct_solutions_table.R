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
#' @param user_config User configuration (unused, kept for compatibility)
#' @return Shiny UI tagList with clean solution table
#' @export
create_enhanced_solutions_table <- function(results, user_config = NULL) {
  # Handle both legacy single results and new cached results format
  if (!is.null(results$current_result) || !is.null(results$all_results)) {
    # New cached results format
    solutions_data <- extract_cached_solutions_data(results)

    # Get workflow_info from current result or first pinned result
    workflow_info <- if (!is.null(results$current_result)) {
      results$current_result$workflow_info
    } else if (length(results$pinned_solutions) > 0) {
      results$pinned_solutions[[1]]$workflow_info
    } else {
      return(create_empty_solutions_table())
    }

  } else {
    # Legacy single result format
    if (is.null(results$optimal_design) || is.null(results$workflow_info)) {
      return(create_empty_solutions_table())
    }

    solutions_data <- extract_clean_solutions_data(results)
    workflow_info <- results$workflow_info
  }

  # Create clean table UI with proper two-row header
  create_clean_solutions_table_ui(solutions_data, workflow_info)
}

# ============================================================================
# DATA EXTRACTION
# ============================================================================

#' Extract Clean Solutions Data
#'
#' @param results Complete analysis results object
#' @return List with structured solutions data
extract_clean_solutions_data <- function(results) {
  optimal <- results$optimal_design
  user_config <- results$user_config
  workflow_info <- results$workflow_info
  minimizing_param <- workflow_info$minimizing_parameter

  list(
    list(
      solution_id = 1,
      achieved_power = round(optimal$achieved_power %||% NA, 3),
      total_cost = if (!is.null(optimal$total_cost)) optimal$total_cost else NULL,
      optimal_value = extract_optimal_parameter_value(optimal, minimizing_param),
      experimental_params = extract_experimental_parameters(optimal, user_config, minimizing_param, workflow_info),
      TPM_threshold = extract_TPM_threshold(optimal, user_config, minimizing_param),
      effect_sizes = extract_effect_sizes_clean(optimal, user_config, minimizing_param),
      minimizing_param = minimizing_param
    )
  )
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
    solutions_list[[solution_counter]] <- list(
      solution_id = solution_counter,
      solution_name = "Current",
      achieved_power = round(current$optimal_design$achieved_power %||% NA, 3),
      total_cost = if (!is.null(current$optimal_design$total_cost)) current$optimal_design$total_cost else NULL,
      optimal_value = extract_optimal_parameter_value(current$optimal_design, current$workflow_info$minimizing_parameter),
      experimental_params = extract_experimental_parameters(current$optimal_design, current$user_config, current$workflow_info$minimizing_parameter, current$workflow_info),
      TPM_threshold = extract_TPM_threshold(current$optimal_design, current$user_config, current$workflow_info$minimizing_parameter),
      effect_sizes = extract_effect_sizes_clean(current$optimal_design, current$user_config, current$workflow_info$minimizing_parameter),
      minimizing_param = current$workflow_info$minimizing_parameter
    )
    solution_counter <- solution_counter + 1
  }

  # Add pinned solutions if available
  if (!is.null(cached_results$pinned_solutions) && length(cached_results$pinned_solutions) > 0) {
    for (i in seq_along(cached_results$pinned_solutions)) {
      solution_name <- names(cached_results$pinned_solutions)[i]
      pinned <- cached_results$pinned_solutions[[i]]

      solutions_list[[solution_counter]] <- list(
        solution_id = solution_counter,
        solution_name = solution_name,
        achieved_power = round(pinned$optimal_design$achieved_power %||% NA, 3),
        total_cost = if (!is.null(pinned$optimal_design$total_cost)) pinned$optimal_design$total_cost else NULL,
        optimal_value = extract_optimal_parameter_value(pinned$optimal_design, pinned$workflow_info$minimizing_parameter),
        experimental_params = extract_experimental_parameters(pinned$optimal_design, pinned$user_config, pinned$workflow_info$minimizing_parameter, pinned$workflow_info),
        TPM_threshold = extract_TPM_threshold(pinned$optimal_design, pinned$user_config, pinned$workflow_info$minimizing_parameter),
        effect_sizes = extract_effect_sizes_clean(pinned$optimal_design, pinned$user_config, pinned$workflow_info$minimizing_parameter),
        minimizing_param = pinned$workflow_info$minimizing_parameter
      )
      solution_counter <- solution_counter + 1
    }
  }

  return(solutions_list)
}

#' Extract Optimal Parameter Value
extract_optimal_parameter_value <- function(optimal, minimizing_param) {
  switch(minimizing_param,
    "cells_per_target" = format_number(optimal$cells_per_target %||% NA),
    "reads_per_cell" = format_number(optimal$sequenced_reads_per_cell %||% optimal$reads_per_cell %||% NA),
    "sequenced_reads_per_cell" = format_number(optimal$sequenced_reads_per_cell %||% NA),
    "TPM_threshold" = round(optimal$TPM_threshold %||% NA),
    "minimum_fold_change" = round(optimal$minimum_fold_change %||% NA, 2),
    "cost" = if (!is.null(optimal$total_cost)) paste0("$", format_number(optimal$total_cost)) else "N/A",
    "N/A"
  )
}

#' Extract Experimental Parameters
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
extract_TPM_threshold <- function(optimal, user_config, minimizing_param) {
  if (minimizing_param == "TPM_threshold") {
    # TPM is minimizing parameter - exclude from this column (will appear in optimal column)
    return(NULL)  # Will show "—" in table
  } else {
    value <- user_config$analysis_choices$TPM_threshold %||% NA
    if (!is.na(value)) round(value) else "N/A"
  }
}

#' Extract Effect Sizes (Clean)
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
create_clean_solutions_table_ui <- function(solutions_data, workflow_info) {

  # Get dynamic column name for optimal parameter
  optimal_col_name <- get_optimal_column_name(workflow_info$minimizing_parameter)

  # Check if workflow includes cost (7 workflows have cost, 4 are power-only)
  has_cost <- !is.null(workflow_info$workflow_id) &&
              !workflow_info$workflow_id %in% c(
                "power_single_cells_per_target", "power_single_reads_per_cell",
                "power_single_TPM_threshold", "power_single_minimum_fold_change"
              )

  # Get minimizing parameter from first solution (all solutions have same workflow)
  minimizing_param <- solutions_data[[1]]$minimizing_param

  # Create two-row header
  header_row1 <- create_header_row1(optimal_col_name, has_cost, minimizing_param)
  header_row2 <- create_header_row2(has_cost, minimizing_param)

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
      style = "margin-bottom: 0; font-size: 12px; width: 100%;",
      tags$thead(
        header_row1,
        header_row2
      ),
      tags$tbody(
        data_rows
      )
    )
  )
}

#' Create Header Row 1 (Main Categories)
create_header_row1 <- function(optimal_col_name, has_cost = NULL, minimizing_param = NULL) {

  # Base rowspan style for single columns that span both header rows
  rowspan_style <- "text-align: center; font-weight: bold; vertical-align: middle; border-bottom: 1px solid #dee2e6; border-right: 1px solid #dee2e6; padding: 12px; background-color: #f8f9fa;"

  cells <- list(
    tags$th("Solution ID", rowspan = "2", style = rowspan_style),
    tags$th("Power", rowspan = "2", style = rowspan_style)
  )

  # Add cost column if workflow includes cost AND cost is not being minimized
  if (has_cost && minimizing_param != "cost") {
    cells <- append(cells, list(
      tags$th("Cost", rowspan = "2", style = rowspan_style)
    ))
  }

  # Add optimal parameter column (single column, rowspan)
  cells <- append(cells, list(
    tags$th(optimal_col_name, rowspan = "2", style = rowspan_style)
  ))

  # Calculate experimental parameters colspan (5 minus any minimized experimental params)
  exp_params_colspan <- 5
  if (minimizing_param == "cells_per_target") exp_params_colspan <- exp_params_colspan - 1
  if (minimizing_param == "reads_per_cell") exp_params_colspan <- exp_params_colspan - 1

  # Add experimental parameters header if any columns remain (multi-column header)
  if (exp_params_colspan > 0) {
    cells <- append(cells, list(
      tags$th("Experimental Parameters",
              colspan = as.character(exp_params_colspan),
              style = "text-align: center; font-weight: bold; border-bottom: 1px solid #dee2e6; border-right: 1px solid #dee2e6; padding: 8px; background-color: #e9ecef;")
    ))
  }

  # Add TPM threshold if not being minimized (two-row structure)
  if (minimizing_param != "TPM_threshold") {
    cells <- append(cells, list(
      tags$th("Analysis Parameter", style = "text-align: center; font-weight: bold; border-bottom: 1px solid #dee2e6; border-right: 1px solid #dee2e6; padding: 8px; background-color: #e9ecef;")
    ))
  }

  # Calculate effect sizes colspan (2 minus fold change if minimized)
  effect_sizes_colspan <- 2
  if (minimizing_param == "minimum_fold_change") effect_sizes_colspan <- effect_sizes_colspan - 1

  # Handle Effect Sizes column based on number of sub-columns
  if (effect_sizes_colspan > 1) {
    # Multi-column header for Effect Sizes (last group, no right border)
    cells <- append(cells, list(
      tags$th("Effect Sizes",
              colspan = as.character(effect_sizes_colspan),
              style = "text-align: center; font-weight: bold; border-bottom: 1px solid #dee2e6; padding: 8px; background-color: #e9ecef;")
    ))
  } else if (effect_sizes_colspan == 1) {
    # Single column header for Effect Sizes (when fold change is minimized, only Non-null Prop remains)
    # Remove right border for last column
    cells <- append(cells, list(
      tags$th("Effect Sizes", rowspan = "2", style = "text-align: center; font-weight: bold; vertical-align: middle; border-bottom: 1px solid #dee2e6; padding: 12px; background-color: #f8f9fa;")
    ))
  }

  tags$tr(cells)
}

#' Create Header Row 2 (Subcolumns)
create_header_row2 <- function(has_cost = NULL, minimizing_param = NULL) {
  # Row 2 only contains sub-columns for multi-column headers
  # Single-row columns (with rowspan=2) don't appear in row 2

  cells <- list()

  # Sub-column style for row 2
  subcolumn_style <- "text-align: center; font-size: 11px; border-top: none; border-bottom: 1px solid #dee2e6; padding: 6px; background-color: #f8f9fa;"

  # Last sub-column in a group style (with right border)
  last_subcolumn_style <- "text-align: center; font-size: 11px; border-top: none; border-bottom: 1px solid #dee2e6; border-right: 1px solid #dee2e6; padding: 6px; background-color: #f8f9fa;"

  # Add experimental parameter subcolumns (only if not being minimized)
  cells <- append(cells, list(
    tags$th("MOI", style = subcolumn_style),
    tags$th("# Targets", style = subcolumn_style),
    tags$th("gRNAs/Target", style = subcolumn_style)
  ))

  # Determine the last experimental parameter column
  exp_params_remaining <- c()
  if (minimizing_param != "cells_per_target") exp_params_remaining <- c(exp_params_remaining, "cells")
  if (minimizing_param != "reads_per_cell") exp_params_remaining <- c(exp_params_remaining, "reads")

  # Add Cells/Target column only if not being minimized
  if (minimizing_param != "cells_per_target") {
    # Use last_subcolumn_style if this is the last experimental parameter
    style_to_use <- if ("reads" %in% exp_params_remaining) subcolumn_style else last_subcolumn_style
    cells <- append(cells, list(
      tags$th("Cells/Target", style = style_to_use)
    ))
  }

  # Add Reads/Cell column only if not being minimized
  if (minimizing_param != "reads_per_cell") {
    # Check if TPM follows this column to determine border style
    style_to_use <- if (minimizing_param != "TPM_threshold") subcolumn_style else last_subcolumn_style
    cells <- append(cells, list(
      tags$th("Reads/Cell", style = style_to_use)
    ))
  }

  # Add TPM Threshold sub-column if not being minimized
  if (minimizing_param != "TPM_threshold") {
    cells <- append(cells, list(
      tags$th("TPM Threshold", style = last_subcolumn_style)
    ))
  }

  # Calculate effect sizes colspan to determine if we need sub-columns
  effect_sizes_colspan <- 2
  if (minimizing_param == "minimum_fold_change") effect_sizes_colspan <- effect_sizes_colspan - 1

  # Only add Effect Sizes sub-columns if there are multiple effect size columns
  if (effect_sizes_colspan > 1) {
    # Add Fold Change column only if not being minimized
    if (minimizing_param != "minimum_fold_change") {
      cells <- append(cells, list(
        tags$th("Fold Change", style = subcolumn_style)
      ))
    }

    # Always add Non-null Prop (never minimized) - last column, no right border
    cells <- append(cells, list(
      tags$th("Non-null Prop", style = "text-align: center; font-size: 11px; border-top: none; border-bottom: 1px solid #dee2e6; padding: 6px; background-color: #f8f9fa;")
    ))
  }
  # If effect_sizes_colspan == 1, Effect Sizes is a single column with rowspan=2, so no sub-columns needed

  tags$tr(cells)
}

#' Create Data Row
create_data_row <- function(solution_data, workflow_info) {

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
  cells <- append(cells, list(
    tags$td(exp_params$moi %||% "N/A", style = "text-align: center; padding: 8px;"),
    tags$td(exp_params$num_targets %||% "N/A", style = "text-align: center; padding: 8px;"),
    tags$td(exp_params$grnas_per_target %||% "N/A", style = "text-align: center; padding: 8px;")
  ))

  # Add Cells/Target column only if not being minimized
  if (solution_data$minimizing_param != "cells_per_target") {
    cells <- append(cells, list(
      tags$td(exp_params$cells_per_target %||% "N/A", style = "text-align: center; padding: 8px;")
    ))
  }

  # Add Reads/Cell column only if not being minimized
  if (solution_data$minimizing_param != "reads_per_cell") {
    cells <- append(cells, list(
      tags$td(exp_params$reads_per_cell %||% "N/A", style = "text-align: center; padding: 8px;")
    ))
  }

  # Add TPM threshold column only if not being minimized
  if (solution_data$minimizing_param != "TPM_threshold") {
    cells <- append(cells, list(
      tags$td(solution_data$TPM_threshold %||% "N/A", style = "text-align: center; padding: 8px;")
    ))
  }

  # Add Fold Change column only if not being minimized
  if (solution_data$minimizing_param != "minimum_fold_change") {
    cells <- append(cells, list(
      tags$td(effect_sizes$fold_change %||% "N/A", style = "text-align: center; padding: 8px;")
    ))
  }

  # Always add Non-null Proportion (never minimized)
  cells <- append(cells, list(
    tags$td(effect_sizes$non_null_proportion %||% "N/A", style = "text-align: center; padding: 8px;")
  ))

  tags$tr(cells)
}

# ============================================================================
# HELPER FUNCTIONS
# ============================================================================

#' Get Optimal Column Name
get_optimal_column_name <- function(minimizing_param) {
  switch(minimizing_param,
    "cells_per_target" = "Optimal Cells/Target",
    "reads_per_cell" = "Optimal Reads/Cell",
    "sequenced_reads_per_cell" = "Optimal Reads/Cell",
    "TPM_threshold" = "Optimal TPM Threshold",
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
