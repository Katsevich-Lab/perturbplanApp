#' Univariate Optimization Functions
#'
#' @description Functions for univariate optimization workflows (workflows 1-4, 6-9)
#' that optimize a single parameter while keeping others fixed.
#'
#' @name univariate-optimization
NULL

#' Perform standard analysis for workflows 1-4, 6-9
#'
#' @description Unified analysis function for 8 standard workflows that use
#' perturbplan::cost_power_computation. Returns plotting-ready format directly.
#'
#' @param config User configuration from sidebar modules
#' @param pilot_data Extracted pilot data for perturbplan
#'
#' @return List containing plotting-ready results (same structure as specialized workflows)
#' @noRd
perform_standard_analysis <- function(config, pilot_data) {

  # Map UI configuration to perturbplan::cost_power_computation parameters
  workflow_info <- config$workflow_info
  perturbplan_params <- map_config_to_perturbplan_params(config, workflow_info, pilot_data)

  # Call perturbplan::cost_power_computation
  results <- do.call(perturbplan::cost_power_computation, perturbplan_params)

  # Standardize perturbplan output column names to sequenced_reads_per_cell
  results$sequenced_reads_per_cell <- results$raw_reads_per_cell
  results$raw_reads_per_cell <- NULL

  # Get the correct parameter column name
  minimizing_param <- workflow_info$minimizing_parameter
  param_column <- get_parameter_column_name(minimizing_param)

  # Get assay type for TPM→Expression transformation
  assay_type <- config$design_options$assay_type

  # Transform parameter values for display if minimizing TPM_threshold
  if (minimizing_param == "TPM_threshold") {
    parameter_values <- sapply(results$TPM_threshold, function(tpm) {
      transform_TPM_to_Expression(tpm, assay_type, pilot_data)
    })
  } else {
    parameter_values <- results[[param_column]]
  }

  # Create power_data in plotting format
  target_power <- config$design_options$target_power
  power_data <- data.frame(
    parameter_value = parameter_values,
    power = results$overall_power,
    meets_threshold = results$overall_power >= target_power,
    stringsAsFactors = FALSE
  )

  # Add cost data if available (for power+cost workflows)
  if ("total_cost" %in% names(results)) {
    power_data$cost = results$total_cost
  }

  # Find optimal design based on parameter type
  feasible_designs <- results[results$overall_power >= target_power, ]

  if (nrow(feasible_designs) > 0) {
    # For fold change: find value closest to 1 among feasible designs
    # For other parameters: find MINIMUM value among feasible designs
    if (minimizing_param == "minimum_fold_change") {
      # Find fold change closest to 1 (minimum absolute distance from 1)
      distances_from_1 <- abs(feasible_designs[[param_column]] - 1)
      optimal_idx <- which.min(distances_from_1)
    } else {
      optimal_idx <- which.min(feasible_designs[[param_column]])
    }
    optimal_row <- feasible_designs[optimal_idx, ]
  } else {
    # No feasible design meets power target, return design with highest power
    optimal_idx <- which.max(results$overall_power)
    optimal_row <- results[optimal_idx, ]
  }

  # Transform TPM_threshold to Expression_threshold for display
  expression_threshold_display <- transform_TPM_to_Expression(
    optimal_row$TPM_threshold,
    assay_type,
    pilot_data
  )

  # Create optimal design in plotting format
  optimal_design <- list(
    parameter_value = if (minimizing_param == "TPM_threshold") {
      expression_threshold_display
    } else {
      optimal_row[[param_column]]
    },
    achieved_power = optimal_row$overall_power,
    cells_per_target = optimal_row$cells_per_target %||% NA,
    sequenced_reads_per_cell = optimal_row$sequenced_reads_per_cell %||% NA,
    TPM_threshold = optimal_row$TPM_threshold %||% NA,
    Expression_threshold = expression_threshold_display,
    minimum_fold_change = optimal_row$minimum_fold_change %||% NA,
    total_cost = optimal_row$total_cost %||% NA
  )

  # Add parameter-specific field names for plotting compatibility (CRITICAL for reads_per_cell)
  # When minimizing reads_per_cell, plotting code expects reads_per_cell field
  if (workflow_info$minimizing_parameter == "reads_per_cell") {
    optimal_design$reads_per_cell <- optimal_design$sequenced_reads_per_cell %||% NA
  }

  # workflow_info already contains all needed information for plotting
  enriched_workflow_info <- workflow_info

  # Create plotting-compatible results structure (same format as specialized workflows)
  plotting_results <- list(
    # Core plotting data (matches existing plotting module expectations)
    power_data = power_data,
    optimal_design = optimal_design,
    user_config = config,  # Contains workflow_info already
    exporting_data = create_exporting_data(results, config, workflow_info, pilot_data),
    pilot_data = pilot_data
  )

  return(plotting_results)
}

#' Get parameter column name from perturbplan results
#'
#' @description Maps minimizing parameter names to actual column names
#' returned by perturbplan::cost_power_computation
#'
#' @param minimizing_param The parameter being minimized
#' @return Character string of actual column name in perturbplan results
#' @noRd
get_parameter_column_name <- function(minimizing_param) {
  switch(minimizing_param,
    "cells_per_target" = "cells_per_target",
    "reads_per_cell" = "sequenced_reads_per_cell",  # standardized column name
    "TPM_threshold" = "TPM_threshold",
    "minimum_fold_change" = "minimum_fold_change",
    minimizing_param  # fallback to original name
  )
}

