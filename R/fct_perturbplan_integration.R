#' Perturbplan Integration Functions
#'
#' @description Helper functions to integrate UI configuration with perturbplan package
#' functions, specifically cost_power_computation for power-only workflows.
#'
#' @name perturbplan-integration
NULL

#' Extract pilot data from experimental setup configuration
#'
#' @description Extracts baseline expression stats and library parameters
#' from the experimental setup configuration for perturbplan functions.
#' Uses custom gene lists when provided for targeted analysis.
#'
#' @param experimental_config Experimental setup configuration from UI
#' @param analysis_config Analysis configuration containing gene list data (optional)
#' @return List with baseline_expression_stats and library_parameters, or NULL if not available
#' @noRd
extract_pilot_data <- function(experimental_config, analysis_config = NULL) {
  if (is.null(experimental_config)) {
    return(NULL)
  }

  pilot_data <- experimental_config$pilot_data

  # Extract gene list data if available
  gene_list_data <- if (!is.null(analysis_config) &&
                        !is.null(analysis_config$gene_list_data) &&
                        analysis_config$gene_list_data$type == "custom" &&
                        analysis_config$gene_list_data$is_valid) {
    analysis_config$gene_list_data$data$response_id
  } else {
    NULL  # Random sampling
  }

  tryCatch({
    if (is.null(pilot_data) || pilot_data$type == "default") {
      # Use built-in data for the selected biological system
      biological_system <- experimental_config$biological_system %||% "K562"

      # Use extract_expression_info to process built-in data
      expression_info <- perturbplan::extract_expression_info(
        biological_system = biological_system,
        B = 1000,                    # Sample 1000 genes
        gene_list = gene_list_data,  # Use custom gene list if provided
        TPM_threshold = 0,           # No TPM filtering at this stage
        custom_pilot_data = NULL     # Use built-in data
      )

      # Return pilot data with processed baseline_expression_stats
      return(list(
        baseline_expression_stats = expression_info$expression_df,  # Processed/sampled data
        library_parameters = expression_info$pilot_data$library_parameters
      ))

    } else if (pilot_data$type == "custom") {
      # Load and validate custom pilot data from uploaded file
      if (!file.exists(pilot_data$file_path)) {
        return(NULL)
      }

      uploaded_data <- readRDS(pilot_data$file_path)

      # Validate the structure using our validation function
      validation_result <- validate_custom_pilot_data(uploaded_data, pilot_data$file_name)

      if (!validation_result$valid) {
        message("Custom pilot data validation failed: ",
                paste(validation_result$errors, collapse = "; "))
        return(NULL)
      }

      # Use extract_expression_info to process custom data
      expression_info <- perturbplan::extract_expression_info(
        biological_system = "K562",           # Not used when custom_pilot_data provided
        B = 1000,                            # Sample 1000 genes
        gene_list = gene_list_data,          # Use custom gene list if provided
        TPM_threshold = 0,                   # No TPM filtering at this stage
        custom_pilot_data = validation_result$data  # Use validated custom data
      )

      # Return pilot data with processed baseline_expression_stats
      return(list(
        baseline_expression_stats = expression_info$expression_df,  # Processed/sampled data
        library_parameters = expression_info$pilot_data$library_parameters
      ))
    }

  }, error = function(e) {
    message("Error processing pilot data: ", e$message)
    return(NULL)
  })

  return(NULL)
}

#' Map UI configuration to perturbplan::cost_power_computation parameters
#'
#' @description Converts the UI configuration structure to parameters
#' expected by perturbplan::cost_power_computation function.
#'
#' @param config Complete UI configuration from sidebar modules
#' @param workflow_info Detected workflow information
#' @param pilot_data Pre-extracted pilot data to avoid duplicate extraction
#' @return Named list of parameters for cost_power_computation
#' @noRd
map_config_to_perturbplan_params <- function(config, workflow_info, pilot_data) {
  # DEBUG: Log what the UI is sending
  if (!is.null(config$design_options$parameter_controls)) {
    for (name in names(config$design_options$parameter_controls)) {
    }
  }

  # Use pre-extracted pilot data (passed as parameter to avoid duplication)

  # Extract design options
  design_opts <- config$design_options
  experimental_opts <- config$experimental_setup
  analysis_opts <- config$analysis_choices
  effect_opts <- config$effect_sizes
  advanced_opts <- config$advanced_choices

  # Determine minimizing variable from workflow
  minimizing_variable <- workflow_info$minimizing_parameter


  # minimizing_variable is already standardized by centralized translation in mod_sidebar.R
  # No additional mapping needed

  # Ensure we have the correct parameter name
  valid_minimizing_params <- c("TPM_threshold", "minimum_fold_change", "cells_per_target", "reads_per_cell", "cost")
  if (!minimizing_variable %in% valid_minimizing_params) {
    stop(sprintf("`minimizing_variable` must be one of: %s! Received: '%s'", paste(valid_minimizing_params, collapse = ", "), minimizing_variable))
  }

  # Build fixed_variable list
  fixed_variable <- list()

  # Get parameter controls from design options
  param_controls <- design_opts$parameter_controls


  if (!is.null(param_controls)) {
    # Default values to use when fixed_value is NULL but type is "fixed"
    default_values <- list(
      cells_per_target = 1000,
      reads_per_cell = 5000,
      TPM_threshold = 10,
      minimum_fold_change = 0.8
    )

    # Add fixed parameters based on their type and actual config values (including slider overrides)
    if (!is.null(param_controls$cells_per_target) &&
        param_controls$cells_per_target$type == "fixed") {
      # Use actual config value (includes slider overrides), fallback to default
      value <- experimental_opts$cells_fixed %||% default_values$cells_per_target
      fixed_variable$cells_per_target <- round(value)
    }

    if (!is.null(param_controls$reads_per_cell) &&
        param_controls$reads_per_cell$type == "fixed") {
      # Get sequenced reads per cell value (includes slider overrides), fallback to default
      sequenced_reads_per_cell <- experimental_opts$reads_per_cell_fixed %||% default_values$reads_per_cell

      # Apply mapping efficiency transformation: reads_per_cell = sequenced_reads_per_cell * mapping_efficiency
      mapping_efficiency <- advanced_opts$mapping_efficiency %||% 0.72
      reads_per_cell <- sequenced_reads_per_cell * mapping_efficiency

      fixed_variable$reads_per_cell <- round(reads_per_cell)
    }

    if (!is.null(param_controls$TPM_threshold) &&
        param_controls$TPM_threshold$type == "fixed") {
      # Use actual config value (includes slider overrides), fallback to default
      value <- analysis_opts$TPM_threshold_fixed %||% default_values$TPM_threshold
      fixed_variable$TPM_threshold <- value
    }

    if (!is.null(param_controls$minimum_fold_change) &&
        param_controls$minimum_fold_change$type == "fixed") {
      # Use actual config value (includes slider overrides), fallback to default
      value <- effect_opts$minimum_fold_change_fixed %||% default_values$minimum_fold_change
      fixed_variable$minimum_fold_change <- value
    }
  }


  # POWER+COST WORKFLOW LOGIC: Calculate missing parameter using cost constraints
  if (!is.null(config$design_options$optimization_type) &&
      config$design_options$optimization_type == "power_cost") {

    # Check if we have a power+cost workflow with one cells/reads parameter fixed
    has_cells_fixed <- !is.null(fixed_variable$cells_per_target)
    has_reads_fixed <- !is.null(fixed_variable$reads_per_cell)

    # Only apply if exactly one of cells/reads is fixed (our target workflows)
    if ((has_cells_fixed && !has_reads_fixed) || (!has_cells_fixed && has_reads_fixed)) {

      # Extract cost parameters
      cost_constraint <- design_opts$cost_budget %||% 1000  # Default budget
      cost_per_cell <- design_opts$cost_per_cell %||% 0.086  # Default cost per cell
      cost_per_million_reads <- design_opts$cost_per_million_reads %||% 0.374  # Default cost per million reads


      # Call obtain_fixed_variable_constraining_cost to calculate the missing parameter
      tryCatch({
        cost_result <- perturbplan::obtain_fixed_variable_constraining_cost(
          cost_per_captured_cell = cost_per_cell,
          cost_per_million_reads = cost_per_million_reads,
          cost_constraint = cost_constraint,
          MOI = experimental_opts$MOI %||% 10,
          num_targets = experimental_opts$num_targets %||% 100,
          non_targeting_gRNAs = experimental_opts$non_targeting_gRNAs %||% 10,
          gRNAs_per_target = experimental_opts$gRNAs_per_target %||% 4,
          reads_per_cell = if(has_reads_fixed) fixed_variable$reads_per_cell else NULL,
          cells_per_target = if(has_cells_fixed) fixed_variable$cells_per_target else NULL,
          mapping_efficiency = config$experimental_setup$mapping_efficiency %||% 0.72
        )

        # Add the calculated parameter to fixed_variable (round to integers as required by perturbplan)
        if (has_cells_fixed && !has_reads_fixed) {
          fixed_variable$reads_per_cell <- round(cost_result$reads_per_cell)
        } else if (has_reads_fixed && !has_cells_fixed) {
          fixed_variable$cells_per_target <- round(cost_result$cells_per_target)
        }

      }, error = function(e) {
        stop("Cost constraint calculation failed: ", e$message)
      })

    }
  }

  # Map analysis choices
  side_mapping <- c("left" = "left", "right" = "right", "both" = "both")
  control_mapping <- c("complement" = "complement", "nt_cells" = "nt_cells")

  # DEBUG: Log what will be passed to perturbplan
  if (length(fixed_variable) > 0) {
    for (name in names(fixed_variable)) {
    }
  } else {
  }

  # Build parameter list
  params <- list(
    minimizing_variable = minimizing_variable,
    fixed_variable = fixed_variable,

    # Experimental parameters
    MOI = experimental_opts$MOI %||% 10,
    num_targets = experimental_opts$num_targets %||% 100,
    non_targeting_gRNAs = experimental_opts$non_targeting_gRNAs %||% 10,
    gRNAs_per_target = experimental_opts$gRNAs_per_target %||% 4,

    # Effect size parameters
    gRNA_variability = advanced_opts$gRNA_variability %||% 0.15,
    prop_non_null = effect_opts$prop_non_null %||% 0.1,

    # Analysis parameters
    control_group = control_mapping[advanced_opts$control_group %||% "complement"],
    side = side_mapping[analysis_opts$side %||% "left"],
    multiple_testing_alpha = advanced_opts$fdr_target %||% 0.1,

    # Power and cost parameters
    power_target = design_opts$target_power %||% 0.8,

    # Cost constraint logic:
    # - For the 4 specific power+cost workflows, cost_constraint should be NULL
    #   because the constraint was already applied via obtain_fixed_variable_constraining_cost
    # - For other power+cost workflows, use the budget
    cost_constraint = NULL,  # Set to NULL for all workflows - cost constraint already applied if needed
    cost_per_captured_cell = design_opts$cost_per_cell %||% 0.086,
    cost_per_million_reads = design_opts$cost_per_million_reads %||% 0.374,

    # Grid parameters - reduced for better performance in single-parameter optimization
    grid_size = 15,

    # Mapping efficiency (from advanced settings)
    mapping_efficiency = advanced_opts$mapping_efficiency %||% 0.72,

    # Pilot data
    baseline_expression_stats = pilot_data$baseline_expression_stats,
    library_parameters = pilot_data$library_parameters
  )

  return(params)
}

#' Standardize perturbplan results to app format
#'
#' @description Converts perturbplan::cost_power_computation results
#' to the standardized format expected by the plotting modules.
#'
#' @param results Results from perturbplan::cost_power_computation
#' @param config UI configuration
#' @param workflow_info Detected workflow information
#' @return Standardized results list matching placeholder format
#' @noRd
standardize_perturbplan_results <- function(results, config, workflow_info) {
  if (is.null(results) || nrow(results) == 0) {
    return(list(
      error = "No results returned from perturbplan analysis",
      metadata = list(
        analysis_mode = "Real Analysis (Empty Results)",
        timestamp = Sys.time()
      )
    ))
  }

  # Convert perturbplan results to our standardized format
  standardized <- list(
    # Raw data from perturbplan
    data = results,

    # Analysis metadata
    metadata = list(
      analysis_mode = "Real Analysis (perturbplan Package)",
      workflow_scenario = workflow_info$scenario,
      plot_type = workflow_info$plot_type,
      minimizing_parameter = workflow_info$minimizing_parameter,
      optimization_type = config$design_options$optimization_type,
      cost_constraint = if (config$design_options$optimization_type == "power_cost")
                         config$design_options$cost_budget else NULL,
      cost_per_cell = if (config$design_options$optimization_type == "power_cost")
                       config$design_options$cost_per_cell %||% 0.086 else NULL,
      cost_per_million_reads = if (config$design_options$optimization_type == "power_cost")
                                config$design_options$cost_per_million_reads %||% 0.374 else NULL,
      timestamp = Sys.time(),
      n_rows = nrow(results),
      parameter_ranges = extract_parameter_ranges_from_results(results)
    ),

    # Summary statistics
    summary = create_perturbplan_results_summary(results, workflow_info),

    # Validation
    validation = list(
      is_valid = TRUE,
      has_data = nrow(results) > 0,
      has_power_curves = "overall_power" %in% names(results),
      meets_power_target = any(results$overall_power >= (config$design_options$target_power %||% 0.8), na.rm = TRUE)
    )
  )

  return(standardized)
}

#' Extract parameter ranges from perturbplan results
#'
#' @param results Data frame from perturbplan
#' @return List of parameter ranges
#' @noRd
extract_parameter_ranges_from_results <- function(results) {
  ranges <- list()

  # Extract ranges for all numeric columns that represent parameters
  param_cols <- c("TPM_threshold", "minimum_fold_change", "cells_per_target", "sequenced_reads_per_cell")

  for (col in param_cols) {
    if (col %in% names(results) && is.numeric(results[[col]])) {
      ranges[[col]] <- range(results[[col]], na.rm = TRUE)
    }
  }

  return(ranges)
}

#' Create summary from perturbplan results
#'
#' @param results Data frame from perturbplan
#' @param workflow_info Workflow information
#' @return Summary list
#' @noRd
create_perturbplan_results_summary <- function(results, workflow_info) {
  if (nrow(results) == 0) {
    return(list(message = "No valid results generated"))
  }

  # Find optimal design (highest power)
  if ("overall_power" %in% names(results)) {
    optimal_idx <- which.max(results$overall_power)
    optimal <- results[optimal_idx, ]

    # Base optimal design info
    # Map minimizing parameter names to actual data field names
    param_field_mapping <- list(
      "reads_per_cell" = "sequenced_reads_per_cell",
      "cells_per_target" = "cells_per_target",
      "TPM_threshold" = "TPM_threshold",
      "minimum_fold_change" = "minimum_fold_change"
    )

    actual_field <- param_field_mapping[[workflow_info$minimizing_parameter]] %||% workflow_info$minimizing_parameter

    optimal_design <- list(
      parameter_value = optimal[[actual_field]],
      achieved_power = optimal$overall_power,
      cells_per_target = optimal$cells_per_target %||% NA,
      sequenced_reads_per_cell = optimal$sequenced_reads_per_cell %||% NA
    )

    # Add cost information if available
    if ("total_cost" %in% names(optimal)) {
      optimal_design$total_cost <- optimal$total_cost
    }
    if ("cost_per_cell" %in% names(optimal)) {
      optimal_design$cost_per_cell <- optimal$cost_per_cell
    }
    if ("cost_per_million_reads" %in% names(optimal)) {
      optimal_design$cost_per_million_reads <- optimal$cost_per_million_reads
    }

    # Add parameter-specific field names for plotting compatibility (FINAL STEP)
    # When minimizing reads_per_cell, plotting code expects reads_per_cell field
    if (workflow_info$minimizing_parameter == "reads_per_cell") {
      optimal_design$reads_per_cell <- optimal$sequenced_reads_per_cell %||% NA
    }

    summary <- list(
      optimal_design = optimal_design,
      power_range = range(results$overall_power, na.rm = TRUE),
      n_designs = nrow(results),
      feasible_designs = sum(results$overall_power >= 0.05, na.rm = TRUE)
    )

    # Add cost range if cost column exists
    if ("total_cost" %in% names(results)) {
      summary$cost_range <- range(results$total_cost, na.rm = TRUE)
    }
  } else {
    summary <- list(
      message = "Power data not available in results",
      n_designs = nrow(results)
    )
  }

  return(summary)
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

#' Transform perturbplan results to plotting format
#'
#' @description Converts standardized perturbplan results to the format
#' expected by the plotting modules (power_data, optimal_design, etc.)
#'
#' @param standardized_results Results from standardize_perturbplan_results()
#' @param config UI configuration from sidebar modules
#' @param workflow_info Detected workflow information
#' @return List with plotting-compatible format (power_data, optimal_design, etc.)
#' @noRd
transform_perturbplan_to_plotting_format <- function(standardized_results, config, workflow_info) {

  # Return error if standardized results contain error
  if (!is.null(standardized_results$error)) {
    return(standardized_results)
  }

  # Extract raw perturbplan data
  raw_data <- standardized_results$data

  if (is.null(raw_data) || nrow(raw_data) == 0) {
    return(list(
      error = "No data available from perturbplan analysis",
      metadata = list(
        analysis_mode = "Real Analysis (Empty Data)",
        timestamp = Sys.time()
      )
    ))
  }

  # Get the correct parameter column name
  minimizing_param <- workflow_info$minimizing_parameter
  param_column <- get_parameter_column_name(minimizing_param)

  # Validate that required columns exist
  required_cols <- c("overall_power", param_column)
  missing_cols <- setdiff(required_cols, names(raw_data))

  if (length(missing_cols) > 0) {
    return(list(
      error = sprintf("Missing required columns in perturbplan results: %s",
                     paste(missing_cols, collapse = ", ")),
      metadata = list(
        analysis_mode = "Real Analysis (Missing Columns)",
        timestamp = Sys.time()
      )
    ))
  }

  # Create power_data in plotting format
  target_power <- config$design_options$target_power %||% 0.8

  power_data <- data.frame(
    parameter_value = raw_data[[param_column]],
    power = raw_data$overall_power,
    meets_threshold = raw_data$overall_power >= target_power,
    stringsAsFactors = FALSE
  )

  # Add cost data if available (for power+cost workflows)
  if ("total_cost" %in% names(raw_data)) {
    power_data$cost = raw_data$total_cost
  }

  # Find optimal design based on parameter type
  feasible_designs <- raw_data[raw_data$overall_power >= target_power, ]

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
    optimal_idx <- which.max(raw_data$overall_power)
    optimal_row <- raw_data[optimal_idx, ]
  }

  optimal_design <- list(
    parameter_value = optimal_row[[param_column]],
    achieved_power = optimal_row$overall_power,
    cells_per_target = optimal_row$cells_per_target %||% NA,
    sequenced_reads_per_cell = optimal_row$sequenced_reads_per_cell %||% NA,
    TPM_threshold = optimal_row$TPM_threshold %||% NA,
    minimum_fold_change = optimal_row$minimum_fold_change %||% NA,
    total_cost = optimal_row$total_cost %||% NA,
    # Add mapping efficiency used in the analysis (from advanced choices)
    mapping_efficiency = config$advanced_choices$mapping_efficiency %||% 0.72,
    # Add cost calculation metadata for power+cost workflows
    is_cost_optimized = !is.null(config$design_options$optimization_type) &&
                        config$design_options$optimization_type == "power_cost",
    cost_constraint = if (!is.null(config$design_options$optimization_type) &&
                          config$design_options$optimization_type == "power_cost")
                        config$design_options$cost_budget else NULL,
    cost_per_cell = if (!is.null(config$design_options$optimization_type) &&
                        config$design_options$optimization_type == "power_cost")
                      config$design_options$cost_per_cell %||% 0.086 else NULL,
    cost_per_million_reads = if (!is.null(config$design_options$optimization_type) &&
                                 config$design_options$optimization_type == "power_cost")
                               config$design_options$cost_per_million_reads %||% 0.374 else NULL
  )

  # Add parameter-specific field names for plotting compatibility (CRITICAL for reads_per_cell)
  # When minimizing reads_per_cell, plotting code expects reads_per_cell field
  if (workflow_info$minimizing_parameter == "reads_per_cell") {
    optimal_design$reads_per_cell <- optimal_design$sequenced_reads_per_cell %||% NA
  }

  # Create enriched workflow_info for plotting
  enriched_workflow_info <- workflow_info
  # Preserve the original plot_type from workflow detection (don't override for power+cost workflows)
  # enriched_workflow_info$plot_type is already set correctly by detect_workflow_scenario()
  enriched_workflow_info$title <- create_workflow_title(workflow_info$minimizing_parameter)
  enriched_workflow_info$description <- create_workflow_description(workflow_info$minimizing_parameter, target_power)

  # Extract parameter ranges from real data
  parameter_ranges <- list()
  parameter_ranges[[minimizing_param]] <- range(power_data$parameter_value, na.rm = TRUE)

  # Create plotting-compatible results structure
  plotting_results <- list(
    # Core plotting data (matches existing plotting module expectations)
    power_data = power_data,
    optimal_design = optimal_design,

    # Workflow and user configuration
    workflow_info = enriched_workflow_info,
    user_config = config,

    # Parameter information
    parameter_ranges = parameter_ranges,

    # Raw data for detailed tables/export
    raw_perturbplan_data = raw_data,

    # Metadata
    metadata = list(
      analysis_mode = "Real Analysis (perturbplan)",
      data_source = "perturbplan::cost_power_computation",
      n_designs_evaluated = nrow(raw_data),
      n_designs_meeting_target = sum(power_data$meets_threshold),
      timestamp = Sys.time()
    )
  )

  return(plotting_results)
}

#' Create workflow title for plotting
#'
#' @param minimizing_param The parameter being minimized
#' @return Character string with plot title
#' @noRd
create_workflow_title <- function(minimizing_param) {
  switch(minimizing_param,
    "cells_per_target" = "Power Optimization: Minimize Cells per Target",
    "reads_per_cell" = "Power Optimization: Minimize Reads per Cell",
    "TPM_threshold" = "Power Optimization: Minimize TPM Threshold",
    "minimum_fold_change" = "Power Optimization: Minimize Fold Change",
    paste("Power Optimization: Minimize", minimizing_param)
  )
}

#' Create workflow description for plotting
#'
#' @param minimizing_param The parameter being minimized
#' @param target_power Target power level (e.g., 0.8)
#' @return Character string with plot description
#' @noRd
create_workflow_description <- function(minimizing_param, target_power) {
  power_pct <- scales::percent_format()(target_power)

  switch(minimizing_param,
    "cells_per_target" = sprintf("Finding minimum cells needed to achieve %s power", power_pct),
    "reads_per_cell" = sprintf("Finding minimum reads needed to achieve %s power", power_pct),
    "TPM_threshold" = sprintf("Finding minimum TPM threshold to achieve %s power", power_pct),
    "minimum_fold_change" = sprintf("Finding minimum fold change to achieve %s power", power_pct),
    sprintf("Finding minimum %s to achieve %s power", minimizing_param, power_pct)
  )
}
