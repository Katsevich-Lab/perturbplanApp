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
#'
#' @param experimental_config Experimental setup configuration from UI
#' @return List with baseline_expression_stats and library_parameters, or NULL if not available
#' @noRd
extract_pilot_data <- function(experimental_config) {
  if (is.null(experimental_config)) {
    return(NULL)
  }

  pilot_data <- experimental_config$pilot_data

  tryCatch({
    if (is.null(pilot_data) || pilot_data$type == "default") {
      # Use built-in data for the selected biological system
      biological_system <- experimental_config$biological_system %||% "K562"

      # Use extract_expression_info to process built-in data
      expression_info <- perturbplan::extract_expression_info(
        biological_system = biological_system,
        B = 1000,                    # Sample 1000 genes
        gene_list = NULL,            # Random sampling
        TPM_threshold = 0,           # No TPM filtering
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
        gene_list = NULL,                    # Random sampling
        TPM_threshold = 0,                   # No TPM filtering
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
#' @return Named list of parameters for cost_power_computation
#' @noRd
map_config_to_perturbplan_params <- function(config, workflow_info) {
  # Extract pilot data
  pilot_data <- extract_pilot_data(config$experimental_setup)

  # Extract design options
  design_opts <- config$design_options
  experimental_opts <- config$experimental_setup
  analysis_opts <- config$analysis_choices
  effect_opts <- config$effect_sizes

  # Determine minimizing variable from workflow
  minimizing_variable <- workflow_info$minimizing_parameter
  if (minimizing_variable == "min_fold_change") {
    minimizing_variable <- "minimum_fold_change"
  }
  if (minimizing_variable == "TPM_threshold") {
    minimizing_variable <- "TPM_threshold"
  }

  # Build fixed_variable list
  fixed_variable <- list()

  # Get parameter controls from design options
  param_controls <- design_opts$parameter_controls

  if (!is.null(param_controls)) {
    # Add fixed parameters based on their type and fixed values
    if (!is.null(param_controls$cells_per_target) &&
        param_controls$cells_per_target$type == "fixed" &&
        !is.null(experimental_opts$cells_fixed)) {
      fixed_variable$cells_per_target <- experimental_opts$cells_fixed
    }

    if (!is.null(param_controls$reads_per_cell) &&
        param_controls$reads_per_cell$type == "fixed" &&
        !is.null(experimental_opts$reads_fixed)) {
      fixed_variable$reads_per_cell <- experimental_opts$reads_fixed
    }

    if (!is.null(param_controls$TPM_threshold) &&
        param_controls$TPM_threshold$type == "fixed" &&
        !is.null(analysis_opts$TPM_threshold)) {
      fixed_variable$TPM_threshold <- analysis_opts$TPM_threshold
    }

    if (!is.null(param_controls$min_fold_change) &&
        param_controls$min_fold_change$type == "fixed" &&
        !is.null(effect_opts$minimum_fold_change)) {
      fixed_variable$minimum_fold_change <- effect_opts$minimum_fold_change
    }
  }

  # Map analysis choices
  side_mapping <- c("left" = "left", "right" = "right", "both" = "both")
  control_mapping <- c("complement" = "complement", "nt_cells" = "non_targeting")

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
    gRNA_variability = effect_opts$fc_sd %||% 0.15,
    prop_non_null = effect_opts$prop_non_null %||% 0.1,

    # Analysis parameters
    control_group = control_mapping[analysis_opts$control_group %||% "complement"],
    side = side_mapping[analysis_opts$side %||% "left"],
    multiple_testing_alpha = analysis_opts$fdr_target %||% 0.05,

    # Power and cost parameters for power-only workflows
    power_target = design_opts$target_power %||% 0.8,
    cost_constraint = NULL, # Power-only = no cost constraint
    
    # Grid parameters
    grid_size = 100,

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
  param_cols <- c("TPM_threshold", "minimum_fold_change", "cells_per_target", "reads_per_cell")

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

    summary <- list(
      optimal_design = list(
        parameter_value = optimal[[workflow_info$minimizing_parameter]],
        achieved_power = optimal$overall_power,
        cells_per_target = optimal$cells_per_target %||% NA,
        reads_per_cell = optimal$reads_per_cell %||% NA
      ),
      power_range = range(results$overall_power, na.rm = TRUE),
      n_designs = nrow(results),
      feasible_designs = sum(results$overall_power >= 0.05, na.rm = TRUE)
    )
  } else {
    summary <- list(
      message = "Power data not available in results",
      n_designs = nrow(results)
    )
  }

  return(summary)
}
