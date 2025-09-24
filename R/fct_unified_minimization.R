#' Unified Constrained Minimization Analysis Functions
#'
#' @description Unified implementation for workflows 10-11 (TPM and FC minimization)
#' using cost_power_computation + find_optimal_cost_design with consistent data handling.
#'
#' @name unified-minimization
NULL

#' Perform unified constrained minimization analysis (Workflows 10-11)
#'
#' @description Implements both TPM and FC minimization workflows using a single
#' unified approach with consistent cost constraint handling and optimal solution extraction.
#'
#' @param config User configuration from sidebar
#' @param workflow_info Detected workflow information
#' @param pilot_data Pre-extracted pilot data to avoid duplication
#' @return List with power_data and optimal_design compatible with plotting
#' @noRd
perform_constrained_minimization_analysis <- function(config, workflow_info, pilot_data) {

  # Step 1: Detect minimization configuration
  minimization_config <- get_minimization_config(workflow_info$workflow_id)

  # Step 2: Extract cost constraint from user (always use config$design_options$cost_budget for workflows 10-11)
  cost_constraint <- config$design_options$cost_budget

  # Step 3: Get comprehensive parameters from UI using existing mapping function
  perturbplan_params <- map_config_to_perturbplan_params(config, workflow_info, pilot_data)

  # Step 4: Configure for minimization workflow
  perturbplan_params$minimizing_variable <- minimization_config$variable
  perturbplan_params$cost_constraint <- as.numeric(cost_constraint)
  perturbplan_params$grid_size <- 30  # Balance between accuracy and performance for TPM/FC minimization

  # Step 5: Set fixed variable based on minimization type
  if (minimization_config$variable == "TPM_threshold") {
    # TPM minimization: fix fold change, let TPM vary
    fc_value <- config$effect_sizes$minimum_fold_change_fixed
    perturbplan_params$fixed_variable$minimum_fold_change <- fc_value
    perturbplan_params$fixed_variable$TPM_threshold <- NULL
  } else {
    # FC minimization: fix TPM, let fold change vary
    TPM_value <- config$analysis_choices$TPM_threshold_fixed
    perturbplan_params$fixed_variable$TPM_threshold <- as.numeric(TPM_value)
    perturbplan_params$fixed_variable$minimum_fold_change <- NULL
  }

  # Always allow cells and reads to vary for cost optimization
  perturbplan_params$fixed_variable$cells_per_target <- NULL
  perturbplan_params$fixed_variable$reads_per_cell <- NULL

  # Step 6: Call cost_power_computation to get power-cost grid
  cost_power_grid <- do.call(perturbplan::cost_power_computation, perturbplan_params)

  # Step 7: Call find_optimal_cost_design with all required parameters
  find_optimal_params <- list(
    cost_power_df = cost_power_grid,
    minimizing_variable = minimization_config$variable,
    power_target = config$design_options$target_power,
    power_precision = 0.02,
    MOI = perturbplan_params$MOI,
    num_targets = perturbplan_params$num_targets,
    non_targeting_gRNAs = perturbplan_params$non_targeting_gRNAs,
    gRNAs_per_target = perturbplan_params$gRNAs_per_target,
    cost_per_captured_cell = perturbplan_params$cost_per_captured_cell,
    cost_per_million_reads = perturbplan_params$cost_per_million_reads,
    cost_grid_size = 200
  )

  optimal_results <- do.call(perturbplan::find_optimal_cost_design, find_optimal_params)

  # Step 8: Apply consistent data grouping for both plotting and optimal solution
  power_data <- optimal_results$optimal_cost_power_df

  # Standardize column names in power_data for UI compatibility
  power_data$sequenced_reads_per_cell <- power_data$raw_reads_per_cell
  power_data$raw_reads_per_cell <- NULL

  # Group by minimizing variable and select minimum cost for each value
  grouped_data <- power_data %>%
    dplyr::group_by(.data[[minimization_config$variable]]) %>%
    dplyr::slice_min(total_cost, n = 1, with_ties = FALSE) %>%
    dplyr::ungroup()

  # Step 9: Find optimal solution using the same grouped data
  optimal_point <- find_optimal_point_in_grouped_data(
    grouped_data,
    minimization_config,
    cost_constraint)

  # Step 9b: Create properly formatted optimal design object
  optimal_design <- list(
    cells_per_target = optimal_point$cells_per_target,
    sequenced_reads_per_cell = optimal_point$sequenced_reads_per_cell,
    total_cost = optimal_point$total_cost,
    achieved_power = optimal_point$overall_power,  # Map overall_power to achieved_power
    optimal_minimized_param = optimal_point[[minimization_config$variable]],  # The minimized parameter value
    mapping_efficiency = config$experimental_setup$mapping_efficiency %||% 0.72  # Use user input or standard default
  )

  # Add the minimizing parameter to the optimal design
  optimal_design[[minimization_config$variable]] <- optimal_point[[minimization_config$variable]]

  # Standardize column names in cost_data for UI compatibility
  cost_data <- optimal_results$optimal_cost_grid
  cost_data$sequenced_reads_per_cell <- cost_data$raw_reads_per_cell
  cost_data$raw_reads_per_cell <- NULL

  # Step 10: Return unified results
  final_results <- list(
    power_data = grouped_data,  # Grouped data for consistent plotting
    cost_data = cost_data,  # For equi-cost curves
    optimal_design = optimal_design,
    user_config = config,
    workflow_info = workflow_info,
    exporting_data = create_exporting_data(cost_power_grid, config, workflow_info, pilot_data),
    pilot_data = pilot_data
  )

  return(final_results)
}

#' Get minimization configuration for workflow
#'
#' @param workflow_id Workflow identifier
#' @return List with variable and optimization goal
#' @noRd
get_minimization_config <- function(workflow_id) {
  switch(workflow_id,
    "power_cost_TPM_cells_reads" = list(
      variable = "TPM_threshold",
      optimization_goal = "minimize"
    ),
    "power_cost_fc_cells_reads" = list(
      variable = "minimum_fold_change",
      optimization_goal = "closest_to_one"
    ),
    stop("Unknown minimization workflow: ", workflow_id)
  )
}

#' Find optimal point in grouped data
#'
#' @param grouped_data Data grouped by minimizing variable
#' @param minimization_config Configuration for minimization
#' @param cost_constraint User's cost budget
#' @return Single row with optimal design
#' @noRd
find_optimal_point_in_grouped_data <- function(grouped_data, minimization_config, cost_constraint) {

  # Filter by cost constraint first
  power_feasible <- grouped_data[grouped_data$total_cost <= cost_constraint, ]

  # Apply minimization strategy based on type
  optimal_point <- switch(minimization_config$optimization_goal,
    "minimize" = power_feasible[which.min(power_feasible[[minimization_config$variable]]), ],
    "closest_to_one" = power_feasible[which.min(abs(power_feasible[[minimization_config$variable]] - 1)), ]
  )

  return(optimal_point)
}

#' Prepare minimization data for plotting and display
#'
#' @param analysis_results Results from perform_constrained_minimization_analysis
#' @return List with grouped data and optimal point
#' @noRd
prepare_minimization_data <- function(analysis_results) {
  # Extract key parameters
  minimizing_variable <- analysis_results$metadata$minimizing_variable
  cost_constraint <- analysis_results$metadata$cost_constraint
  target_power <- analysis_results$user_config$design_options$target_power

  # The grouped data (already processed for consistency)
  grouped_data <- analysis_results$power_data

  # Get minimization config
  minimization_config <- get_minimization_config(analysis_results$workflow_info$workflow_id)

  # Find optimal point using same logic as analysis
  optimal_point <- find_optimal_point_in_grouped_data(
    grouped_data,
    minimization_config,
    cost_constraint
  )

  return(list(
    grouped_data = grouped_data,
    optimal_point = optimal_point,
    minimizing_variable = minimizing_variable,
    cost_constraint = cost_constraint,
    target_power = target_power
  ))
}


