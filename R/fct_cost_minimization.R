#' Cost Minimization Analysis Functions
#'
#' @description Helper functions for power-only cost minimization workflow (Workflow 5)
#' using cost_power_computation + find_optimal_design integration.
#'
#' @name cost-minimization
NULL

#' Perform cost minimization analysis using cost_power_computation + find_optimal_design
#'
#' @description Implements Workflow 5: Power-only cost minimization by calling
#' cost_power_computation with cost_constraint=NULL, then find_optimal_design
#' to generate equi-power and equi-cost curves.
#'
#' @param config User configuration from sidebar
#' @param workflow_info Detected workflow information  
#' @return List with power_data and optimal_design compatible with current plotting
#' @noRd
perform_cost_minimization_analysis <- function(config, workflow_info) {
  
  cat("=== COST MINIMIZATION ANALYSIS ===\n")
  cat("Workflow:", workflow_info$workflow_id, "\n")
  cat("Target power:", config$design_options$target_power, "\n")
  
  # Step 1: Build parameters for cost_power_computation 
  perturbplan_params <- map_config_to_perturbplan_params(config, workflow_info)
  
  # Override specific parameters for cost minimization
  perturbplan_params$cost_constraint <- NULL  # Key requirement: no cost constraint
  perturbplan_params$grid_size <- 100         # As specified
  
  cat("Calling cost_power_computation with cost_constraint = NULL, grid_size = 100\n")
  
  # Step 2: Call cost_power_computation to get power-cost grid
  cost_power_grid <- do.call(perturbplan::cost_power_computation, perturbplan_params)
  
  cat("cost_power_computation completed. Grid dimensions:", nrow(cost_power_grid), "x", ncol(cost_power_grid), "\n")
  
  # Step 3: Call find_optimal_design with specified parameters
  find_optimal_params <- list(
    cost_power_grid = cost_power_grid,
    cost_grid_size = 200,      # Default as specified
    power_precision = 0.01     # As specified
  )
  
  cat("Calling find_optimal_design with cost_grid_size = 200, power_precision = 0.01\n")
  
  optimal_results <- do.call(perturbplan::find_optimal_design, find_optimal_params)
  
  cat("find_optimal_design completed\n")
  
  # Step 4: Extract key components and format for CURRENT plotting engine
  # For now, return in the same format as other workflows to avoid breaking plotting
  
  # Use optimal_power_cost_df as the main data
  power_data <- optimal_results$optimal_power_cost_df
  
  # Find minimum cost point for target power
  target_power <- config$design_options$target_power
  power_tolerance <- 0.01
  target_rows <- power_data[abs(power_data$power - target_power) <= power_tolerance, ]
  
  if (nrow(target_rows) == 0) {
    target_rows <- power_data[which.min(abs(power_data$power - target_power)), ]
  }
  
  optimal_idx <- which.min(target_rows$total_cost)
  optimal_point <- target_rows[optimal_idx, ]
  
  # Create optimal design in expected format
  optimal_design <- list(
    cells_per_target = optimal_point$cells_per_target,
    reads_per_cell = optimal_point$reads_per_cell,
    total_cost = optimal_point$total_cost,
    achieved_power = optimal_point$power,
    optimal_minimized_param = optimal_point$total_cost
  )
  
  cat("Optimal design: cells =", optimal_design$cells_per_target,
      ", reads =", optimal_design$reads_per_cell, 
      ", cost =", optimal_design$total_cost, "\n")
  
  # Return in format compatible with current system
  return(list(
    power_data = power_data,
    optimal_design = optimal_design,
    user_config = config,
    workflow_info = workflow_info,
    metadata = list(
      analysis_mode = get_analysis_mode(),
      workflow_type = workflow_info$workflow_id,
      analysis_timestamp = Sys.time(),
      cost_minimization_data = list(
        optimal_cost_grid = optimal_results$optimal_cost_grid,
        cost_power_grid = cost_power_grid
      )
    )
  ))
}