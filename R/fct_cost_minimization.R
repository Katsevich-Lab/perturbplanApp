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
perform_cost_minimization_analysis <- function(config, workflow_info, pilot_data) {
  
  # cat("=== COST MINIMIZATION ANALYSIS ===\n")
  # cat("Workflow:", workflow_info$workflow_id, "\n")
  # cat("Target power:", config$design_options$target_power, "\n")
  
  # Step 1: Get comprehensive parameters from UI using existing mapping function
  # This includes MOI, num_targets, experimental setup, pilot data, costs, etc.
  # Use pre-extracted pilot data (passed as parameter to avoid duplication)
  perturbplan_params <- map_config_to_perturbplan_params(config, workflow_info, pilot_data)
  
  # Override for cost minimization workflow: 
  # - Use "cost" as minimizing variable (perturbplan now supports this)
  # - Let cells_per_target and reads_per_cell vary to find minimum cost combination
  perturbplan_params$minimizing_variable <- "cost"
  
  # Always ensure minimum_fold_change is fixed for cost minimization
  fc_value <- config$effect_sizes$minimum_fold_change_fixed %||% 
             config$design_options$parameter_controls$minimum_fold_change$fixed_value %||% 
             0.5  # Default for downregulation
  perturbplan_params$fixed_variable$minimum_fold_change <- fc_value
  
  # Always ensure TPM_threshold is fixed  
  TPM_value <- config$analysis_choices$TPM_threshold_fixed %||%
              config$design_options$parameter_controls$TPM_threshold$fixed_value %||%
              10  # Default TPM threshold
  perturbplan_params$fixed_variable$TPM_threshold <- as.numeric(TPM_value)
  
  # Remove cells/reads from fixed_variable so they can vary for cost optimization
  # (This is required for cost minimization regardless of UI parameter control status)
  perturbplan_params$fixed_variable$cells_per_target <- NULL
  perturbplan_params$fixed_variable$reads_per_cell <- NULL
  
  # Key parameters for cost minimization
  perturbplan_params$cost_constraint <- NULL  # Key requirement: no cost constraint  
  perturbplan_params$grid_size <- 100         # As specified
  
  # cat("Calling cost_power_computation with cost_constraint = NULL, grid_size = 100\n")
  
  # Step 2: Call cost_power_computation to get power-cost grid
  cost_power_grid <- do.call(perturbplan::cost_power_computation, perturbplan_params)
  
  # cat("cost_power_computation completed. Grid dimensions:", nrow(cost_power_grid), "x", ncol(cost_power_grid), "\n")
  
  # Step 3: Call find_optimal_cost_design with ALL required parameters
  # Use the same minimizing_variable as cost_power_computation (cost)
  # Include all experimental parameters needed for equi-cost curve generation
  find_optimal_params <- list(
    cost_power_df = cost_power_grid,              # Output from cost_power_computation
    minimizing_variable = "cost",                 # Cost minimization
    power_target = config$design_options$target_power,
    power_precision = 0.002,                      # Very tight precision for optimal results
    MOI = perturbplan_params$MOI,                 # From UI experimental setup
    num_targets = perturbplan_params$num_targets, # From UI experimental setup
    non_targeting_gRNAs = perturbplan_params$non_targeting_gRNAs, # From UI experimental setup
    gRNAs_per_target = perturbplan_params$gRNAs_per_target, # From UI experimental setup
    cost_per_captured_cell = perturbplan_params$cost_per_captured_cell, # From UI cost info
    cost_per_million_reads = perturbplan_params$cost_per_million_reads, # From UI cost info
    cost_grid_size = 200                          # As specified
  )
  
  # cat("Calling find_optimal_cost_design with cost_grid_size = 200, power_precision = 0.01\n")
  
  optimal_results <- do.call(perturbplan::find_optimal_cost_design, find_optimal_params)
  
  # cat("find_optimal_cost_design completed\n")
  
  # Step 4: Extract key components and format for CURRENT plotting engine
  # Use the ACTUAL data structure returned by find_optimal_cost_design
  
  # Use optimal_cost_power_df as the main data (this has the correct structure)
  power_data <- optimal_results$optimal_cost_power_df
  
  # Find minimum cost point for target power
  target_power <- config$design_options$target_power
  power_tolerance <- 0.01
  target_rows <- power_data[abs(power_data$overall_power - target_power) <= power_tolerance, ]
  
  if (nrow(target_rows) == 0) {
    target_rows <- power_data[which.min(abs(power_data$overall_power - target_power)), ]
  }
  
  optimal_idx <- which.min(target_rows$total_cost)
  optimal_point <- target_rows[optimal_idx, ]
  
  # Create optimal design in expected format using CORRECT column names
  optimal_design <- list(
    cells_per_target = optimal_point$cells_per_target,
    reads_per_cell = optimal_point$reads_per_cell,  # This is corrected in find_optimal_cost_design output
    total_cost = optimal_point$total_cost,
    achieved_power = optimal_point$overall_power,   # Correct column name
    optimal_minimized_param = optimal_point$total_cost
  )
  
  # cat("Optimal design: cells =", optimal_design$cells_per_target,
  #     ", reads =", optimal_design$reads_per_cell, 
  #     ", cost =", optimal_design$total_cost, "\n")
  
  # Return in format compatible with current system
  return(list(
    power_data = power_data,  # This is optimal_cost_power_df for equi-power curves
    cost_data = optimal_results$optimal_cost_grid,  # This is cost_grid for equi-cost curves
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