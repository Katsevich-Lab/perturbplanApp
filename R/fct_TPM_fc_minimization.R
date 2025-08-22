#' TPM and Fold Change Minimization Analysis Functions
#'
#' @description Helper functions for power+cost TPM/FC minimization workflows (Workflows 6-7)
#' using cost_power_computation + find_optimal_cost_design integration with cost constraints.
#'
#' @name TPM-fc-minimization
NULL

#' Perform TPM threshold minimization analysis using cost_power_computation + find_optimal_cost_design
#'
#' @description Implements Workflow 6: Power+cost TPM minimization by calling
#' cost_power_computation with cost_constraint (from UI), then find_optimal_cost_design
#' to generate equi-power and equi-cost curves.
#'
#' @param config User configuration from sidebar
#' @param workflow_info Detected workflow information  
#' @param pilot_data Pre-extracted pilot data to avoid duplication
#' @return List with power_data and optimal_design compatible with current plotting
#' @noRd
perform_TPM_minimization_analysis <- function(config, workflow_info, pilot_data) {
  
  # Step 1: Get comprehensive parameters from UI using existing mapping function
  # This includes MOI, num_targets, experimental setup, pilot data, costs, etc.
  # Use pre-extracted pilot data (passed as parameter to avoid duplication)
  perturbplan_params <- map_config_to_perturbplan_params(config, workflow_info, pilot_data)
  
  # Override for TPM minimization workflow: 
  # - Use "TPM_threshold" as minimizing variable (perturbplan supports this)
  # - Let cells_per_target and reads_per_cell vary to find minimum TPM combination
  perturbplan_params$minimizing_variable <- "TPM_threshold"
  
  # Always ensure minimum_fold_change is fixed for TPM minimization
  fc_value <- config$effect_sizes$minimum_fold_change_fixed %||% 
             config$design_options$parameter_controls$minimum_fold_change$fixed_value %||% 
             0.5  # Default for downregulation
  perturbplan_params$fixed_variable$minimum_fold_change <- fc_value
  
  # Remove TPM_threshold from fixed_variable so it can vary for optimization
  # (This is required for TPM minimization regardless of UI parameter control status)
  perturbplan_params$fixed_variable$TPM_threshold <- NULL
  
  # Remove cells/reads from fixed_variable so they can vary for cost optimization
  # (This is required for TPM minimization regardless of UI parameter control status)
  perturbplan_params$fixed_variable$cells_per_target <- NULL
  perturbplan_params$fixed_variable$reads_per_cell <- NULL
  
  # Key parameters for TPM minimization with cost constraint
  # Extract cost constraint from UI (required for power+cost workflows)
  cost_constraint <- config$cost_budget %||%
                    config$cost_info$cost_constraint_budget %||%
                    config$design_options$parameter_controls$cost$fixed_value %||%
                    10000  # Default cost constraint if not specified
  
  # Debug: Check cost extraction and config structure
  cat("DEBUG TPM: config structure:\n")
  cat("  config$cost_budget =", config$cost_budget %||% "NULL", "\n")
  cat("  config$cost_info =", if(!is.null(config$cost_info)) "present" else "NULL", "\n")
  if (!is.null(config$cost_info)) {
    cat("  config$cost_info keys:", paste(names(config$cost_info), collapse = ", "), "\n")
  }
  cat("  config$design_options =", if(!is.null(config$design_options)) "present" else "NULL", "\n")
  if (!is.null(config$design_options)) {
    cat("  config$design_options keys:", paste(names(config$design_options), collapse = ", "), "\n")
  }
  cat("  Final cost_constraint =", cost_constraint, "\n")
  
  perturbplan_params$cost_constraint <- as.numeric(cost_constraint)  # Key difference from cost minimization
  perturbplan_params$grid_size <- 30         # Reduced for faster computation
  
  # Step 2: Call cost_power_computation to get power-cost grid
  cost_power_grid <- do.call(perturbplan::cost_power_computation, perturbplan_params)
  
  # Step 3: Call find_optimal_cost_design with ALL required parameters
  # Use the same minimizing_variable as cost_power_computation (TPM_threshold)
  # Include all experimental parameters needed for equi-cost curve generation
  find_optimal_params <- list(
    cost_power_df = cost_power_grid,              # Output from cost_power_computation
    minimizing_variable = "TPM_threshold",        # TPM minimization
    power_target = config$design_options$target_power,
    power_precision = 0.01,                      # Updated precision for workflows 10-11
    MOI = perturbplan_params$MOI,                 # From UI experimental setup
    num_targets = perturbplan_params$num_targets, # From UI experimental setup
    non_targeting_gRNAs = perturbplan_params$non_targeting_gRNAs, # From UI experimental setup
    gRNAs_per_target = perturbplan_params$gRNAs_per_target, # From UI experimental setup
    cost_per_captured_cell = perturbplan_params$cost_per_captured_cell, # From UI cost info
    cost_per_million_reads = perturbplan_params$cost_per_million_reads, # From UI cost info
    cost_grid_size = 30                          # Reduced for faster computation
  )
  
  optimal_results <- do.call(perturbplan::find_optimal_cost_design, find_optimal_params)
  
  # Step 4: Extract key components and format for CURRENT plotting engine
  # Use the ACTUAL data structure returned by find_optimal_cost_design
  
  # Use optimal_cost_power_df as the main data (this has the correct structure)
  power_data <- optimal_results$optimal_cost_power_df
  
  # Find minimum TPM threshold for target power under cost constraint
  target_power <- config$design_options$target_power
  power_tolerance <- 0.01
  
  # Filter by cost constraint first, then find optimal TPM among power-feasible solutions
  cost_feasible <- power_data[power_data$total_cost <= cost_constraint, ]
  
  if (nrow(cost_feasible) == 0) {
    # If no solutions under cost constraint, take the cheapest available
    cost_feasible <- power_data[which.min(power_data$total_cost), ]
  }
  
  # Among cost-feasible solutions, find those meeting target power
  feasible_rows <- cost_feasible[abs(cost_feasible$overall_power - target_power) <= power_tolerance, ]
  
  if (nrow(feasible_rows) == 0) {
    # If no cost-feasible solutions meet target power, take closest power among cost-feasible
    feasible_rows <- cost_feasible[which.min(abs(cost_feasible$overall_power - target_power)), ]
  }
  
  optimal_idx <- which.min(feasible_rows$TPM_threshold)  # Minimize TPM threshold among feasible solutions
  optimal_point <- feasible_rows[optimal_idx, ]
  
  # Create optimal design in expected format using CORRECT column names
  optimal_design <- list(
    cells_per_target = optimal_point$cells_per_target,
    reads_per_cell = optimal_point$reads_per_cell,
    TPM_threshold = optimal_point$TPM_threshold,
    total_cost = optimal_point$total_cost,
    achieved_power = optimal_point$overall_power,
    optimal_minimized_param = optimal_point$TPM_threshold
  )
  
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
      TPM_minimization_data = list(
        cost_grid = optimal_results$cost_grid,
        cost_power_grid = cost_power_grid,
        cost_constraint = cost_constraint
      )
    )
  ))
}

#' Perform fold change minimization analysis using cost_power_computation + find_optimal_cost_design
#'
#' @description Implements Workflow 7: Power+cost FC minimization by calling
#' cost_power_computation with cost_constraint (from UI), then find_optimal_cost_design
#' to generate equi-power and equi-cost curves.
#'
#' @param config User configuration from sidebar
#' @param workflow_info Detected workflow information  
#' @param pilot_data Pre-extracted pilot data to avoid duplication
#' @return List with power_data and optimal_design compatible with current plotting
#' @noRd
perform_fc_minimization_analysis <- function(config, workflow_info, pilot_data) {
  
  # Step 1: Get comprehensive parameters from UI using existing mapping function
  # This includes MOI, num_targets, experimental setup, pilot data, costs, etc.
  # Use pre-extracted pilot data (passed as parameter to avoid duplication)
  perturbplan_params <- map_config_to_perturbplan_params(config, workflow_info, pilot_data)
  
  # Override for FC minimization workflow: 
  # - Use "minimum_fold_change" as minimizing variable (perturbplan supports this)
  # - Let cells_per_target and reads_per_cell vary to find minimum FC combination
  perturbplan_params$minimizing_variable <- "minimum_fold_change"
  
  # Always ensure TPM_threshold is fixed for FC minimization
  TPM_value <- config$analysis_choices$TPM_threshold_fixed %||%
              config$design_options$parameter_controls$TPM_threshold$fixed_value %||%
              10  # Default TPM threshold
  perturbplan_params$fixed_variable$TPM_threshold <- as.numeric(TPM_value)
  
  # Remove minimum_fold_change from fixed_variable so it can vary for optimization
  # (This is required for FC minimization regardless of UI parameter control status)
  perturbplan_params$fixed_variable$minimum_fold_change <- NULL
  
  # Remove cells/reads from fixed_variable so they can vary for cost optimization
  # (This is required for FC minimization regardless of UI parameter control status)
  perturbplan_params$fixed_variable$cells_per_target <- NULL
  perturbplan_params$fixed_variable$reads_per_cell <- NULL
  
  # Key parameters for FC minimization with cost constraint
  # Extract cost constraint from UI (required for power+cost workflows)
  cost_constraint <- config$cost_budget %||%
                    config$cost_info$cost_constraint_budget %||%
                    config$design_options$parameter_controls$cost$fixed_value %||%
                    10000  # Default cost constraint if not specified
  
  # Debug: Check cost extraction
  cat("DEBUG FC: config$cost_budget =", config$cost_budget %||% "NULL", "\n")
  cat("DEBUG FC: Final cost_constraint =", cost_constraint, "\n")
  
  perturbplan_params$cost_constraint <- as.numeric(cost_constraint)  # Key difference from cost minimization
  perturbplan_params$grid_size <- 30         # Reduced for faster computation
  
  # Step 2: Call cost_power_computation to get power-cost grid
  cost_power_grid <- do.call(perturbplan::cost_power_computation, perturbplan_params)
  
  # Step 3: Call find_optimal_cost_design with ALL required parameters
  # Use the same minimizing_variable as cost_power_computation (minimum_fold_change)
  # Include all experimental parameters needed for equi-cost curve generation
  find_optimal_params <- list(
    cost_power_df = cost_power_grid,              # Output from cost_power_computation
    minimizing_variable = "minimum_fold_change",  # FC minimization
    power_target = config$design_options$target_power,
    power_precision = 0.01,                      # Updated precision for workflows 10-11
    MOI = perturbplan_params$MOI,                 # From UI experimental setup
    num_targets = perturbplan_params$num_targets, # From UI experimental setup
    non_targeting_gRNAs = perturbplan_params$non_targeting_gRNAs, # From UI experimental setup
    gRNAs_per_target = perturbplan_params$gRNAs_per_target, # From UI experimental setup
    cost_per_captured_cell = perturbplan_params$cost_per_captured_cell, # From UI cost info
    cost_per_million_reads = perturbplan_params$cost_per_million_reads, # From UI cost info
    cost_grid_size = 30                          # Reduced for faster computation
  )
  
  optimal_results <- do.call(perturbplan::find_optimal_cost_design, find_optimal_params)
  
  # Step 4: Extract key components and format for CURRENT plotting engine
  # Use the ACTUAL data structure returned by find_optimal_cost_design
  
  # Use optimal_cost_power_df as the main data (this has the correct structure)
  power_data <- optimal_results$optimal_cost_power_df
  
  # Find FC closest to 1 (no effect) for target power under cost constraint
  target_power <- config$design_options$target_power
  power_tolerance <- 0.01
  
  # Filter by cost constraint first, then find optimal FC among power-feasible solutions
  cost_feasible <- power_data[power_data$total_cost <= cost_constraint, ]
  
  if (nrow(cost_feasible) == 0) {
    # If no solutions under cost constraint, take the cheapest available
    cost_feasible <- power_data[which.min(power_data$total_cost), ]
  }
  
  # Among cost-feasible solutions, find those meeting target power
  feasible_rows <- cost_feasible[abs(cost_feasible$overall_power - target_power) <= power_tolerance, ]
  
  if (nrow(feasible_rows) == 0) {
    # If no cost-feasible solutions meet target power, take closest power among cost-feasible
    feasible_rows <- cost_feasible[which.min(abs(cost_feasible$overall_power - target_power)), ]
  }
  
  optimal_idx <- which.min(abs(feasible_rows$minimum_fold_change - 1))  # Find FC closest to 1 among feasible solutions
  optimal_point <- feasible_rows[optimal_idx, ]
  
  # Create optimal design in expected format using CORRECT column names
  optimal_design <- list(
    cells_per_target = optimal_point$cells_per_target,
    reads_per_cell = optimal_point$reads_per_cell,
    minimum_fold_change = optimal_point$minimum_fold_change,
    total_cost = optimal_point$total_cost,
    achieved_power = optimal_point$overall_power,
    optimal_minimized_param = optimal_point$minimum_fold_change
  )
  
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
      fc_minimization_data = list(
        cost_grid = optimal_results$cost_grid,
        cost_power_grid = cost_power_grid,
        cost_constraint = cost_constraint
      )
    )
  ))
}