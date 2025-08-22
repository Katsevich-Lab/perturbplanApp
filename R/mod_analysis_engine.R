#' Analysis Engine Module UI Function
#'
#' @description Backend-only module that generates analysis data.
#' No UI components needed - this is a pure server-side module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_analysis_engine_ui <- function(id) {
  ns <- NS(id)
  tagList(
    # No UI - this is a backend analysis module
  )
}

#' Analysis Engine Server Functions
#'
#' @description THE critical swap point: generates placeholder data that matches
#' user configuration and can be easily replaced with real perturbplan calculations.
#' This is where placeholder vs real analysis mode is determined.
#'
#' @param id Module namespace ID
#' @param workflow_config Reactive containing complete user configuration
#'
#' @return Reactive list containing analysis results data
#' @noRd
#'
#' @importFrom shiny moduleServer reactive req bindCache
#' @importFrom magrittr %>%
mod_analysis_engine_server <- function(id, workflow_config) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ========================================================================
    # MAIN ANALYSIS REACTIVE - THE SWAP POINT
    # ========================================================================

    # Track previous configuration to detect sidebar changes
    previous_config <- reactiveVal(NULL)
    last_plan_count <- reactiveVal(0)

    # Cache for expensive computation results
    cached_results <- reactiveVal(NULL)

    analysis_results <- reactive({
      req(workflow_config())

      config <- workflow_config()

      # Skip analysis if plan not clicked
      if (is.null(config$plan_clicked) || config$plan_clicked == 0) {
        return(NULL)
      }

      # Protection mechanism: Clear results if sidebar inputs changed since last plan
      current_config_hash <- create_config_hash(config)
      previous_hash <- previous_config()
      current_plan_count <- config$plan_clicked

      # If this is a new plan click, update tracking and clear cache
      if (current_plan_count > last_plan_count()) {
        previous_config(current_config_hash)
        last_plan_count(current_plan_count)
        cached_results(NULL)  # Clear cache for new plan
      } else {
        # Check if sidebar inputs changed since last plan
        if (!is.null(previous_hash) && current_config_hash != previous_hash) {
          # Sidebar changed but no new plan click - return NULL to clear results
          cached_results(NULL)  # Clear cache
          return(NULL)
        }

        # Same config as before - return cached results if available
        if (!is.null(cached_results())) {
          return(cached_results())
        }
      }

      # Configuration received successfully

      # Validate configuration
      validation <- validate_workflow_config(config)
      if (!validation$is_valid) {
        return(list(
          error = paste("Configuration Error:", paste(validation$errors, collapse = ", ")),
          metadata = list(
            analysis_mode = get_analysis_mode(),
            timestamp = Sys.time()
          )
        ))
      }

      # ============================================================================
      # CENTRALIZED PARAMETER TRANSLATION: UI → Backend
      # ============================================================================
      # Standardize UI parameter names to backend-compatible names before analysis
      # This happens AFTER UI processing but BEFORE backend analysis functions
      if (!is.null(config$design_options$minimization_target)) {
        config$design_options$minimization_target <- switch(
          config$design_options$minimization_target,
          "cells" = "cells_per_target",           # "Cells per target" → backend
          "reads" = "reads_per_cell",             # "Reads per cell" → backend
          "fold_change" = "minimum_fold_change",  # "Fold change" → backend
          config$design_options$minimization_target  # No change for TPM_threshold, cost
        )
      }

      # Detect workflow scenario (with translated parameter names)
      workflow_info <- detect_workflow_scenario(config)

      # THE CRITICAL SWAP POINT: Choose placeholder vs real analysis
      # Wrap in comprehensive error handling to prevent app crashes
      results <- tryCatch({
        if (use_placeholder_mode()) {
          # PLACEHOLDER MODE: Generate realistic fake data
          generate_placeholder_analysis(config, workflow_info)
        } else {
          # REAL MODE: Call perturbplan package functions
          generate_real_analysis(config, workflow_info)
        }
      }, error = function(e) {
        # Return error object instead of crashing
        list(
          error = paste("Analysis Error:", e$message),
          metadata = list(
            analysis_mode = get_analysis_mode(),
            workflow_type = workflow_info$workflow_id %||% "unknown",
            timestamp = Sys.time(),
            error_details = as.character(e)
          )
        )
      })

      # Cache the results to prevent duplicate computation
      cached_results(results)
      return(results)
    })

    return(analysis_results)
  })
}


# ============================================================================
# PLACEHOLDER DATA GENERATION (Current Implementation)
# ============================================================================

#' Generate placeholder analysis results
#'
#' @description Creates realistic placeholder data that uses actual user inputs
#' and follows the same data structure as real perturbplan results.
#' THIS IS WHERE PLACEHOLDER DATA IS GENERATED.
#'
#' @param config User configuration from sidebar modules
#' @param workflow_info Detected workflow information
#'
#' @return List containing placeholder analysis results
#' @noRd
generate_placeholder_analysis <- function(config, workflow_info) {

  # Extract user parameters for realistic placeholder data
  design_config <- config$design_options
  target_power <- design_config$target_power
  cost_budget <- design_config$cost_budget

  # Create parameter grid based on user configuration
  param_grid <- create_parameter_grid(config, workflow_info)

  # Generate power calculations based on workflow type
  if (workflow_info$plot_type == "single_parameter_curve") {
    power_results <- generate_single_parameter_power_curve(param_grid, workflow_info, target_power)
  } else if (workflow_info$plot_type == "cost_tradeoff_curves") {
    power_results <- generate_cost_tradeoff_curves(param_grid, workflow_info, target_power, cost_budget)
  } else {
    # Fallback for unknown plot types
    power_results <- list(
      error = paste("Unknown plot type:", workflow_info$plot_type)
    )
  }

  # Create comprehensive results object
  results <- list(
    # Core data for plotting
    power_data = power_results$power_data,
    optimal_design = power_results$optimal_design,

    # Workflow and user configuration
    workflow_info = workflow_info,
    user_config = config,

    # Parameter information
    parameter_grid = param_grid,
    parameter_ranges = extract_all_parameter_ranges(config),

    # Cost calculations (if applicable)
    cost_data = if (!is.null(cost_budget) || workflow_info$plot_type == "cost_tradeoff_curves") {
      power_results$cost_data
    } else {
      NULL
    },

    # Results summary
    summary = create_results_summary(power_results, workflow_info, config),

    # Analysis metadata
    metadata = create_analysis_metadata(config, workflow_info),

    # Success status
    success = is.null(power_results$error),
    error = power_results$error
  )

  return(results)
}


# ============================================================================
# PARAMETER GRID CREATION
# ============================================================================

#' Create parameter grid for analysis
#'
#' @description Creates parameter combinations based on user configuration.
#' Uses actual user inputs for fixed parameters and reasonable ranges for varying ones.
#'
#' @param config User configuration
#' @param workflow_info Detected workflow information
#'
#' @return Data frame with parameter combinations
#' @noRd
create_parameter_grid <- function(config, workflow_info) {

  # Extract parameter controls
  param_controls <- config$design_options$parameter_controls

  # Initialize parameter values
  grid_params <- list()

  # Process each parameter based on control type
  # Map internal names to UI control names
  param_mapping <- c(
    "cells" = "cells_per_target",
    "reads" = "reads_per_cell",
    "TPM_threshold" = "TPM_threshold",
    "fold_change" = "min_fold_change"
  )

  for (param_name in names(param_mapping)) {
    control_name <- param_mapping[[param_name]]
    param_control <- param_controls[[control_name]]

    # Process parameter control

    if (is.null(param_control)) {
      # Use defaults if not specified
      grid_params[[param_name]] <- get_default_parameter_range(param_name)[1]
    } else if (param_control$type == "fixed") {
      # Use user-specified fixed value
      grid_params[[param_name]] <- param_control$fixed_value
    } else if (param_control$type %in% c("varying", "minimizing", "optimizing")) {
      # Use range for varying parameters
      grid_params[[param_name]] <- get_default_parameter_range(param_name)
    }
  }

  # Create combinations based on workflow type
  if (workflow_info$plot_type == "single_parameter_curve") {
    # Single parameter varies, others fixed
    varying_param <- workflow_info$minimizing_parameter

    # Create parameter combinations

    # Create grid with one varying parameter
    grid <- expand.grid(
      cells = if (varying_param == "cells") grid_params$cells else grid_params$cells[1],
      reads = if (varying_param == "reads") grid_params$reads else grid_params$reads[1],
      TPM_threshold = if (varying_param == "TPM_threshold") grid_params$TPM_threshold else grid_params$TPM_threshold[1],
      fold_change = if (varying_param == "fold_change") grid_params$fold_change else grid_params$fold_change[1],
      stringsAsFactors = FALSE
    )

  } else if (workflow_info$plot_type == "cost_tradeoff_curves") {
    # Multiple parameters vary (typically cells and reads)
    grid <- expand.grid(
      cells = grid_params$cells[seq(1, length(grid_params$cells), by = 3)],  # Sample for performance
      reads = grid_params$reads[seq(1, length(grid_params$reads), by = 5)],   # Sample for performance
      TPM_threshold = grid_params$TPM_threshold[1],
      fold_change = grid_params$fold_change[1],
      stringsAsFactors = FALSE
    )
  }

  return(grid)
}


# ============================================================================
# SINGLE PARAMETER POWER CURVE GENERATION (8 workflows)
# ============================================================================

#' Generate single parameter power curve data
#'
#' @description Creates realistic power curve for workflows 1-4, 6-7, 9-10
#' where one parameter varies and others are fixed.
#'
#' @param param_grid Parameter combinations
#' @param workflow_info Workflow information
#' @param target_power User's target power threshold
#'
#' @return List with power_data and optimal_design
#' @noRd
generate_single_parameter_power_curve <- function(param_grid, workflow_info, target_power) {

  varying_param <- workflow_info$minimizing_parameter

  # Extract the varying parameter values from the grid
  if (varying_param %in% c("cells", "cells_per_target")) {
    varying_values <- param_grid$cells %||% param_grid$cells_per_target
  } else if (varying_param %in% c("reads", "reads_per_cell")) {
    varying_values <- param_grid$reads_per_cell %||% param_grid$reads
  } else if (varying_param %in% c("TPM_threshold")) {
    varying_values <- param_grid$TPM_threshold
  } else if (varying_param %in% c("fold_change", "minimum_fold_change")) {
    varying_values <- param_grid$fold_change %||% param_grid$minimum_fold_change
  } else {
    stop(paste("Unknown varying parameter:", varying_param))
  }

  # Generate realistic STRICTLY INCREASING power curves for all parameters
  # Normalize parameter values to [0,1] range for consistent scaling
  normalized_values <- (varying_values - min(varying_values)) / (max(varying_values) - min(varying_values))

  if (varying_param %in% c("cells", "reads", "cells_per_target", "reads_per_cell")) {
    # More cells/reads = higher power, with diminishing returns (sigmoid curve)
    power_values <- 0.1 + 0.85 * (1 - exp(-4 * normalized_values))
  } else if (varying_param %in% c("TPM_threshold")) {
    # TPM threshold: show as INCREASING for optimization perspective
    # (interpret as: lower threshold = more genes included = higher power)
    power_values <- 0.15 + 0.8 * normalized_values
  } else if (varying_param %in% c("fold_change", "minimum_fold_change")) {
    # Higher fold change = easier to detect larger effects = higher power
    power_values <- 0.1 + 0.85 * (1 - exp(-3 * normalized_values))
  } else {
    # Fallback for any unknown parameters
    power_values <- 0.2 + 0.7 * normalized_values
  }

  # Ensure strictly increasing by using cumulative maximum
  power_values <- pmax(power_values, cummax(power_values))
  power_values <- pmin(power_values, 0.95)  # Cap at 95%

  # Add realistic noise
  power_values <- power_values + rnorm(length(power_values), 0, 0.02)
  power_values <- pmax(pmin(power_values, 0.99), 0.01)  # Keep in bounds

  # Create power data
  power_data <- data.frame(
    parameter_name = varying_param,
    parameter_value = varying_values,
    power = power_values,
    meets_threshold = power_values >= target_power,
    stringsAsFactors = FALSE
  )

  # For power+cost workflows, add computed cells/reads columns
  if (workflow_info$category == "power_cost_single") {
    # Cost calculation parameters
    cost_per_cell <- 0.10
    cost_per_million_reads <- 50
    cost_budget <- 10000  # Default cost budget for placeholder

    if (!is.null(workflow_info$varying_parameter)) {
      if (workflow_info$varying_parameter == "cells") {
        # Reads is fixed, cells is computed from cost constraint
        power_data$reads <- 1200  # Fixed value from sidebar
        # Compute cells for each TPM/FC to stay within cost budget
        power_data$cells <- pmax(300, pmin(1000,
          (cost_budget - cost_per_million_reads * (power_data$reads / 1e6) * 500) /
          (cost_per_cell + cost_per_million_reads * (power_data$reads / 1e6))
        ))
      } else if (workflow_info$varying_parameter == "reads") {
        # Cells is fixed, reads is computed from cost constraint
        power_data$cells <- 500  # Fixed value from sidebar
        # Compute reads for each TPM/FC to stay within cost budget
        remaining_budget <- cost_budget - cost_per_cell * power_data$cells
        power_data$reads <- pmax(800, pmin(2000,
          (remaining_budget / (cost_per_million_reads / 1e6)) / power_data$cells
        ))
      }

      # Calculate resulting cost
      power_data$cost <- power_data$cells * cost_per_cell +
                        cost_per_million_reads * (power_data$reads / 1e6) * power_data$cells
    }
  }

  # Find optimal design (minimum parameter value that meets power threshold)
  valid_designs <- power_data[power_data$meets_threshold, ]
  if (nrow(valid_designs) > 0) {
    optimal_idx <- which.min(valid_designs$parameter_value)
    optimal_design <- list(
      parameter = varying_param,
      value = valid_designs$parameter_value[optimal_idx],
      power = valid_designs$power[optimal_idx],
      found = TRUE
    )

    # For ALL cost-related workflows, ensure complete (TPM/FC, cells, reads) combination
    if (workflow_info$category %in% c("power_cost_single", "power_cost_multi", "power_only_cost")) {

      # Add optimal minimized parameter (TPM or FC)
      if (workflow_info$minimizing_parameter == "TPM_threshold") {
        optimal_design$optimal_minimized_param <- optimal_design$value  # Use the actual optimal value
      } else if (workflow_info$minimizing_parameter == "fold_change") {
        optimal_design$optimal_minimized_param <- optimal_design$value  # Use the actual optimal value
      } else if (workflow_info$minimizing_parameter == "cost") {
        # For cost minimization workflows, use fixed TPM/FC from sidebar or defaults
        if (!is.null(workflow_info$fixed_TPM)) {
          optimal_design$optimal_minimized_param <- workflow_info$fixed_TPM
        } else if (!is.null(workflow_info$fixed_fc)) {
          optimal_design$optimal_minimized_param <- workflow_info$fixed_fc
        } else {
          # Default values when both TPM and FC are varying
          optimal_design$optimal_minimized_param <- 15  # Default TPM threshold
        }
      }

      # Ensure cells and reads are always included
      if (is.null(optimal_design$cells) || is.null(optimal_design$reads)) {
        # Compute cells and reads values based on cost constraint + optimal TPM/FC
        if (!is.null(workflow_info$varying_parameter)) {
          if (workflow_info$varying_parameter == "cells") {
            # reads is fixed, cells is computed
            optimal_design$cells <- 600  # Placeholder computed cells value
            optimal_design$reads <- 1200  # Fixed value from sidebar
          } else if (workflow_info$varying_parameter == "reads") {
            # cells is fixed, reads is computed
            optimal_design$cells <- 500  # Fixed value from sidebar
            optimal_design$reads <- 1400  # Placeholder computed reads value
          } else {
            # Both cells and reads vary - use optimal combination
            optimal_design$cells <- 500   # Optimal cells
            optimal_design$reads <- 1500  # Optimal reads
          }
        } else {
          # Default values when parameters not specified
          optimal_design$cells <- 500   # Default optimal cells
          optimal_design$reads <- 1500  # Default optimal reads
        }
      }

      # Calculate cost if not already present
      if (is.null(optimal_design$cost)) {
        optimal_design$cost <- optimal_design$cells * 0.10 + 50 * (optimal_design$reads / 1e6) * optimal_design$cells
      }
    }
  } else {
    optimal_design <- list(
      parameter = varying_param,
      value = NA,
      power = NA,
      found = FALSE,
      message = "No design meets target power within parameter range"
    )
  }

  return(list(
    power_data = power_data,
    optimal_design = optimal_design,
    error = NULL
  ))
}


# ============================================================================
# COST TRADEOFF CURVES GENERATION (3 workflows)
# ============================================================================

#' Generate cost-power tradeoff curve data
#'
#' @description Creates cost-power optimization data for workflows 5, 8, 11
#' where cells and reads vary simultaneously.
#'
#' @param param_grid Parameter combinations with cells and reads varying
#' @param workflow_info Workflow information
#' @param target_power User's target power threshold
#' @param cost_budget User's cost budget (NULL for power-only)
#'
#' @return List with power_data, cost_data, and optimal_design
#' @noRd
generate_cost_tradeoff_curves <- function(param_grid, workflow_info, target_power, cost_budget) {

  # Calculate power for each combination (cells × reads interaction)
  param_grid$power <- with(param_grid, {
    # Realistic power calculation: more total effort (cells × reads) = higher power
    total_effort <- cells * reads / 1000  # Scale for realistic values
    power <- 1 - exp(-0.001 * total_effort)
    power <- pmax(pmin(power, 0.95), 0.05)  # Keep in realistic bounds

    # Add noise and ensure variability
    power + rnorm(nrow(param_grid), 0, 0.03)
  })

  # Calculate costs (using default cost structure)
  cost_per_cell <- 0.10      # $0.10 per cell
  cost_per_million_reads <- 50   # $50 per million reads

  param_grid$cost <- with(param_grid, {
    cost_per_cell * cells + cost_per_million_reads * (reads / 1e6) * cells
  })

  # Add meets_threshold column for plotting
  param_grid$meets_threshold <- param_grid$power >= target_power

  # For power+cost multi-parameter workflows, add conditional TPM/FC columns
  if (workflow_info$category == "power_cost_multi") {
    if (workflow_info$minimizing_parameter == "TPM_threshold") {
      # Add TPM threshold values for each (cells, reads) combination
      param_grid$TPM_threshold <- with(param_grid, {
        # Generate realistic TPM values: higher power = lower TPM needed
        base_TPM <- 20 - 15 * (power - 0.5) / 0.45  # Scale TPM inversely with power
        pmax(5, pmin(50, base_TPM + rnorm(nrow(param_grid), 0, 2)))  # Add noise, keep in bounds
      })
    } else if (workflow_info$minimizing_parameter == "fold_change") {
      # Add fold change values for each (cells, reads) combination
      param_grid$fold_change <- with(param_grid, {
        # Generate realistic FC values: higher power = lower FC needed
        base_fc <- 2.5 - 1.3 * (power - 0.5) / 0.45  # Scale FC inversely with power
        pmax(1.1, pmin(5.0, base_fc + rnorm(nrow(param_grid), 0, 0.1)))  # Add noise, keep in bounds
      })
    }
    # Note: If minimizing "cost" (Workflow 5), no additional columns added
  }

  # Find designs that meet power threshold
  valid_designs <- param_grid[param_grid$meets_threshold, ]

  # Find optimal design
  if (!is.null(cost_budget)) {
    # Power + cost optimization: minimize cost while meeting power, within budget
    if (nrow(valid_designs) > 0) {
      budget_feasible <- valid_designs[valid_designs$cost <= cost_budget, ]
      if (nrow(budget_feasible) > 0) {
        optimal_idx <- which.min(budget_feasible$cost)
        optimal_design <- budget_feasible[optimal_idx, ]
        optimal_design$found <- TRUE
        optimal_design$type <- "cost_minimized_within_budget"

        # For ALL cost-related multi-parameter workflows, ensure complete parameter set
        if (workflow_info$category %in% c("power_cost_multi", "power_only_cost")) {
          if (workflow_info$minimizing_parameter == "TPM_threshold") {
            optimal_design$optimal_minimized_param <- 12  # Placeholder optimal TPM for multi-param
          } else if (workflow_info$minimizing_parameter == "fold_change") {
            optimal_design$optimal_minimized_param <- 1.5  # Placeholder optimal fold change for multi-param
          } else if (workflow_info$minimizing_parameter == "cost") {
            # For cost minimization, use fixed TPM/FC or reasonable default
            optimal_design$optimal_minimized_param <- 15  # Default TPM threshold for cost minimization
          }
        }
      } else {
        optimal_design <- list(found = FALSE, message = "No design meets power target within budget")
      }
    } else {
      optimal_design <- list(found = FALSE, message = "No design meets target power")
    }
  } else {
    # Power-only optimization OR power+cost multi-parameter workflows
    optimal_design <- list(
      found = TRUE,
      cells = 500,
      reads = 1500,
      cost = 500 * 0.10 + 50 * (1500 / 1e6) * 500,
      power = 0.85,
      type = if (!is.null(cost_budget)) "cost_minimized_within_budget" else "cost_minimized_power_only"
    )

    # For ALL cost-related multi-parameter workflows, ensure complete parameter set
    if (workflow_info$category %in% c("power_cost_multi", "power_only_cost")) {
      if (workflow_info$minimizing_parameter == "TPM_threshold") {
        # Find the TPM value for the optimal design from param_grid
        optimal_design$optimal_minimized_param <- 12  # Placeholder optimal TPM for multi-param
      } else if (workflow_info$minimizing_parameter == "fold_change") {
        optimal_design$optimal_minimized_param <- 1.5  # Placeholder optimal fold change for multi-param
      } else if (workflow_info$minimizing_parameter == "cost") {
        # For cost minimization, use fixed TPM/FC or reasonable default
        optimal_design$optimal_minimized_param <- 15  # Default TPM threshold for cost minimization
      }
    }
  }

  return(list(
    power_data = param_grid,
    cost_data = param_grid,  # Same data, used for cost-focused plots
    optimal_design = optimal_design,
    error = NULL
  ))
}


# ============================================================================
# REAL ANALYSIS PLACEHOLDER (Future Integration Point)
# ============================================================================

#' Generate real analysis results using perturbplan package
#'
#' @description THIS IS THE FUTURE INTEGRATION POINT.
#' When ready to integrate with perturbplan package, implement here.
#' The function signature and return structure should match generate_placeholder_analysis().
#'
#' @param config User configuration from sidebar modules
#' @param workflow_info Detected workflow information
#'
#' @return List containing real analysis results (same structure as placeholder)
#' @noRd
generate_real_analysis <- function(config, workflow_info) {
  # Extract pilot data for perturbplan function
  pilot_data <- extract_pilot_data(config$experimental_setup)

  # Handle case where pilot data is not available
  if (is.null(pilot_data)) {
    return(list(
      error = "Pilot data required for real analysis. Please upload reference expression data or use built-in data.",
      metadata = list(
        analysis_mode = "Real Analysis (Data Missing)",
        timestamp = Sys.time()
      )
    ))
  }

  # Check if this is cost minimization workflow (Workflow 5)
  if (workflow_info$workflow_id == "power_cost_minimization") {
    # Use specialized cost minimization analysis
    tryCatch({
      results <- perform_cost_minimization_analysis(config, workflow_info, pilot_data)

      # Return results directly (already in plotting format)
      return(results)

    }, error = function(e) {
      stop("Cost minimization analysis failed: ", e$message)
    })
  }

  # Check if this is TPM or FC minimization workflow (Workflows 10-11)
  if (workflow_info$workflow_id %in% c("power_cost_TPM_cells_reads", "power_cost_fc_cells_reads")) {
    # Use unified constrained minimization analysis
    cat("=== ANALYSIS ENGINE: Calling unified constrained minimization ===\n")
    tryCatch({
      results <- perform_constrained_minimization_analysis(config, workflow_info, pilot_data)
      
      # Debug: Check what unified function returns
      cat("=== UNIFIED RESULTS STRUCTURE ===\n")
      cat("  power_data available:", !is.null(results$power_data), "\n")
      cat("  power_data rows:", if(!is.null(results$power_data)) nrow(results$power_data) else "NULL", "\n")
      cat("  optimal_design available:", !is.null(results$optimal_design), "\n")
      cat("  workflow_info available:", !is.null(results$workflow_info), "\n")
      cat("  metadata available:", !is.null(results$metadata), "\n")
      if (!is.null(results$error)) {
        cat("  ERROR in results:", results$error, "\n")
      }

      # Return results directly (already in plotting format)
      return(results)

    }, error = function(e) {
      cat("=== ERROR in unified minimization ===\n")
      cat("Error message:", e$message, "\n")
      stop("Constrained minimization analysis failed: ", e$message)
    })
  }

  # For all other workflows: Use standard cost_power_computation
  # Map UI configuration to perturbplan::cost_power_computation parameters
  perturbplan_params <- map_config_to_perturbplan_params(config, workflow_info, pilot_data)

  # Call perturbplan::cost_power_computation
  tryCatch({
    results <- do.call(perturbplan::cost_power_computation, perturbplan_params)

    # Rename raw_reads_per_cell to reads_per_cell for consistency
    if ("raw_reads_per_cell" %in% names(results) && !"reads_per_cell" %in% names(results)) {
      results$reads_per_cell <- results$raw_reads_per_cell
      results$raw_reads_per_cell <- NULL
    }

    # Convert perturbplan results to our standardized format
    standardized_results <- standardize_perturbplan_results(results, config, workflow_info)

    # NEW: Transform to plotting-compatible format
    if (!is.null(standardized_results$error)) {
      return(standardized_results)  # Return error as-is
    }

    plotting_results <- transform_perturbplan_to_plotting_format(
      standardized_results, config, workflow_info
    )

    return(plotting_results)

  }, error = function(e) {
    # Return error object to display to user instead of falling back
    cat("=== REAL ANALYSIS ERROR ===\n")
    cat("Error:", e$message, "\n")
    cat("Returning error to user\n")
    cat("==========================\n")

    return(list(
      error = e$message,
      metadata = list(
        analysis_mode = get_analysis_mode(),
        workflow_type = workflow_info$workflow_id %||% "unknown",
        timestamp = Sys.time(),
        error_details = as.character(e)
      )
    ))
  })
}


# ============================================================================
# UTILITY FUNCTIONS
# ============================================================================

#' Create configuration hash for change detection
#'
#' @description Creates a hash of the sidebar configuration to detect changes.
#' Excludes plan_clicked and timestamp to focus on actual parameter changes.
#'
#' @param config User configuration from sidebar modules
#' @return Character string representing configuration hash
#' @noRd
create_config_hash <- function(config) {
  # Extract relevant configuration excluding plan_clicked and timestamp
  config_for_hash <- list(
    design_options = config$design_options,
    perturbation_choices = config$perturbation_choices,
    experimental_setup = config$experimental_setup,
    analysis_choices = config$analysis_choices,
    effect_sizes = config$effect_sizes
  )

  # Create hash using digest (assuming digest package is available)
  # If digest not available, use simple serialization
  tryCatch({
    digest::digest(config_for_hash, algo = "md5")
  }, error = function(e) {
    # Fallback: use serialization and simple hash
    paste(collapse = "", as.character(serialize(config_for_hash, NULL)))
  })
}

#' Extract all parameter ranges from configuration
#'
#' @param config User configuration
#' @return Named list of parameter ranges
#' @noRd
extract_all_parameter_ranges <- function(config) {
  param_names <- c("cells", "reads", "TPM_threshold", "fold_change")
  ranges <- list()

  for (param_name in param_names) {
    ranges[[param_name]] <- extract_parameter_range(config, param_name)
  }

  return(ranges)
}

#' Create results summary
#'
#' @param power_results Power analysis results
#' @param workflow_info Workflow information
#' @param config User configuration
#' @return List with summary information
#' @noRd
create_results_summary <- function(power_results, workflow_info, config) {

  if (!is.null(power_results$error)) {
    return(list(
      success = FALSE,
      error = power_results$error
    ))
  }

  optimal <- power_results$optimal_design
  target_power <- config$design_options$target_power

  summary <- list(
    workflow_type = workflow_info$workflow_id,
    workflow_description = workflow_info$description,
    target_power = target_power,
    optimal_design_found = if (is.list(optimal)) optimal$found else !is.na(optimal),

    # Workflow-specific summary
    optimization_summary = if (workflow_info$plot_type == "single_parameter_curve") {
      if (is.list(optimal) && optimal$found) {
        list(
          minimized_parameter = optimal$parameter,
          optimal_value = optimal$value,
          achieved_power = optimal$power,
          recommendation = paste("Minimize", format_parameter_name(optimal$parameter), "to", optimal$value)
        )
      } else {
        list(recommendation = "No feasible design found within parameter constraints")
      }
    } else {
      if (is.list(optimal) && optimal$found) {
        list(
          optimal_cells = optimal$cells,
          optimal_reads = optimal$reads,
          total_cost = optimal$cost,
          achieved_power = optimal$power,
          recommendation = paste("Use", optimal$cells, "cells and", optimal$reads, "reads per cell")
        )
      } else {
        list(recommendation = "No feasible design found within constraints")
      }
    }
  )

  return(summary)
}
