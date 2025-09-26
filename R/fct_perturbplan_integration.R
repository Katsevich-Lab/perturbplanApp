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

  # extract the pilot data
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
        TPM_threshold = 1,           # TPM filtering at 1
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
        TPM_threshold = 1,                   # TPM filtering at 1
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

  # Extract design options
  design_opts <- config$design_options
  experimental_opts <- config$experimental_setup
  analysis_opts <- config$analysis_choices
  effect_opts <- config$effect_sizes
  advanced_opts <- config$advanced_choices

  # Determine minimizing variable from workflow
  minimizing_variable <- workflow_info$minimizing_parameter

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

    # Add fixed parameters based on their type and actual config values (including slider overrides)
    if (!is.null(param_controls$cells_per_target) && param_controls$cells_per_target$type == "fixed") {
      # Use actual config value (includes slider overrides), fallback to default
      value <- experimental_opts$cells_fixed
      fixed_variable$cells_per_target <- round(value)
    }

    if (!is.null(param_controls$reads_per_cell) && param_controls$reads_per_cell$type == "fixed") {
      # Get sequenced reads per cell value (includes slider overrides), fallback to default
      sequenced_reads_per_cell <- experimental_opts$reads_per_cell_fixed

      # Apply mapping efficiency transformation: reads_per_cell = sequenced_reads_per_cell * mapping_efficiency
      mapping_efficiency <- advanced_opts$mapping_efficiency
      reads_per_cell <- sequenced_reads_per_cell * mapping_efficiency

      fixed_variable$reads_per_cell <- round(reads_per_cell)
    }

    if (!is.null(param_controls$TPM_threshold) &&
        param_controls$TPM_threshold$type == "fixed") {
      # Use actual config value (includes slider overrides), fallback to default
      value <- analysis_opts$TPM_threshold_fixed
      fixed_variable$TPM_threshold <- value
    }

    if (!is.null(param_controls$minimum_fold_change) &&
        param_controls$minimum_fold_change$type == "fixed") {
      # Use actual config value (includes slider overrides), fallback to default
      value <- effect_opts$minimum_fold_change_fixed
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
      cost_constraint <- design_opts$cost_budget
      cost_per_cell <- design_opts$cost_per_cell
      cost_per_million_reads <- design_opts$cost_per_million_reads


      # Call obtain_fixed_variable_constraining_cost to calculate the missing parameter
      tryCatch({
        cost_result <- perturbplan::obtain_fixed_variable_constraining_cost(
          cost_per_captured_cell = cost_per_cell,
          cost_per_million_reads = cost_per_million_reads,
          cost_constraint = cost_constraint,
          MOI = experimental_opts$MOI,
          num_targets = experimental_opts$num_targets,
          non_targeting_gRNAs = experimental_opts$non_targeting_gRNAs,
          gRNAs_per_target = experimental_opts$gRNAs_per_target,
          reads_per_cell = if(has_reads_fixed) fixed_variable$reads_per_cell else NULL,
          cells_per_target = if(has_cells_fixed) fixed_variable$cells_per_target else NULL,
          mapping_efficiency = advanced_opts$mapping_efficiency
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

  # Build parameter list
  params <- list(
    minimizing_variable = minimizing_variable,
    fixed_variable = fixed_variable,

    # Experimental parameters
    MOI = experimental_opts$MOI,
    num_targets = experimental_opts$num_targets,
    non_targeting_gRNAs = experimental_opts$non_targeting_gRNAs,
    gRNAs_per_target = experimental_opts$gRNAs_per_target,

    # Effect size parameters
    gRNA_variability = advanced_opts$gRNA_variability,
    prop_non_null = effect_opts$prop_non_null,

    # Analysis parameters
    control_group = control_mapping[advanced_opts$control_group],
    side = side_mapping[analysis_opts$side],
    multiple_testing_alpha = advanced_opts$fdr_target,

    # Power and cost parameters
    power_target = design_opts$target_power,

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
    mapping_efficiency = advanced_opts$mapping_efficiency,

    # Pilot data
    baseline_expression_stats = pilot_data$baseline_expression_stats,
    library_parameters = pilot_data$library_parameters
  )

  return(params)
}

#' Create Comprehensive Export Data
#'
#' @description Combines perturbplan results with configuration parameters
#' to create a comprehensive dataset for export with all analysis parameters,
#' including experimental design, analysis settings, and conditional cost information.
#'
#' @param perturbplan_results Data frame from perturbplan::cost_power_computation
#' @param config User configuration from sidebar modules
#' @param workflow_info Detected workflow information
#' @param pilot_data Extracted pilot data (for future extensibility)
#'
#' @return Data frame with columns: overall_power, total_cost, MOI, num_targets,
#' gRNAs_per_target, non_targeting_gRNAs, cells_per_target, reads_per_cell,
#' TPM_threshold, minimum_fold_change, side, target_power, gRNA_variability,
#' prop_non_null, mapping_efficiency, multiple_testing_alpha, control_group,
#' biological_system, cost_constraint (conditional), cost_per_captured_cell (conditional),
#' cost_per_million_reads (conditional)
#' @noRd
create_exporting_data <- function(perturbplan_results, config, workflow_info, pilot_data) {

  # Step 1: Start with perturbplan results as base data frame
  export_data <- perturbplan_results

  # Step 2: Standardize column names
  export_data$fold_change <- export_data$minimum_fold_change
  export_data$power_estimate <- export_data$overall_power
  export_data$minimum_fold_change <- NULL
  export_data$overall_power <- NULL

  # Step 3: Extract config sections
  design_opts <- config$design_options
  experimental_opts <- config$experimental_setup
  analysis_opts <- config$analysis_choices
  advanced_opts <- config$advanced_choices
  effect_opts <- config$effect_sizes

  # Step 4: Add missing columns from config (repeated for each row)
  n_rows <- nrow(export_data)

  # Core experimental parameters
  export_data$MOI <- rep(experimental_opts$MOI %||% NA, n_rows)
  export_data$num_targets <- rep(experimental_opts$num_targets %||% NA, n_rows)
  export_data$gRNAs_per_target <- rep(experimental_opts$gRNAs_per_target %||% NA, n_rows)
  export_data$non_targeting_gRNAs <- rep(experimental_opts$non_targeting_gRNAs %||% NA, n_rows)

  # Effect size and analysis parameters
  export_data$gRNA_variability <- rep(advanced_opts$gRNA_variability %||% NA, n_rows)
  export_data$prop_non_null <- rep(effect_opts$prop_non_null %||% NA, n_rows)
  export_data$mapping_efficiency <- rep(advanced_opts$mapping_efficiency %||% NA, n_rows)
  export_data$multiple_testing_alpha <- rep(advanced_opts$fdr_target %||% NA, n_rows)
  export_data$control_group <- rep(advanced_opts$control_group %||% NA, n_rows)

  # Analysis configuration parameters
  export_data$side <- rep(analysis_opts$side %||% NA, n_rows)
  export_data$biological_system <- rep(experimental_opts$biological_system %||% NA, n_rows)
  export_data$target_power <- rep(design_opts$target_power %||% NA, n_rows)

  # Cost information logic (corrected for cost minimization)
  cost_minimization_workflow <- !is.null(workflow_info$workflow_id) &&
                                workflow_info$workflow_id == "power_cost_minimization"
  power_cost_workflows <- !is.null(workflow_info$workflow_id) &&
                          workflow_info$workflow_id %in% c(
                            "power_cost_TPM_cells_reads", "power_cost_fc_cells_reads",
                            "power_cost_TPM_cells", "power_cost_TPM_reads",
                            "power_cost_fc_cells", "power_cost_fc_reads"
                          )

  has_cost_info <- cost_minimization_workflow || power_cost_workflows
  has_cost_constraint <- power_cost_workflows  # Only power+cost workflows have constraints

  if (has_cost_info) {
    # Cost information for both cost minimization AND power+cost workflows
    export_data$cost_per_captured_cell <- rep(design_opts$cost_per_cell %||% NA, n_rows)
    export_data$cost_per_million_reads <- rep(design_opts$cost_per_million_reads %||% NA, n_rows)

    if (has_cost_constraint) {
      # Cost constraint only for power+cost workflows
      export_data$cost_constraint <- rep(design_opts$cost_budget %||% NA, n_rows)
    } else {
      # Cost minimization has cost info but NO constraint
      export_data$cost_constraint <- rep(NA, n_rows)
    }
  } else {
    # Power-only workflows: no cost information at all
    export_data$total_cost <- rep(NA, n_rows)
    export_data$cost_constraint <- rep(NA, n_rows)
    export_data$cost_per_captured_cell <- rep(NA, n_rows)
    export_data$cost_per_million_reads <- rep(NA, n_rows)
  }

  # Step 5: Reorder columns for logical export structure
  column_order <- c(
    # Analysis results
    "power_estimate", "target_power",

    # Experimental design
    "MOI", "num_targets", "gRNAs_per_target", "non_targeting_gRNAs",
    "cells_per_target", "sequenced_reads_per_cell",

    # Analysis parameters
    "TPM_threshold", "fold_change", "side",

    # Effect size parameters
    "gRNA_variability", "prop_non_null",

    # Technical parameters
    "mapping_efficiency", "multiple_testing_alpha", "control_group",
    "biological_system",

    # Cost parameters (conditional)
    "total_cost", "cost_constraint", "cost_per_captured_cell", "cost_per_million_reads"
  )

  # Only select columns that exist
  existing_columns <- intersect(column_order, names(export_data))
  export_data <- export_data[existing_columns]

  return(export_data)
}
