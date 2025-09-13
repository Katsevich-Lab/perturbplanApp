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
#' @param plots Plot objects (unused, kept for compatibility)
#' @param user_config User configuration (unused, kept for compatibility) 
#' @return Shiny UI tagList with clean solution table
#' @export
create_enhanced_solutions_table <- function(results, plots = NULL, user_config = NULL) {
  if (is.null(results$optimal_design) || is.null(results$workflow_info)) {
    return(create_empty_solutions_table())
  }
  
  # Extract solution data using clean extraction
  solution_data <- extract_clean_solution_data(results)
  
  # Create clean table UI with proper two-row header
  create_clean_solutions_table_ui(solution_data, results$workflow_info)
}

# ============================================================================
# DATA EXTRACTION
# ============================================================================

#' Extract Clean Solution Data
#'
#' @param results Complete analysis results object
#' @return List with structured solution data
extract_clean_solution_data <- function(results) {
  optimal <- results$optimal_design
  user_config <- results$user_config
  workflow_info <- results$workflow_info
  minimizing_param <- workflow_info$minimizing_parameter
  
  list(
    solution_id = 1,
    achieved_power = round(optimal$power %||% 0.8, 3),
    total_cost = if (!is.null(optimal$total_cost)) optimal$total_cost else NULL,
    optimal_value = extract_optimal_parameter_value(optimal, minimizing_param),
    experimental_params = extract_experimental_parameters(optimal, user_config, minimizing_param),
    tpm_threshold = extract_tpm_threshold(optimal, user_config, minimizing_param),
    effect_sizes = extract_effect_sizes_clean(optimal, user_config, minimizing_param),
    minimizing_param = minimizing_param
  )
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
extract_experimental_parameters <- function(optimal, user_config, minimizing_param) {
  list(
    moi = get_param_value("MOI", optimal, user_config, minimizing_param),
    num_targets = get_param_value("num_targets", optimal, user_config, minimizing_param),
    grnas_per_target = get_param_value("gRNAs_per_target", optimal, user_config, minimizing_param),
    cells_per_target = get_param_value("cells_fixed", optimal, user_config, minimizing_param),
    reads_per_cell = get_param_value("reads_per_cell_fixed", optimal, user_config, minimizing_param)
  )
}

#' Extract TPM Threshold
extract_tpm_threshold <- function(optimal, user_config, minimizing_param) {
  if (minimizing_param == "TPM_threshold") {
    round(optimal$TPM_threshold %||% NA)
  } else {
    value <- user_config$analysis_choices$TPM_threshold %||% NA
    if (!is.na(value)) round(value) else "N/A"
  }
}

#' Extract Effect Sizes (Clean)
extract_effect_sizes_clean <- function(optimal, user_config, minimizing_param) {
  # Fold change
  if (minimizing_param == "minimum_fold_change") {
    fold_change <- round(optimal$minimum_fold_change %||% NA, 2)
  } else {
    fold_change <- round(user_config$effect_sizes$minimum_fold_change %||% NA, 2)
  }
  
  # Non-null proportion (always from user config)
  non_null_prop <- round(user_config$effect_sizes$prop_non_null %||% NA, 2)
  
  list(
    fold_change = if (is.na(fold_change)) "N/A" else fold_change,
    non_null_proportion = if (is.na(non_null_prop)) "N/A" else non_null_prop
  )
}

# ============================================================================
# UTILITY FUNCTIONS
# ============================================================================

#' Get Parameter Value (Optimal vs User Config)
get_param_value <- function(param_name, optimal, user_config, minimizing_param) {
  if (minimizing_param == param_name) {
    # Get optimal value
    value <- optimal[[param_name]]
    if (param_name %in% c("cells_fixed", "num_targets")) {
      format_number(value %||% NA)
    } else if (param_name == "reads_per_cell_fixed") {
      format_number(optimal$sequenced_reads_per_cell %||% optimal$reads_per_cell %||% NA)
    } else {
      value %||% "N/A"
    }
  } else {
    # Get fixed value from user config
    value <- user_config$experimental_setup[[param_name]] %||% 
             user_config$perturbation_choices[[param_name]] %||% NA
    if (param_name %in% c("cells_fixed", "num_targets")) {
      format_number(value %||% NA)
    } else if (param_name == "reads_per_cell_fixed") {
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
    as.character(round(x, 2))
  }
}

# ============================================================================
# TABLE UI CREATION
# ============================================================================

#' Create Clean Solutions Table UI
create_clean_solutions_table_ui <- function(solution_data, workflow_info) {
  
  # Get dynamic column name for optimal parameter
  optimal_col_name <- get_optimal_column_name(workflow_info$minimizing_parameter)
  
  # Check if workflow includes cost (7 workflows have cost, 4 are power-only)
  has_cost <- !is.null(workflow_info$workflow_id) && 
              !workflow_info$workflow_id %in% c(
                "power_single_cells_per_target", "power_single_reads_per_cell",
                "power_single_TPM_threshold", "power_single_minimum_fold_change"
              )
  
  # Create two-row header
  header_row1 <- create_header_row1(optimal_col_name, has_cost)
  header_row2 <- create_header_row2(has_cost = has_cost)
  
  # Create data row
  data_row <- create_data_row(solution_data, workflow_info)
  
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
        data_row
      )
    )
  )
}

#' Create Header Row 1 (Main Categories)
create_header_row1 <- function(optimal_col_name, has_cost = NULL) {
  
  cells <- list(
    tags$th("Solution ID", style = "text-align: center; font-weight: bold; border-bottom: 1px solid #dee2e6; padding: 8px;"),
    tags$th("Power", style = "text-align: center; font-weight: bold; border-bottom: 1px solid #dee2e6; padding: 8px;")
  )
  
  # Add cost column if workflow includes cost
  if (has_cost) {
    cells <- append(cells, list(
      tags$th("Cost", style = "text-align: center; font-weight: bold; border-bottom: 1px solid #dee2e6; padding: 8px;")
    ))
  }
  
  # Add remaining columns
  cells <- append(cells, list(
    tags$th(optimal_col_name, style = "text-align: center; font-weight: bold; border-bottom: 1px solid #dee2e6; padding: 8px;"),
    tags$th("Experimental Parameters", colspan = "5", style = "text-align: center; font-weight: bold; border-bottom: 1px solid #dee2e6; padding: 8px;"),
    tags$th("TPM Threshold", style = "text-align: center; font-weight: bold; border-bottom: 1px solid #dee2e6; padding: 8px;"),
    tags$th("Effect Sizes", colspan = "2", style = "text-align: center; font-weight: bold; border-bottom: 1px solid #dee2e6; padding: 8px;")
  ))
  
  tags$tr(cells)
}

#' Create Header Row 2 (Subcolumns)
create_header_row2 <- function(has_cost = NULL) {
  cells <- list(
    tags$th("", style = "border-top: none; border-bottom: 1px solid #dee2e6; padding: 6px;"), # Solution ID
    tags$th("", style = "border-top: none; border-bottom: 1px solid #dee2e6; padding: 6px;")  # Power
  )
  
  # Add cost column if workflow includes cost
  if (has_cost) {
    cells <- append(cells, list(
      tags$th("", style = "border-top: none; border-bottom: 1px solid #dee2e6; padding: 6px;") # Cost
    ))
  }
  
  # Add remaining columns  
  cells <- append(cells, list(
    tags$th("", style = "border-top: none; border-bottom: 1px solid #dee2e6; padding: 6px;"), # Optimal param
    # Experimental parameter subcolumns
    tags$th("MOI", style = "text-align: center; font-size: 11px; border-top: none; border-bottom: 1px solid #dee2e6; padding: 6px;"),
    tags$th("# Targets", style = "text-align: center; font-size: 11px; border-top: none; border-bottom: 1px solid #dee2e6; padding: 6px;"),
    tags$th("gRNAs/Target", style = "text-align: center; font-size: 11px; border-top: none; border-bottom: 1px solid #dee2e6; padding: 6px;"),
    tags$th("Cells/Target", style = "text-align: center; font-size: 11px; border-top: none; border-bottom: 1px solid #dee2e6; padding: 6px;"),
    tags$th("Reads/Cell", style = "text-align: center; font-size: 11px; border-top: none; border-bottom: 1px solid #dee2e6; padding: 6px;"),
    tags$th("", style = "border-top: none; border-bottom: 1px solid #dee2e6; padding: 6px;"), # TPM threshold
    # Effect size subcolumns
    tags$th("Fold Change", style = "text-align: center; font-size: 11px; border-top: none; border-bottom: 1px solid #dee2e6; padding: 6px;"),
    tags$th("Non-null Prop", style = "text-align: center; font-size: 11px; border-top: none; border-bottom: 1px solid #dee2e6; padding: 6px;")
  ))
  
  tags$tr(cells)
}

#' Create Data Row
create_data_row <- function(solution_data, workflow_info) {
  
  cells <- list(
    tags$td(solution_data$solution_id, style = "text-align: center; padding: 8px;"),
    tags$td(solution_data$achieved_power, style = "text-align: center; padding: 8px;")
  )
  
  # Add cost if workflow includes cost
  has_cost <- !is.null(workflow_info$workflow_id) && 
              !workflow_info$workflow_id %in% c(
                "power_single_cells_per_target", "power_single_reads_per_cell",
                "power_single_TPM_threshold", "power_single_minimum_fold_change"
              )
  
  if (has_cost && !is.null(solution_data$total_cost)) {
    cells <- append(cells, list(
      tags$td(paste0("$", format_number(solution_data$total_cost)), style = "text-align: center; padding: 8px;")
    ))
  }
  
  # Add remaining columns
  exp_params <- solution_data$experimental_params
  effect_sizes <- solution_data$effect_sizes
  
  cells <- append(cells, list(
    tags$td(solution_data$optimal_value, style = "text-align: center; font-weight: bold; padding: 8px;"),
    # Experimental parameters
    tags$td(exp_params$moi %||% "N/A", style = "text-align: center; padding: 8px;"),
    tags$td(exp_params$num_targets %||% "N/A", style = "text-align: center; padding: 8px;"),
    tags$td(exp_params$grnas_per_target %||% "N/A", style = "text-align: center; padding: 8px;"),
    tags$td(exp_params$cells_per_target %||% "N/A", style = "text-align: center; padding: 8px;"),
    tags$td(exp_params$reads_per_cell %||% "N/A", style = "text-align: center; padding: 8px;"),
    # TPM threshold
    tags$td(solution_data$tpm_threshold %||% "N/A", style = "text-align: center; padding: 8px;"),
    # Effect sizes
    tags$td(effect_sizes$fold_change %||% "N/A", style = "text-align: center; padding: 8px;"),
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