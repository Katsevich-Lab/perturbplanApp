#' Excel Export Functions for Cached Results
#'
#' @description Functions for creating Excel export data from cached results
#' with solution table and individual power_data sheets.
#'

#' Create Complete Excel Export Data from Cached Results
#'
#' @description Main function to create Excel workbook data with Parameter Settings
#' sheet and individual data sheets for each solution.
#'
#' @param cached_results Cached results from mod_results_cache
#' @return Named list of data frames for Excel export
#' @noRd
#' @importFrom scales comma
create_excel_export_data <- function(cached_results) {
  if (is.null(cached_results)) {
    return(list("No_Data" = data.frame(Message = "No analysis results available")))
  }

  excel_data <- list()

  # Sheet 1: Parameter Settings (Solution Table)
  excel_data[["Parameter Settings"]] <- create_parameter_settings_sheet(cached_results)

  # Dynamic Data Sheets: Individual power_data for each solution
  data_sheets <- create_power_data_sheets(cached_results)
  excel_data <- c(excel_data, data_sheets)

  return(excel_data)
}

#' Create Parameter Settings Sheet
#'
#' @description Creates the first sheet with solution table summary
#' @param cached_results Cached results with current and pinned solutions
#' @return Data frame for Parameter Settings sheet
#' @noRd
create_parameter_settings_sheet <- function(cached_results) {
  # Use the exact same logic as the solution table in the app
  solutions_data <- safe_extract_cached_solutions_data(cached_results)

  if (length(solutions_data) == 0) {
    return(data.frame(Message = "No solutions available"))
  }

  # Get workflow_info from cached results (same logic as create_enhanced_solutions_table)
  workflow_info <- if (!is.null(cached_results$current_result)) {
    cached_results$current_result$workflow_info
  } else if (length(cached_results$pinned_solutions) > 0) {
    cached_results$pinned_solutions[[1]]$workflow_info
  } else {
    return(data.frame(Message = "No workflow information available"))
  }

  # Convert the solution table to Excel data frame format using the same logic as the app
  convert_solutions_table_to_excel(solutions_data, workflow_info)
}

#' Convert Solutions Table to Excel Format
#'
#' @description Converts solution table data to Excel data frame format,
#' replicating the exact same structure and logic as the app's solution table
#' @param solutions_data Structured solution data from extract_cached_solutions_data
#' @param workflow_info Workflow information containing minimizing parameter
#' @return Data frame for Excel export matching app's solution table
#' @noRd
convert_solutions_table_to_excel <- function(solutions_data, workflow_info) {
  # Get dynamic column name for optimal parameter (same logic as app)
  optimal_col_name <- get_optimal_column_name(workflow_info$minimizing_parameter)

  # Check if workflow includes cost (same logic as app)
  has_cost <- !is.null(workflow_info$workflow_id) &&
              !workflow_info$workflow_id %in% c(
                "power_single_cells_per_target", "power_single_reads_per_cell",
                "power_single_TPM_threshold", "power_single_minimum_fold_change"
              )

  # Get minimizing parameter from first solution (same logic as app)
  minimizing_param <- solutions_data[[1]]$minimizing_param

  # Build column headers exactly like the app does
  column_headers <- c("Setting", "Power")

  # Add cost column if workflow includes cost AND cost is not being minimized
  if (has_cost && minimizing_param != "cost") {
    column_headers <- c(column_headers, "Cost")
  }

  # Add optimal parameter column
  column_headers <- c(column_headers, optimal_col_name)

  # Add experimental parameter columns (always show these)
  column_headers <- c(column_headers, "MOI", "# Targets", "gRNAs/Target")

  # Add experimental parameters only if not being minimized
  if (minimizing_param != "cells_per_target") {
    column_headers <- c(column_headers, "Cells/Target")
  }
  if (minimizing_param != "reads_per_cell") {
    column_headers <- c(column_headers, "Reads/Cell")
  }

  # Add TPM Threshold if not being minimized
  if (minimizing_param != "TPM_threshold") {
    column_headers <- c(column_headers, "TPM Threshold")
  }

  # Add effect sizes columns
  if (minimizing_param != "minimum_fold_change") {
    column_headers <- c(column_headers, "Fold Change")
  }

  # Always add Non-null Prop (never minimized)
  column_headers <- c(column_headers, "Non-null Prop")

  # Create data frame with dynamic columns
  excel_df <- data.frame(matrix(ncol = length(column_headers), nrow = 0))
  colnames(excel_df) <- column_headers

  # Fill data rows using the same logic as create_data_row
  for (solution in solutions_data) {
    row_data <- list()

    # Setting name
    row_data[["Setting"]] <- solution$solution_name %||% paste("Solution", solution$solution_id)

    # Achieved power (convert to percentage)
    row_data[["Power"]] <- paste0(round(solution$achieved_power * 100, 1), "%")

    # Cost (if applicable)
    if (has_cost && minimizing_param != "cost") {
      if (!is.null(solution$total_cost)) {
        row_data[["Cost"]] <- paste0("$", format_number(solution$total_cost))
      } else {
        row_data[["Cost"]] <- "N/A"
      }
    }

    # Optimal parameter value
    row_data[[optimal_col_name]] <- solution$optimal_value %||% "N/A"

    # Experimental parameters (always show)
    row_data[["MOI"]] <- solution$experimental_params$MOI %||% "N/A"
    row_data[["# Targets"]] <- solution$experimental_params$num_targets %||% "N/A"
    row_data[["gRNAs/Target"]] <- solution$experimental_params$gRNAs_per_target %||% "N/A"

    # Conditional experimental parameters
    if (minimizing_param != "cells_per_target") {
      row_data[["Cells/Target"]] <- solution$experimental_params$cells_per_target %||% "N/A"
    }
    if (minimizing_param != "reads_per_cell") {
      row_data[["Reads/Cell"]] <- solution$experimental_params$reads_per_cell %||% "N/A"
    }

    # TPM Threshold
    if (minimizing_param != "TPM_threshold") {
      row_data[["TPM Threshold"]] <- solution$TPM_threshold %||% "N/A"
    }

    # Effect sizes
    if (minimizing_param != "minimum_fold_change") {
      row_data[["Fold Change"]] <- solution$effect_sizes$minimum_fold_change %||% "N/A"
    }

    # Non-null Prop (always shown)
    row_data[["Non-null Prop"]] <- solution$effect_sizes$non_null_prop %||% "N/A"

    # Add row to data frame
    excel_df <- rbind(excel_df, row_data, stringsAsFactors = FALSE)
  }

  return(excel_df)
}

#' Create Power Data Sheets
#'
#' @description Creates individual data sheets for each solution's power_data
#' @param cached_results Cached results with current and pinned solutions
#' @return Named list of data frames for power data sheets
#' @noRd
create_power_data_sheets <- function(cached_results) {
  data_sheets <- list()

  # Current result data sheet
  if (!is.null(cached_results$current_result) &&
      !is.null(cached_results$current_result$power_data)) {
    data_sheets[["Current_Data"]] <- cached_results$current_result$power_data
  }

  # Pinned solutions data sheets
  if (!is.null(cached_results$pinned_solutions) &&
      length(cached_results$pinned_solutions) > 0) {

    for (solution_name in names(cached_results$pinned_solutions)) {
      solution <- cached_results$pinned_solutions[[solution_name]]
      if (!is.null(solution$power_data)) {
        # Create sheet name from solution name (e.g., "Setting 1" -> "Setting_1_Data")
        sheet_name <- paste0(gsub(" ", "_", solution_name), "_Data")
        data_sheets[[sheet_name]] <- solution$power_data
      }
    }
  }

  # If no data sheets created, add placeholder
  if (length(data_sheets) == 0) {
    data_sheets[["No_Power_Data"]] <- data.frame(
      Message = "No power analysis data available"
    )
  }

  return(data_sheets)
}

#' Safe Extract Cached Solutions Data
#'
#' @description Safely extracts solutions data with error handling
#' @param cached_results Cached results object
#' @return List of solution data or empty list if error
#' @noRd
safe_extract_cached_solutions_data <- function(cached_results) {
  tryCatch({
    extract_cached_solutions_data(cached_results)
  }, error = function(e) {
    warning("Failed to extract cached solutions data: ", e$message)
    list()
  })
}

# Legacy function aliases for backward compatibility (if needed)
create_excel_summary <- function(results, plots) {
  warning("create_excel_summary is deprecated. Use create_excel_export_data with cached_results instead.")
  return(data.frame(Message = "Function deprecated"))
}

create_excel_design_options <- function(design_options) {
  warning("create_excel_design_options is deprecated. Use create_excel_export_data with cached_results instead.")
  return(data.frame(Message = "Function deprecated"))
}

create_excel_experimental_setup <- function(experimental_setup) {
  warning("create_excel_experimental_setup is deprecated. Use create_excel_export_data with cached_results instead.")
  return(data.frame(Message = "Function deprecated"))
}

create_excel_analysis_choices <- function(analysis_choices) {
  warning("create_excel_analysis_choices is deprecated. Use create_excel_export_data with cached_results instead.")
  return(data.frame(Message = "Function deprecated"))
}

create_excel_effect_sizes <- function(effect_sizes) {
  warning("create_excel_effect_sizes is deprecated. Use create_excel_export_data with cached_results instead.")
  return(data.frame(Message = "Function deprecated"))
}