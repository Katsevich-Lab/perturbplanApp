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

  # Sheet 2: Pilot Data (baseline expression + library parameters)
  excel_data[["Pilot Data"]] <- create_pilot_data_sheet(cached_results)

  # Sheet 3+: Dynamic Data Sheets (Individual analysis results for each solution)
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
    cached_results$current_result$user_config$workflow_info
  } else if (length(cached_results$pinned_solutions) > 0) {
    cached_results$pinned_solutions[[1]]$user_config$workflow_info
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
  # Extract assay_type from first solution (all solutions have same assay type)
  assay_type <- solutions_data[[1]]$assay_type

  # Get dynamic column name for optimal parameter (assay-aware, same logic as app)
  optimal_col_name <- get_optimal_column_name(workflow_info$minimizing_parameter, assay_type)

  # Check if workflow includes cost (same logic as app)
  has_cost <- !is.null(workflow_info$workflow_id) &&
              !workflow_info$workflow_id %in% c(
                "power_single_cells_per_target", "power_single_reads_per_cell",
                "power_single_TPM_threshold", "power_single_minimum_fold_change"
              )

  # Get minimizing parameter from first solution (same logic as app)
  minimizing_param <- solutions_data[[1]]$minimizing_param

  # Build column headers (simplified names to avoid Excel conversion issues)
  column_headers <- c("Setting", "Power")

  # Add cost column if workflow includes cost AND cost is not being minimized
  if (has_cost && minimizing_param != "cost") {
    column_headers <- c(column_headers, "Cost")
  }

  # Add optimal parameter column (simplified)
  optimal_col_simple <- gsub("[/\\s]+", "_", optimal_col_name)
  column_headers <- c(column_headers, optimal_col_simple)

  # Add experimental parameter columns (always show these, simplified names)
  column_headers <- c(column_headers, "MOI", "Targets", "gRNAs_per_Target")

  # Add experimental parameters only if not being minimized
  if (minimizing_param != "cells_per_target") {
    column_headers <- c(column_headers, "Cells_per_Target")
  }
  if (minimizing_param != "reads_per_cell") {
    column_headers <- c(column_headers, "Reads_per_Cell")
  }

  # Add Expression Threshold if not being minimized (assay-aware column name)
  if (minimizing_param != "TPM_threshold") {
    # Assay-aware column header
    expression_col_name <- if (!is.null(assay_type) && assay_type == "tap_seq") {
      "UMIs_cell_threshold"
    } else {
      "TPM_Threshold"
    }
    column_headers <- c(column_headers, expression_col_name)
  }

  # Add effect sizes columns
  if (minimizing_param != "minimum_fold_change") {
    column_headers <- c(column_headers, "Fold_Change")
  }

  # Always add Non-null Prop (never minimized)
  column_headers <- c(column_headers, "Non_null_Prop")

  # Initialize empty data frame with all columns
  excel_df_rows <- list()

  # Fill data rows using the same logic as create_data_row
  for (i in seq_along(solutions_data)) {
    solution <- solutions_data[[i]]
    row_data <- vector("list", length(column_headers))
    names(row_data) <- column_headers

    # Initialize all columns with "N/A"
    for (col in column_headers) {
      row_data[[col]] <- "N/A"
    }

    # Setting name (exact same logic as create_data_row)
    row_data[["Setting"]] <- solution$solution_name %||% paste("Solution", solution$solution_id)

    # Achieved power (format as percentage like in the app)
    power_value <- solution$achieved_power
    if (!is.null(power_value) && is.numeric(power_value)) {
      row_data[["Power"]] <- paste0(round(power_value * 100, 1), "%")
    }

    # Cost (if applicable) - exact same logic as create_data_row
    if (has_cost && minimizing_param != "cost") {
      if (!is.null(solution$total_cost)) {
        row_data[["Cost"]] <- paste0("$", format_number(solution$total_cost))
      }
    }

    # Optimal parameter value
    if (optimal_col_simple %in% column_headers) {
      row_data[[optimal_col_simple]] <- solution$optimal_value %||% "N/A"
    }

    # Experimental parameters (exact same logic as create_data_row)
    exp_params <- solution$experimental_params
    if (!is.null(exp_params)) {
      row_data[["MOI"]] <- exp_params$moi %||% "N/A"
      row_data[["Targets"]] <- exp_params$num_targets %||% "N/A"
      row_data[["gRNAs_per_Target"]] <- exp_params$grnas_per_target %||% "N/A"

      # Conditional experimental parameters (exact same logic as create_data_row)
      if (minimizing_param != "cells_per_target" && "Cells_per_Target" %in% column_headers) {
        row_data[["Cells_per_Target"]] <- exp_params$cells_per_target %||% "N/A"
      }
      if (minimizing_param != "reads_per_cell" && "Reads_per_Cell" %in% column_headers) {
        row_data[["Reads_per_Cell"]] <- exp_params$reads_per_cell %||% "N/A"
      }
    }

    # Expression Threshold (assay-aware column name)
    if (minimizing_param != "TPM_threshold") {
      # Use the assay-aware column name determined earlier
      expr_col <- if (!is.null(assay_type) && assay_type == "tap_seq") {
        "UMIs_cell_threshold"
      } else {
        "TPM_Threshold"
      }
      if (expr_col %in% column_headers) {
        row_data[[expr_col]] <- solution$TPM_threshold %||% "N/A"
      }
    }

    # Effect sizes (exact same logic as create_data_row)
    effect_sizes <- solution$effect_sizes
    if (!is.null(effect_sizes)) {
      if (minimizing_param != "minimum_fold_change" && "Fold_Change" %in% column_headers) {
        row_data[["Fold_Change"]] <- effect_sizes$fold_change %||% "N/A"
      }
      # Non-null Prop (always shown, exact same logic as create_data_row)
      if ("Non_null_Prop" %in% column_headers) {
        row_data[["Non_null_Prop"]] <- effect_sizes$non_null_proportion %||% "N/A"
      }
    }

    # Add row to list
    excel_df_rows[[i]] <- row_data
  }

  return(do.call(rbind.data.frame, excel_df_rows))
}

#' Create Power Data Sheets
#'
#' @description Creates individual data sheets for each solution's comprehensive analysis data.
#' Prioritizes exporting_data (comprehensive) over power_data (basic) for richer export content.
#' @param cached_results Cached results with current and pinned solutions
#' @return Named list of data frames for analysis data sheets
#' @noRd
create_power_data_sheets <- function(cached_results) {
  data_sheets <- list()

  # Current result data sheet - use exporting_data if available, fallback to power_data
  if (!is.null(cached_results$current_result)) {
    if (!is.null(cached_results$current_result$exporting_data)) {
      data_sheets[["Current_Data"]] <- cached_results$current_result$exporting_data
    } else if (!is.null(cached_results$current_result$power_data)) {
      data_sheets[["Current_Data"]] <- cached_results$current_result$power_data
    }
  }

  # Pinned solutions data sheets - use exporting_data if available, fallback to power_data
  if (!is.null(cached_results$pinned_solutions) &&
      length(cached_results$pinned_solutions) > 0) {

    for (solution_name in names(cached_results$pinned_solutions)) {
      solution <- cached_results$pinned_solutions[[solution_name]]
      if (!is.null(solution$exporting_data)) {
        # Create sheet name from solution name (e.g., "Setting 1" -> "Setting_1_Data")
        sheet_name <- paste0(gsub(" ", "_", solution_name), "_Data")
        data_sheets[[sheet_name]] <- solution$exporting_data
      } else if (!is.null(solution$power_data)) {
        # Fallback to power_data for backward compatibility
        sheet_name <- paste0(gsub(" ", "_", solution_name), "_Data")
        data_sheets[[sheet_name]] <- solution$power_data
      }
    }
  }

  # If no data sheets created, add placeholder
  if (length(data_sheets) == 0) {
    data_sheets[["No_Export_Data"]] <- data.frame(
      Message = "No analysis data available for export"
    )
  }

  return(data_sheets)
}

#' Create Pilot Data Sheet
#'
#' @description Creates pilot data sheet with both baseline expression stats and library parameters
#' as separate sections in one sheet (Option C format)
#' @param cached_results Cached results with pilot data information
#' @return Data frame for pilot data sheet
#' @noRd
create_pilot_data_sheet <- function(cached_results) {
  # Extract pilot data from current result, with fallback to pinned solutions
  pilot_data <- NULL

  # Try current result first
  if (!is.null(cached_results$current_result)) {
    pilot_data <- cached_results$current_result$pilot_data
  }

  # If not found, try first pinned solution
  if (is.null(pilot_data) && !is.null(cached_results$pinned_solutions) && length(cached_results$pinned_solutions) > 0) {
    first_pinned <- cached_results$pinned_solutions[[1]]
    pilot_data <- first_pinned$pilot_data
  }

  # If pilot data still not available, return debug message
  if (is.null(pilot_data)) {
    debug_info <- data.frame(
      Debug = c(
        "No pilot data available",
        paste("Current result exists:", !is.null(cached_results$current_result)),
        paste("Pinned solutions count:", length(cached_results$pinned_solutions %||% list())),
        paste("Current result fields:", paste(names(cached_results$current_result %||% list()), collapse = ", "))
      ),
      stringsAsFactors = FALSE
    )
    return(debug_info)
  }

  # Create combined sheet with baseline expression stats and library parameters as additional columns

  # Start with baseline expression stats
  baseline_stats <- pilot_data$baseline_expression_stats
  if (is.null(baseline_stats) || nrow(baseline_stats) == 0) {
    return(data.frame(Message = "No baseline expression stats available"))
  }

  # Start with baseline expression data
  combined_data <- baseline_stats

  # Add library parameters as additional columns on the right side
  library_params <- pilot_data$library_parameters
  if (!is.null(library_params) && length(library_params) > 0) {

    # Create library parameters columns - we'll add them row by row
    param_names <- names(library_params)
    param_values <- as.character(unlist(library_params))

    # Add empty separator column
    combined_data[["  "]] <- ""  # Empty column as visual separator

    # Add library parameters columns - use meaningful column names
    combined_data[["Library Parameter"]] <- ""
    combined_data[["Value"]] <- ""

    # Fill in library parameters starting from the first row (no separate header needed)
    for (i in seq_along(param_names)) {
      if (i <= nrow(combined_data)) {
        combined_data[i, "Library Parameter"] <- param_names[i]
        combined_data[i, "Value"] <- param_values[i]
      }
    }

    # If we have more parameters than available rows, add more rows
    if (length(param_names) > nrow(combined_data)) {
      extra_rows_needed <- length(param_names) - nrow(combined_data)
      for (j in 1:extra_rows_needed) {
        new_row <- combined_data[1, ]
        new_row[1, ] <- NA  # Fill baseline stats columns with NA
        param_index <- nrow(combined_data) + j
        new_row[1, "Library Parameter"] <- param_names[param_index]
        new_row[1, "Value"] <- param_values[param_index]
        combined_data <- rbind(combined_data, new_row)
      }
    }

  } else {
    # No library parameters available
    combined_data[["  "]] <- ""  # Empty separator column
    combined_data[["Library Parameter"]] <- ""
    combined_data[["Value"]] <- ""
    combined_data[1, "Library Parameter"] <- "Not available"
    combined_data[1, "Value"] <- ""
  }

  return(combined_data)
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
