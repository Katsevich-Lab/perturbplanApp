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
  # Use existing solution table extraction logic with error handling
  solutions_data <- safe_extract_cached_solutions_data(cached_results)

  if (length(solutions_data) == 0) {
    return(data.frame(Message = "No solutions available"))
  }

  # Convert solutions data to clean Excel format
  solutions_df <- data.frame(
    Solution = character(),
    `Achieved Power` = numeric(),
    `Total Cost` = character(),
    `Optimal Value` = character(),
    `MOI` = numeric(),
    `Targets` = numeric(),
    `gRNAs per Target` = numeric(),
    `Cells per Target` = character(),
    `Reads per Cell` = character(),
    `TPM Threshold` = character(),
    `Fold Change` = character(),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  for (solution in solutions_data) {
    solutions_df <- rbind(solutions_df, data.frame(
      Solution = solution$solution_name %||% paste("Solution", solution$solution_id),
      `Achieved Power` = round(solution$achieved_power * 100, 1),
      `Total Cost` = if (!is.null(solution$total_cost)) paste0("$", scales::comma(solution$total_cost)) else "N/A",
      `Optimal Value` = solution$optimal_value %||% "N/A",
      `MOI` = solution$experimental_params$MOI %||% NA,
      `Targets` = solution$experimental_params$num_targets %||% NA,
      `gRNAs per Target` = solution$experimental_params$gRNAs_per_target %||% NA,
      `Cells per Target` = solution$experimental_params$cells_per_target %||% "N/A",
      `Reads per Cell` = solution$experimental_params$reads_per_cell %||% "N/A",
      `TPM Threshold` = solution$TPM_threshold %||% "N/A",
      `Fold Change` = solution$effect_sizes$minimum_fold_change %||% "N/A",
      stringsAsFactors = FALSE,
      check.names = FALSE
    ))
  }

  return(solutions_df)
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