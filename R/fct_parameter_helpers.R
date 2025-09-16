#' Get Parameter Ranges for Analysis
#'
#' @description Returns appropriate parameter ranges based on experimental setup
#'
#' @param biological_system Character, biological system (e.g., "K562")
#' @param experimental_platform Character, platform (e.g., "10x Chromium v3")
#'
#' @return List with parameter ranges for cells, reads, TPM_threshold, fold_change
#'
#' @noRd
get_parameter_ranges <- function(biological_system = "K562", experimental_platform = "10x Chromium v3") {
  # Default ranges - can be customized based on biological system and platform
  list(
    cells_per_target = list(min = 20, max = 2000, step = 20, default = 1000),
    reads_per_cell = list(min = 1000, max = 150000, step = 1000, default = 5000),
    TPM_threshold = list(min = 0, max = 100, step = 1, default = 10),
    fold_change = list(min = 1.1, max = 2, step = 0.1, default = 1.5)
  )
}

#' Create Parameter Grid for Analysis
#'
#' @description Creates analysis grid based on parameter controls configuration
#'
#' @param parameter_controls List of parameter control settings
#' @param ranges List of parameter ranges from get_parameter_ranges()
#'
#' @return Data frame with parameter combinations for analysis
#'
#' @noRd
create_parameter_grid <- function(parameter_controls, ranges) {
  grid_params <- list()
  
  for (param_name in names(parameter_controls)) {
    param_config <- parameter_controls[[param_name]]
    param_range <- ranges[[param_name]]
    
    if (is.null(param_config) || is.null(param_range)) {
      next
    }
    
    if (param_config$type == "fixed" && !is.null(param_config$fixed_value)) {
      # Fixed parameter - single value
      grid_params[[param_name]] <- param_config$fixed_value
    } else if (param_config$type == "minimizing") {
      # Minimizing parameter - create optimization sequence
      grid_params[[param_name]] <- seq(param_range$min, param_range$max, by = param_range$step)
    } else {
      # Varying parameter - create reasonable grid
      grid_params[[param_name]] <- seq(param_range$min, param_range$max, length.out = 5)
    }
  }
  
  # Create grid (expand.grid equivalent)
  if (length(grid_params) > 0) {
    return(do.call(expand.grid, grid_params))
  } else {
    return(data.frame())
  }
}

#' Format Parameter Values for Display
#'
#' @description Formats parameter values for user-friendly display
#'
#' @param value Numeric value to format
#' @param parameter_type Character, type of parameter
#'
#' @return Character, formatted value
#'
#' @noRd
format_parameter_value <- function(value, parameter_type) {
  switch(parameter_type,
    "cells_per_target" = paste(scales::comma(value), "cells"),
    "reads_per_cell" = paste(scales::comma(value), "reads/cell"),
    "TPM_threshold" = paste(value, "TPM"),
    "fold_change" = paste0(value, "x"),
    as.character(value)
  )
}

#' Validate Parameter Combinations
#'
#' @description Checks if parameter combinations are valid for analysis
#'
#' @param parameter_grid Data frame with parameter combinations
#'
#' @return List with is_valid (logical) and warnings (character vector)
#'
#' @noRd
validate_parameter_combinations <- function(parameter_grid) {
  warnings <- character()
  
  if (nrow(parameter_grid) == 0) {
    return(list(is_valid = FALSE, warnings = "No valid parameter combinations"))
  }
  
  if (nrow(parameter_grid) > 1000) {
    warnings <- c(warnings, "Large parameter grid may result in slow analysis")
  }
  
  # Check for unrealistic combinations
  if ("cells_per_target" %in% names(parameter_grid) && 
      "reads_per_cell" %in% names(parameter_grid)) {
    total_reads <- parameter_grid$cells_per_target * parameter_grid$reads_per_cell
    if (any(total_reads > 1e8)) {
      warnings <- c(warnings, "Some combinations result in very high total read counts")
    }
  }
  
  return(list(
    is_valid = TRUE,
    warnings = warnings
  ))
}
