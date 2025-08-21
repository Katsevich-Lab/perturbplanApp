#' Pilot Data Validation Functions
#'
#' @description Functions for validating custom pilot data uploads
#' in the expected structure for perturbplan integration.
#'
#' @name pilot-data-validation
NULL

#' Validate custom pilot data structure
#'
#' @description Main validation function for custom pilot data uploads.
#' Validates the overall structure and delegates to component validators.
#'
#' @param data A list object loaded from an RDS file
#' @param file_name Character. Name of the uploaded file for error messages
#' @return List with validation results
#' @noRd
validate_custom_pilot_data <- function(data, file_name = "uploaded file") {
  errors <- character(0)
  warnings <- character(0)
  
  # Check if data is a list
  if (!is.list(data)) {
    errors <- c(errors, "Data must be a list object")
    return(list(
      valid = FALSE,
      data = NULL,
      errors = errors,
      warnings = warnings,
      summary = paste0("<em style='color:red;'>Invalid file structure</em>")
    ))
  }
  
  # Check for required top-level components
  required_components <- c("baseline_expression_stats", "library_parameters")
  missing_components <- setdiff(required_components, names(data))
  
  if (length(missing_components) > 0) {
    errors <- c(errors, paste("Missing required components:", 
                             paste(missing_components, collapse = ", ")))
  }
  
  # Check for unexpected components
  extra_components <- setdiff(names(data), required_components)
  if (length(extra_components) > 0) {
    warnings <- c(warnings, paste("Unexpected components will be ignored:", 
                                 paste(extra_components, collapse = ", ")))
  }
  
  # Validate baseline expression stats if present
  if ("baseline_expression_stats" %in% names(data)) {
    baseline_validation <- validate_baseline_expression_stats(data$baseline_expression_stats)
    errors <- c(errors, baseline_validation$errors)
    warnings <- c(warnings, baseline_validation$warnings)
  }
  
  # Validate library parameters if present
  if ("library_parameters" %in% names(data)) {
    library_validation <- validate_library_parameters(data$library_parameters)
    errors <- c(errors, library_validation$errors)
    warnings <- c(warnings, library_validation$warnings)
  }
  
  # Determine if validation passed
  valid <- length(errors) == 0
  
  # Create summary message
  if (valid) {
    n_genes <- nrow(data$baseline_expression_stats)
    umi_per_cell <- data$library_parameters$UMI_per_cell
    variation <- data$library_parameters$variation
    
    summary <- sprintf("Successfully loaded custom pilot data: %d genes, UMI/cell = %.0f, variation = %.3f", 
                      n_genes, umi_per_cell, variation)
  } else {
    summary <- paste0("<em style='color:red;'>Validation failed</em>")
  }
  
  return(list(
    valid = valid,
    data = if (valid) data else NULL,
    errors = errors,
    warnings = warnings,
    summary = summary
  ))
}

#' Validate baseline expression stats dataframe
#'
#' @description Validates the baseline_expression_stats component
#' 
#' @param baseline_df Data frame with baseline expression statistics
#' @return List with errors and warnings
#' @noRd
validate_baseline_expression_stats <- function(baseline_df) {
  errors <- character(0)
  warnings <- character(0)
  
  # Check if it's a data frame
  if (!is.data.frame(baseline_df)) {
    errors <- c(errors, "baseline_expression_stats must be a data frame")
    return(list(errors = errors, warnings = warnings))
  }
  
  # Check required columns
  required_cols <- c("response_id", "relative_expression", "expression_size")
  missing_cols <- setdiff(required_cols, names(baseline_df))
  
  if (length(missing_cols) > 0) {
    errors <- c(errors, paste("Missing required columns in baseline_expression_stats:", 
                             paste(missing_cols, collapse = ", ")))
  }
  
  # Check column types and values
  if ("response_id" %in% names(baseline_df)) {
    if (!is.character(baseline_df$response_id)) {
      errors <- c(errors, "response_id column must be character type")
    }
    
    # Check for duplicates
    if (any(duplicated(baseline_df$response_id))) {
      warnings <- c(warnings, "Duplicate response_ids found in baseline_expression_stats")
    }
    
    # Check for empty strings
    if (any(is.na(baseline_df$response_id) | baseline_df$response_id == "")) {
      errors <- c(errors, "response_id column contains missing or empty values")
    }
  }
  
  if ("relative_expression" %in% names(baseline_df)) {
    if (!is.numeric(baseline_df$relative_expression)) {
      errors <- c(errors, "relative_expression column must be numeric")
    } else {
      # Check for negative values
      if (any(baseline_df$relative_expression < 0, na.rm = TRUE)) {
        errors <- c(errors, "relative_expression contains negative values")
      }
      
      # Check for reasonable range (TPM/1e6 scale)
      max_expr <- max(baseline_df$relative_expression, na.rm = TRUE)
      if (max_expr > 1) {
        warnings <- c(warnings, sprintf("Maximum relative_expression (%.3f) seems high for TPM/1e6 scale", max_expr))
      }
    }
  }
  
  if ("expression_size" %in% names(baseline_df)) {
    if (!is.numeric(baseline_df$expression_size)) {
      errors <- c(errors, "expression_size column must be numeric")
    } else {
      # Check for negative values
      if (any(baseline_df$expression_size <= 0, na.rm = TRUE)) {
        errors <- c(errors, "expression_size contains non-positive values")
      }
    }
  }
  
  # Check number of rows
  if (nrow(baseline_df) == 0) {
    errors <- c(errors, "baseline_expression_stats is empty")
  } else if (nrow(baseline_df) < 1000) {
    warnings <- c(warnings, sprintf("Only %d genes in baseline_expression_stats (recommend >1000)", nrow(baseline_df)))
  }
  
  return(list(errors = errors, warnings = warnings))
}

#' Validate library parameters list
#'
#' @description Validates the library_parameters component
#' 
#' @param library_params List with library parameters
#' @return List with errors and warnings  
#' @noRd
validate_library_parameters <- function(library_params) {
  errors <- character(0)
  warnings <- character(0)
  
  # Check if it's a list
  if (!is.list(library_params)) {
    errors <- c(errors, "library_parameters must be a list")
    return(list(errors = errors, warnings = warnings))
  }
  
  # Check required parameters
  required_params <- c("UMI_per_cell", "variation")
  missing_params <- setdiff(required_params, names(library_params))
  
  if (length(missing_params) > 0) {
    errors <- c(errors, paste("Missing required parameters in library_parameters:", 
                             paste(missing_params, collapse = ", ")))
  }
  
  # Validate UMI_per_cell
  if ("UMI_per_cell" %in% names(library_params)) {
    umi_val <- library_params$UMI_per_cell
    
    if (!is.numeric(umi_val) || length(umi_val) != 1) {
      errors <- c(errors, "UMI_per_cell must be a single numeric value")
    } else {
      if (umi_val <= 0) {
        errors <- c(errors, "UMI_per_cell must be positive")
      } else if (umi_val < 1000) {
        warnings <- c(warnings, sprintf("UMI_per_cell (%.0f) seems low for modern sequencing", umi_val))
      } else if (umi_val > 200000) {
        warnings <- c(warnings, sprintf("UMI_per_cell (%.0f) seems very high", umi_val))
      }
    }
  }
  
  # Validate variation
  if ("variation" %in% names(library_params)) {
    var_val <- library_params$variation
    
    if (!is.numeric(var_val) || length(var_val) != 1) {
      errors <- c(errors, "variation must be a single numeric value")
    } else {
      if (var_val <= 0) {
        errors <- c(errors, "variation must be positive")
      } else if (var_val > 2) {
        warnings <- c(warnings, sprintf("variation (%.3f) seems high (typically 0.1-1.0)", var_val))
      }
    }
  }
  
  return(list(errors = errors, warnings = warnings))
}