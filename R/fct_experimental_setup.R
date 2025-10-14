#' Experimental Setup Helper Functions
#'
#' @description Pure functions extracted from mod_experimental_setup to improve
#' testability and maintainability. These functions have no side effects and can
#' be tested independently.
#'
#' @noRd
NULL

#' Get Experimental Setup Default Values
#'
#' @description Returns a list of default values for experimental setup parameters.
#' Centralizes all magic numbers and default values in one place.
#'
#' @return Named list of default values
#'
#' @noRd
get_experimental_defaults <- function() {
  list(
    # Fixed parameter defaults
    cells_fixed_default = 1000,
    reads_per_cell_fixed_default = 20000,

    # File upload limits
    max_file_size_mb = 50,

    # Perturbation parameter defaults
    MOI_default = 10,
    num_targets_default = 500,
    gRNAs_per_target_default = 4,
    non_targeting_gRNAs_default = 50,

    # Error display duration
    error_duration_seconds = 10
  )
}

#' Validate Upload File
#'
#' @description Validates uploaded file for size and format requirements.
#' Pure function with no side effects.
#'
#' @param file_info List containing file information from shiny::fileInput
#' @param max_size_mb Numeric maximum file size in megabytes
#'
#' @return List with validation results:
#'   - valid: Logical indicating if file is valid
#'   - error_message: Character error message if invalid
#'   - file_size_mb: Numeric actual file size for logging
#'
#' @noRd
validate_upload_file <- function(file_info, max_size_mb = 50) {
  if (is.null(file_info) || is.null(file_info$datapath)) {
    return(list(
      valid = FALSE,
      error_message = "No file provided",
      file_size_mb = 0
    ))
  }

  # Check file extension
  file_ext <- tolower(tools::file_ext(file_info$name))
  if (file_ext != "rds") {
    return(list(
      valid = FALSE,
      error_message = "Please upload an RDS file with the required pilot data structure",
      file_size_mb = 0
    ))
  }

  # Check file size
  file_size_mb <- file.size(file_info$datapath) / (1024^2)
  if (file_size_mb > max_size_mb) {
    return(list(
      valid = FALSE,
      error_message = paste0("File size (", round(file_size_mb, 1),
                            "MB) exceeds the ", max_size_mb, "MB limit. Please use a smaller dataset."),
      file_size_mb = file_size_mb
    ))
  }

  return(list(
    valid = TRUE,
    error_message = NULL,
    file_size_mb = file_size_mb
  ))
}

#' Format Error Message for Display
#'
#' @description Standardizes error message formatting across the module.
#' Handles different error types with consistent styling.
#'
#' @param error_type Character type of error ("file_read", "validation", "generic")
#' @param details Character additional error details
#'
#' @return Character HTML-formatted error message
#'
#' @noRd
format_error_message <- function(error_type, details = NULL) {
  base_msg <- switch(error_type,
    "file_read" = "Cannot read the uploaded file. Please ensure it's a valid RDS file.",
    "file_corrupt" = "File appears to be corrupted or not a valid RDS file. Please check the file format.",
    "file_version" = "RDS file was created with a newer version of R. Please re-save the file with your current R version.",
    "validation" = "Validation failed",
    "generic" = "An error occurred while processing the file"
  )

  if (!is.null(details)) {
    full_msg <- paste0(base_msg, ": ", details)
  } else {
    full_msg <- base_msg
  }

  # Return HTML formatted error
  paste0("<em style='color:red;'>", full_msg, "</em>")
}

#' Categorize File Reading Error
#'
#' @description Analyzes error messages to determine error type for better
#' user feedback. Pure function for error classification.
#'
#' @param error_message Character error message from tryCatch
#'
#' @return Character error type for use with format_error_message()
#'
#' @noRd
categorize_file_error <- function(error_message) {
  if (grepl("cannot open the connection", error_message, ignore.case = TRUE)) {
    return("file_read")
  } else if (grepl("magic number|format", error_message, ignore.case = TRUE)) {
    return("file_corrupt")
  } else if (grepl("version", error_message, ignore.case = TRUE)) {
    return("file_version")
  } else {
    return("generic")
  }
}

#' Assemble Experimental Configuration
#'
#' @description Pure function to build the experimental configuration object
#' from input values and defaults. No side effects.
#'
#' @param inputs List of input values from Shiny session
#' @param pilot_data List containing pilot data configuration
#' @param defaults List of default values from get_experimental_defaults()
#'
#' @return List experimental configuration object
#'
#' @noRd
assemble_experimental_config <- function(inputs, pilot_data, defaults) {
  list(
    biological_system = inputs$biological_system,
    pilot_data = pilot_data,

    # Fixed value inputs (only provide defaults if inputs are actually hidden/NULL)
    cells_fixed = inputs$cells_fixed,
    reads_per_cell_fixed = inputs$reads_per_cell_fixed,

    # Perturbation choices (with fallback to defaults)
    MOI = inputs$MOI %||% defaults$MOI_default,
    num_targets = inputs$num_targets %||% defaults$num_targets_default,
    gRNAs_per_target = inputs$gRNAs_per_target %||% defaults$gRNAs_per_target_default,
    non_targeting_gRNAs = inputs$non_targeting_gRNAs %||% defaults$non_targeting_gRNAs_default,

    timestamp = Sys.time()
  )
}

#' Build Pilot Data Configuration
#'
#' @description Pure function to build pilot data configuration based on
#' biological system selection and file upload status.
#'
#' @param biological_system Character selected biological system
#' @param pilot_data_file List file input information (can be NULL)
#' @param custom_pilot_data Validated custom pilot data (can be NULL)
#'
#' @return List pilot data configuration
#'
#' @noRd
build_pilot_data_config <- function(biological_system, pilot_data_file, custom_pilot_data) {
  if (biological_system == "Custom" && !is.null(pilot_data_file)) {
    list(
      type = "custom",
      file_path = pilot_data_file$datapath,
      file_name = pilot_data_file$name,
      data = custom_pilot_data
    )
  } else {
    list(
      type = "default",
      biological_system = biological_system
    )
  }
}

#' Format Validation Success Message
#'
#' @description Creates formatted success message for file upload with optional warnings.
#' Pure function for message formatting.
#'
#' @param summary Character validation summary message
#' @param warnings Character vector of warning messages (can be empty)
#'
#' @return Character HTML-formatted success message
#'
#' @noRd
format_success_message <- function(summary, warnings = NULL) {
  status_msg <- summary

  if (length(warnings) > 0) {
    warning_msg <- paste0("<br/><em style='color:orange;'>",
                         paste(warnings, collapse = "<br/>"),
                         "</em>")
    status_msg <- paste0(status_msg, warning_msg)
  }

  return(status_msg)
}