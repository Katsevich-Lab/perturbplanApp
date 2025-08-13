#' Validate File Upload
#'
#' @description Validates uploaded files for format and content
#'
#' @param file_info File info from fileInput (datapath, name, size, type)
#' @param expected_format Character, expected file format ("csv", "rds")
#' @param max_size_mb Numeric, maximum file size in MB
#'
#' @return List with is_valid (logical), data (if valid), and errors (character vector)
#'
#' @importFrom utils read.csv
#'
#' @noRd
validate_file_upload <- function(file_info, expected_format, max_size_mb = 50) {
  errors <- character()
  
  if (is.null(file_info)) {
    return(list(is_valid = FALSE, data = NULL, errors = "No file uploaded"))
  }
  
  # Check file size
  if (file_info$size > max_size_mb * 1024^2) {
    errors <- c(errors, paste("File size exceeds", max_size_mb, "MB limit"))
  }
  
  # Check file extension
  file_ext <- tools::file_ext(file_info$name)
  if (tolower(file_ext) != tolower(expected_format)) {
    errors <- c(errors, paste("Expected", expected_format, "file, got", file_ext))
  }
  
  if (length(errors) > 0) {
    return(list(is_valid = FALSE, data = NULL, errors = errors))
  }
  
  # Try to load the file
  tryCatch({
    if (expected_format == "csv") {
      data <- read.csv(file_info$datapath, stringsAsFactors = FALSE)
    } else if (expected_format == "rds") {
      data <- readRDS(file_info$datapath)
    } else {
      errors <- c(errors, paste("Unsupported format:", expected_format))
      return(list(is_valid = FALSE, data = NULL, errors = errors))
    }
    
    return(list(is_valid = TRUE, data = data, errors = character()))
    
  }, error = function(e) {
    return(list(is_valid = FALSE, data = NULL, errors = paste("Failed to read file:", e$message)))
  })
}

#' Validate Gene List CSV
#'
#' @description Validates gene list CSV format and content
#'
#' @param data Data frame from CSV file
#'
#' @return List with is_valid (logical) and errors (character vector)
#'
#' @noRd
validate_gene_list_csv <- function(data) {
  errors <- character()
  
  # Check required columns
  required_cols <- c("grna_target", "response_id")
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    errors <- c(errors, paste("Missing required columns:", paste(missing_cols, collapse = ", ")))
  }
  
  if (length(errors) > 0) {
    return(list(is_valid = FALSE, errors = errors))
  }
  
  # Check for empty data
  if (nrow(data) == 0) {
    errors <- c(errors, "CSV file is empty")
  }
  
  # Check for missing values
  if (any(is.na(data$grna_target) | data$grna_target == "")) {
    errors <- c(errors, "Missing values in grna_target column")
  }
  
  if (any(is.na(data$response_id) | data$response_id == "")) {
    errors <- c(errors, "Missing values in response_id column")
  }
  
  # Check gene ID format (should be Ensembl IDs)
  if (!"response_id" %in% names(data)) {
    # Already checked above, but being explicit
  } else {
    non_ensembl <- !grepl("^ENS[A-Z]*G\\d{11}$", data$response_id)
    if (any(non_ensembl)) {
      errors <- c(errors, paste("Some response_id values are not valid Ensembl gene IDs"))
    }
  }
  
  return(list(is_valid = length(errors) == 0, errors = errors))
}

#' Validate Combined Pilot Data RDS
#'
#' @description Validates combined pilot data RDS file structure
#'
#' @param data Object loaded from RDS file
#'
#' @return List with is_valid (logical) and errors (character vector)
#'
#' @noRd
validate_pilot_data_rds <- function(data) {
  errors <- character()
  
  # Check top-level structure
  if (!is.list(data)) {
    errors <- c(errors, "RDS file must contain a list")
    return(list(is_valid = FALSE, errors = errors))
  }
  
  required_elements <- c("baseline_expression", "library_parameters")
  missing_elements <- setdiff(required_elements, names(data))
  if (length(missing_elements) > 0) {
    errors <- c(errors, paste("Missing required elements:", paste(missing_elements, collapse = ", ")))
  }
  
  # Validate baseline_expression
  if ("baseline_expression" %in% names(data)) {
    baseline <- data$baseline_expression
    if (!is.list(baseline)) {
      errors <- c(errors, "baseline_expression must be a list")
    } else {
      # Check for required components
      if (!"baseline_expression" %in% names(baseline) || !is.data.frame(baseline$baseline_expression)) {
        errors <- c(errors, "baseline_expression must contain a data.frame named 'baseline_expression'")
      }
      
      if (!"expression_dispersion_curve" %in% names(baseline) || !is.function(baseline$expression_dispersion_curve)) {
        errors <- c(errors, "baseline_expression must contain a function named 'expression_dispersion_curve'")
      }
    }
  }
  
  # Validate library_parameters
  if ("library_parameters" %in% names(data)) {
    library_params <- data$library_parameters
    if (!is.list(library_params)) {
      errors <- c(errors, "library_parameters must be a list")
    } else {
      required_params <- c("UMI_per_cell", "variation")
      missing_params <- setdiff(required_params, names(library_params))
      if (length(missing_params) > 0) {
        errors <- c(errors, paste("Missing library parameters:", paste(missing_params, collapse = ", ")))
      }
      
      # Check parameter types and values
      if ("UMI_per_cell" %in% names(library_params)) {
        if (!is.numeric(library_params$UMI_per_cell) || library_params$UMI_per_cell <= 0) {
          errors <- c(errors, "UMI_per_cell must be a positive number")
        }
      }
      
      if ("variation" %in% names(library_params)) {
        if (!is.numeric(library_params$variation) || library_params$variation <= 0) {
          errors <- c(errors, "variation must be a positive number")
        }
      }
    }
  }
  
  return(list(is_valid = length(errors) == 0, errors = errors))
}
