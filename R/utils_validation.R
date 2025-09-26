#' Validate Gene List RDS File
#'
#' @description Validates RDS file containing gene pairs for custom analysis
#'
#' @param file_path Path to the RDS file to validate
#'
#' @return List with is_valid (logical), errors (character vector), and data (if valid)
#'
#' @examples
#' \dontrun{
#' result <- validate_gene_list_rds("gene_pairs.rds")
#' if (result$is_valid) {
#'   gene_data <- result$data
#' }
#' }
#'
#' @noRd
validate_gene_list_rds <- function(file_path) {
  errors <- character()
  data <- NULL

  # Try to read RDS file
  tryCatch({
    data <- readRDS(file_path)
  }, error = function(e) {
    errors <<- c(errors, paste("Failed to read RDS file:", e$message))
    return(list(is_valid = FALSE, errors = errors, data = NULL))
  })

  if (length(errors) > 0) {
    return(list(is_valid = FALSE, errors = errors, data = NULL))
  }

  # Check if data is a data frame
  if (!is.data.frame(data)) {
    errors <- c(errors, "RDS file must contain a data frame")
    return(list(is_valid = FALSE, errors = errors, data = NULL))
  }

  # Check required columns
  required_cols <- c("grna_target", "response_id")
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    errors <- c(errors, paste("Missing required columns:", paste(missing_cols, collapse = ", ")))
  }

  if (length(errors) > 0) {
    return(list(is_valid = FALSE, errors = errors, data = NULL))
  }

  # Check for empty data
  if (nrow(data) == 0) {
    errors <- c(errors, "RDS file contains empty data frame")
  }

  # Check for missing values
  if (any(is.na(data$grna_target) | data$grna_target == "")) {
    errors <- c(errors, "Missing values in grna_target column")
  }

  if (any(is.na(data$response_id) | data$response_id == "")) {
    errors <- c(errors, "Missing values in response_id column")
  }

  # Check gene ID format (should be Ensembl IDs)
  non_ensembl <- !grepl("^ENS[A-Z]*G\\d{11}$", data$response_id)
  if (any(non_ensembl)) {
    errors <- c(errors, "Some response_id values are not valid Ensembl gene IDs")
  }

  # Create summary statistics for valid data
  summary_stats <- if (length(errors) == 0) {
    list(
      total_pairs = nrow(data),
      unique_genes = length(unique(data$response_id)),
      unique_targets = length(unique(data$grna_target))
    )
  } else {
    NULL
  }

  # Return validation result with summary
  return(list(
    is_valid = length(errors) == 0,
    errors = errors,
    data = if(length(errors) == 0) data else NULL,
    summary = summary_stats
  ))
}
