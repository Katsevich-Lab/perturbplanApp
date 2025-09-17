test_that("get_experimental_defaults returns correct structure", {
  defaults <- get_experimental_defaults()

  # Check that all expected keys are present
  expected_keys <- c("cells_fixed_default", "reads_per_cell_fixed_default",
                     "max_file_size_mb", "MOI_default", "num_targets_default",
                     "gRNAs_per_target_default", "non_targeting_gRNAs_default",
                     "error_duration_seconds")

  expect_true(all(expected_keys %in% names(defaults)))

  # Check types
  expect_type(defaults$cells_fixed_default, "double")
  expect_type(defaults$max_file_size_mb, "double")
  expect_type(defaults$MOI_default, "double")
})

test_that("validate_upload_file handles missing file", {
  result <- validate_upload_file(NULL)

  expect_false(result$valid)
  expect_equal(result$error_message, "No file provided")
  expect_equal(result$file_size_mb, 0)
})

test_that("validate_upload_file handles wrong extension", {
  mock_file <- list(name = "test.txt", datapath = tempfile())
  file.create(mock_file$datapath)

  result <- validate_upload_file(mock_file)

  expect_false(result$valid)
  expect_true(grepl("RDS file", result$error_message))

  unlink(mock_file$datapath)
})

test_that("categorize_file_error correctly identifies error types", {
  expect_equal(categorize_file_error("cannot open the connection"), "file_read")
  expect_equal(categorize_file_error("magic number error"), "file_corrupt")
  expect_equal(categorize_file_error("version mismatch"), "file_version")
  expect_equal(categorize_file_error("unknown error"), "generic")
})

test_that("format_error_message creates proper HTML", {
  result <- format_error_message("file_read")
  expect_true(grepl("<em style='color:red;'>", result))
  expect_true(grepl("valid RDS file", result))

  result_with_details <- format_error_message("generic", "specific error")
  expect_true(grepl("specific error", result_with_details))
})

test_that("assemble_experimental_config builds correct structure", {
  inputs <- list(
    biological_system = "K562",
    cells_fixed = 1500,
    reads_per_cell_fixed = 6000,
    MOI = 12,
    num_targets = 150,
    gRNAs_per_target = 5,
    non_targeting_gRNAs = 15
  )

  pilot_data <- list(type = "default", biological_system = "K562")
  defaults <- get_experimental_defaults()

  config <- assemble_experimental_config(inputs, pilot_data, defaults)

  expect_equal(config$biological_system, "K562")
  expect_equal(config$pilot_data, pilot_data)
  expect_equal(config$cells_fixed, 1500)
  expect_equal(config$MOI, 12)
  expect_true("timestamp" %in% names(config))
})

test_that("build_pilot_data_config handles custom and default cases", {
  # Test custom case
  mock_file <- list(datapath = "/path/to/file", name = "test.rds")
  custom_data <- list(some = "data")

  result <- build_pilot_data_config("Custom", mock_file, custom_data)
  expect_equal(result$type, "custom")
  expect_equal(result$file_path, "/path/to/file")
  expect_equal(result$data, custom_data)

  # Test default case
  result <- build_pilot_data_config("K562", NULL, NULL)
  expect_equal(result$type, "default")
  expect_equal(result$biological_system, "K562")
})

test_that("format_success_message handles warnings correctly", {
  # Without warnings
  result <- format_success_message("Success!")
  expect_equal(result, "Success!")

  # With warnings
  warnings <- c("Warning 1", "Warning 2")
  result <- format_success_message("Success!", warnings)
  expect_true(grepl("Success!", result))
  expect_true(grepl("Warning 1", result))
  expect_true(grepl("color:orange", result))
})