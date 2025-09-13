test_that("collapsible_section creates proper HTML structure", {
  ns <- function(x) paste0("test-", x)
  
  # Test collapsed section
  result <- collapsible_section(ns, "section1", "Test Section", tags$div("Content"))
  
  expect_s3_class(result, "shiny.tag.list")
  
  # Check for proper CSS classes
  expect_true(grepl("collapsible-section", as.character(result)))
  expect_true(grepl("collapsible-header", as.character(result)))
  expect_true(grepl("collapsible-content", as.character(result)))
  
  # Check for chevron icon
  expect_true(grepl("fa-chevron-right", as.character(result)))
  expect_true(grepl("display: none", as.character(result)))
  
  # Test expanded section
  result_expanded <- collapsible_section(ns, "section2", "Test Section", tags$div("Content"), collapsed = FALSE)
  expect_true(grepl("fa-chevron-down", as.character(result_expanded)))
  expect_false(grepl("display: none", as.character(result_expanded)))
})

test_that("parameter_input_group creates proper structure", {
  ns <- function(x) paste0("test-", x)
  input_elem <- numericInput("test", NULL, value = 5)
  
  # Test with label and help text
  result <- parameter_input_group(ns, "param1", "Test Label", input_elem, "Help text")
  
  expect_s3_class(result, "shiny.tag")
  expect_true(grepl("input-group", as.character(result)))
  expect_true(grepl("input-label", as.character(result)))
  expect_true(grepl("input-help", as.character(result)))
  expect_true(grepl("Test Label", as.character(result)))
  expect_true(grepl("Help text", as.character(result)))
  
  # Test without label
  result_no_label <- parameter_input_group(ns, "param2", NULL, input_elem)
  expect_false(grepl("input-label", as.character(result_no_label)))
  
  # Test without help text  
  result_no_help <- parameter_input_group(ns, "param3", "Label", input_elem, NULL)
  expect_false(grepl("input-help", as.character(result_no_help)))
})

test_that("currency_input creates proper currency structure", {
  ns <- function(x) paste0("test-", x)
  
  result <- currency_input(ns, "cost", "Cost per cell:", value = 0.05, max = 10)
  
  expect_s3_class(result, "shiny.tag")
  expect_true(grepl("currency-wrapper", as.character(result)))
  expect_true(grepl("currency-symbol", as.character(result)))
  expect_true(grepl("\\$", as.character(result)))
  expect_true(grepl("Cost per cell:", as.character(result)))
  expect_true(grepl("0.05", as.character(result)))
})

test_that("parameter_matrix creates proper matrix structure", {
  ns <- function(x) paste0("test-", x)
  params <- list(
    cells_per_target = "varying",
    reads_per_cell = "fixed",
    TPM_threshold = "minimizing"
  )
  
  result <- parameter_matrix(ns, params)
  
  expect_s3_class(result, "shiny.tag")
  expect_true(grepl("parameter-matrix", as.character(result)))
  expect_true(grepl("parameter-row", as.character(result)))
  expect_true(grepl("parameter-name", as.character(result)))
  expect_true(grepl("parameter-controls", as.character(result)))
  
  # Check for proper parameter names
  expect_true(grepl("Cells per target", as.character(result)))
  expect_true(grepl("Sequenced reads per cell", as.character(result)))
  expect_true(grepl("TPM threshold", as.character(result)))
  
  # Check for radio buttons
  expect_true(grepl('value="varying"', as.character(result)))
  expect_true(grepl('value="fixed"', as.character(result)))
  expect_true(grepl('value="minimizing"', as.character(result)))
  
  # Check for checked states
  expect_true(grepl('checked.*varying', as.character(result)))
  expect_true(grepl('checked.*fixed', as.character(result)))
  expect_true(grepl('checked.*minimizing', as.character(result)))
})

test_that("parameter_matrix handles disabled parameters", {
  ns <- function(x) paste0("test-", x)
  params <- list(cells_per_target = "varying")
  disabled <- c("cells_per_target")
  
  result <- parameter_matrix(ns, params, disabled_params = disabled)
  
  expect_true(grepl('disabled="disabled"', as.character(result)))
})

test_that("parameter_matrix handles empty parameters", {
  ns <- function(x) paste0("test-", x)
  
  result <- parameter_matrix(ns, list())
  
  expect_s3_class(result, "shiny.tag")
  # Should return empty div
  expect_true(grepl("<div></div>", as.character(result)))
})

test_that("step_container creates proper step structure", {
  content <- tags$div("Step content")
  
  result <- step_container(1, "Test Step", content)
  result_str <- as.character(result)
  
  expect_s3_class(result, "shiny.tag")
  expect_true(grepl("step-container", result_str))
  expect_true(grepl("step-header", result_str))
  expect_true(grepl("Step.*1.*Test Step", result_str))
  expect_true(grepl("Step content", result_str))
  
  # Test without divider
  result_no_divider <- step_container(2, "Test Step 2", content, show_divider = FALSE)
  expect_true(grepl("step-container-last", as.character(result_no_divider)))
})

test_that("file_upload_zone creates proper upload structure", {
  ns <- function(x) paste0("test-", x)
  
  result <- file_upload_zone(ns, "upload", 
    accept = c(".rds"),
    info_text = "RDS files only",
    placeholder = "Choose file..."
  )
  
  expect_s3_class(result, "shiny.tag")
  expect_true(grepl("file-upload-info", as.character(result)))
  expect_true(grepl("fa-info-circle", as.character(result)))
  expect_true(grepl("RDS files only", as.character(result)))
  expect_true(grepl("Choose file...", as.character(result)))
  expect_true(grepl('accept=".rds"', as.character(result)))
  
  # Test without info text
  result_no_info <- file_upload_zone(ns, "upload2", accept = c(".csv"))
  expect_false(grepl("file-upload-info", as.character(result_no_info)))
})

test_that("all components use semantic CSS classes", {
  ns <- function(x) paste0("test-", x)
  
  # Test that all components use our CSS foundation classes
  collapsible <- collapsible_section(ns, "test", "Title", tags$div("Content"))
  expect_true(grepl("collapsible-section|collapsible-header|collapsible-content", as.character(collapsible)))
  
  input_group <- parameter_input_group(ns, "test", "Label", numericInput("test", NULL, value = 1))
  expect_true(grepl("input-group|input-label", as.character(input_group)))
  
  currency <- currency_input(ns, "test", "Label", value = 1, max = 10)
  expect_true(grepl("currency-wrapper|currency-symbol|input-group", as.character(currency)))
  
  matrix <- parameter_matrix(ns, list(test_param = "varying"))
  expect_true(grepl("parameter-matrix|parameter-row|parameter-name|parameter-controls", as.character(matrix)))
  
  step <- step_container(1, "Title", tags$div("Content"))
  expect_true(grepl("step-container|step-header", as.character(step)))
  
  upload <- file_upload_zone(ns, "test", info_text = "Info")
  expect_true(grepl("file-upload-info", as.character(upload)))
})