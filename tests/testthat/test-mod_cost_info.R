test_that("mod_cost_info UI works", {
  ui <- mod_cost_info_ui("test")
  expect_true(inherits(ui, "shiny.tag"))
  expect_true(length(ui) > 0)
})

test_that("mod_cost_info server works", {
  # Create a mock design config reactive
  design_config <- reactive({
    list(
      optimization_type = "power_cost",
      minimization_target = "tpm_threshold"
    )
  })
  
  testServer(mod_cost_info_server, args = list(design_config = design_config), {
    # Test initial state
    expect_true(!is.null(session$returned()))
    expect_true(is.list(session$returned()))
    
    # Test cost parameters
    session$setInputs(
      cost_per_cell = 0.02,
      cost_per_million_reads = 1.5
    )
    
    config <- session$returned()
    expect_equal(config$cost_per_cell, 0.02)
    expect_equal(config$cost_per_million_reads, 1.5)
    expect_true(!is.null(config$timestamp))
  })
})