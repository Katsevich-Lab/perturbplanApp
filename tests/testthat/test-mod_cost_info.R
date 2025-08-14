test_that("mod_cost_info server works", {
  # Create a mock design config reactive
  design_config <- reactive({
    list(
      optimization_type = "power_cost",
      minimization_target = "tpm_threshold"
    )
  })
  
  testServer(
    mod_cost_info_server,
    args = list(design_config = design_config),
    {
      ns <- session$ns
      expect_true(inherits(ns, "function"))
      expect_true(grepl(id, ns("")))
      expect_true(grepl("test", ns("test")))
      
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
    }
  )
})

test_that("mod_cost_info ui works", {
  ui <- mod_cost_info_ui(id = "test")
  golem::expect_shinytaglist(ui)
  # Check that formals have not been removed
  fmls <- formals(mod_cost_info_ui)
  for (i in c("id")){
    expect_true(i %in% names(fmls))
  }
})