testServer(
  mod_sidebar_server,
  # Add here your module params  
  args = list(param_manager = list(parameters = list(
    MOI = 10,
    num_targets = 100,
    gRNAs_per_target = 4,
    cells_per_target = 1000,
    reads_per_cell = 5000,
    TPM_threshold = 10,
    minimum_fold_change = 0.8,
    cost_budget = 50000
  )))
  , {
    ns <- session$ns
    expect_true(
      inherits(ns, "function")
    )
    expect_true(
      grepl(id, ns(""))
    )
    expect_true(
      grepl("test", ns("test"))
    )
    # Here are some examples of tests you can
    # run on your module
    # - Testing the setting of inputs
    # session$setInputs(x = 1)
    # expect_true(input$x == 1)
    # - If ever your input updates a reactiveValues
    # - Note that this reactiveValues must be passed
    # - to the testServer function via args = list()
    # expect_true(r$x == 1)
    # - Testing output
    # expect_true(inherits(output$tbl$html, "html"))
})
 
test_that("module ui works", {
  ui <- mod_sidebar_ui(id = "test")
  # Sidebar returns dashboardSidebar directly (shiny.tag), not tagList
  expect_true(inherits(ui, "shiny.tag"))
  # Check that formals have not been removed
  fmls <- formals(mod_sidebar_ui)
  for (i in c("id")){
    expect_true(i %in% names(fmls))
  }
})
 
