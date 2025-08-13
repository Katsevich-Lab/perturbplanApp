test_that("mod_experimental_setup UI works", {
  ui <- mod_experimental_setup_ui("test")
  expect_true(inherits(ui, "shiny.tag"))
  expect_true(length(ui) > 0)
})

test_that("mod_experimental_setup server works", {
  testServer(mod_experimental_setup_server, {
    # Test initial state - modules return reactive values immediately
    expect_true(!is.null(session$returned()))
    expect_true(is.list(session$returned()))
    
    # Test biological system selection
    session$setInputs(biological_system = "K562")
    expect_equal(session$returned()$biological_system, "K562")
    
    # Test pilot data choice
    session$setInputs(pilot_data_choice = "default")
    expect_equal(session$returned()$pilot_data_choice, "default")
    expect_equal(session$returned()$pilot_data$type, "default")
  })
})