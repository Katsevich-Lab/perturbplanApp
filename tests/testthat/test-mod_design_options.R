test_that("mod_design_options UI works", {
  ui <- mod_design_options_ui("test")
  expect_true(inherits(ui, "shiny.tag"))
  expect_true(length(ui) > 0)
})

test_that("mod_design_options server works", {
  testServer(mod_design_options_server, {
    # Test initial state - modules return reactive values immediately
    expect_true(!is.null(session$returned()))
    expect_true(is.list(session$returned()))
    
    # Test optimization type selection
    session$setInputs(optimization_type = "power_only")
    expect_equal(session$returned()$optimization_type, "power_only")
    
    # Test minimization target selection and auto-setting
    session$setInputs(minimization_target = "cells")
    expect_equal(session$returned()$minimization_target, "cells")
    
    # Note: Auto-setting of parameter controls happens via observers
    # In real app, this works due to updateSelectInput, but in tests
    # we need to manually set the input to test the logic
    session$setInputs(cells_control = "minimizing")
    expect_equal(session$returned()$parameter_controls$cells_per_target$type, "minimizing")
  })
})