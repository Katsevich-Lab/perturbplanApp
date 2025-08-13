test_that("mod_sidebar UI works", {
  ui <- mod_sidebar_ui("test")
  expect_true(inherits(ui, "shiny.tag"))
  expect_true(length(ui) > 0)
})

test_that("mod_sidebar server integrates all modules", {
  testServer(mod_sidebar_server, {
    # Test initial state - modules return reactive values immediately
    expect_true(!is.null(session$returned()))
    expect_true(is.list(session$returned()))
    
    # Test plan button click
    session$setInputs(plan_btn = 1)
    
    config <- session$returned()
    expect_true(!is.null(config))
    expect_true(!is.null(config$design_options))
    expect_true(!is.null(config$experimental_setup))
    expect_true(!is.null(config$analysis_choices))
    expect_true(!is.null(config$effect_sizes))
    expect_equal(config$plan_clicked, 1)
    expect_true(!is.null(config$timestamp))
  })
})