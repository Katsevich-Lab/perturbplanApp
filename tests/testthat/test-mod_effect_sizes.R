test_that("mod_effect_sizes UI works", {
  ui <- mod_effect_sizes_ui("test")
  expect_true(inherits(ui, "shiny.tag"))
  expect_true(length(ui) > 0)
})

test_that("mod_effect_sizes server works", {
  testServer(mod_effect_sizes_server, {
    # Test initial state - modules return reactive values immediately
    expect_true(!is.null(session$returned()))
    expect_true(is.list(session$returned()))
    
    # Test effect size parameters
    session$setInputs(
      fc_mean = 0.85,
      fc_sd = 0.15,
      prop_non_null = 0.1
    )
    
    config <- session$returned()
    expect_equal(config$fc_mean, 0.85)
    expect_equal(config$fc_sd, 0.15)
    expect_equal(config$prop_non_null, 0.1)
    expect_true(!is.null(config$timestamp))
  })
})