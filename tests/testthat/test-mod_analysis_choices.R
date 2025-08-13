test_that("mod_analysis_choices UI works", {
  ui <- mod_analysis_choices_ui("test")
  expect_true(inherits(ui, "shiny.tag"))
  expect_true(length(ui) > 0)
})

test_that("mod_analysis_choices server works", {
  testServer(mod_analysis_choices_server, {
    # Test initial state - modules return reactive values immediately
    expect_true(!is.null(session$returned()))
    expect_true(is.list(session$returned()))
    
    # Test gene list mode selection
    session$setInputs(gene_list_mode = "random")
    expect_equal(session$returned()$gene_list_mode, "random")
    expect_equal(session$returned()$gene_list_data$type, "random")
    
    # Test statistical parameters
    session$setInputs(
      side = "left",
      control_group = "complement", 
      fdr_target = 0.05
    )
    expect_equal(session$returned()$side, "left")
    expect_equal(session$returned()$control_group, "complement")
    expect_equal(session$returned()$fdr_target, 0.05)
  })
})