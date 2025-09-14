#' solution_table UI Function
#'
#' @description Solution table and analysis summary display module.
#' Handles rendering of solution tables and analysis summaries.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList uiOutput
#' @importFrom shinycssloaders withSpinner
mod_solution_table_ui <- function(id) {
  ns <- NS(id)
  tagList(
    withSpinner(
      uiOutput(ns("enhanced_solutions_table")),
      type = 1,
      color = "#0dc5c1",
      size = 1
    )
  )
}

#' solution_table Server Functions
#'
#' @description Server logic for solution table and analysis summary rendering.
#' Handles different analysis result structures and error states.
#'
#' @param id Module namespace ID
#' @param cached_results Reactive containing cached results with pinned + current solutions
#' @param user_config Reactive containing user configuration
#'
#' @noRd
#'
#' @importFrom shiny moduleServer req renderUI tags h4
mod_solution_table_server <- function(id, cached_results, user_config) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ========================================================================
    # ENHANCED SOLUTIONS TABLE (DEV BRANCH APPROACH)
    # ========================================================================

    output$enhanced_solutions_table <- renderUI({
      cat("DEBUG: enhanced_solutions_table renderUI called\n")

      req(cached_results())
      cat("DEBUG: cached_results() requirement passed\n")

      results <- cached_results()
      cat("DEBUG: cached_results obtained\n")

      # Check for errors in current result
      if (!is.null(results$current_result$error)) {
        cat("DEBUG: Current result has error, returning error message\n")
        return(tags$div(
          style = "color: #C73E1D; padding: 10px;",
          tags$p("Error in analysis - table cannot be generated.")
        ))
      }

      # Create enhanced solutions table
      cat("DEBUG: About to call create_enhanced_solutions_table\n")
      tryCatch({
        result <- create_enhanced_solutions_table(results, user_config)
        cat("DEBUG: create_enhanced_solutions_table completed successfully\n")
        result
      }, error = function(e) {
        cat("DEBUG: Error in create_enhanced_solutions_table:", e$message, "\n")
        tags$div(
          style = "color: #C73E1D; padding: 10px;",
          tags$p(paste("Table generation error:", e$message))
        )
      })
    })

  })
}

## To be copied in the UI
# mod_solution_table_ui("solution_table_1")

## To be copied in the server
# mod_solution_table_server("solution_table_1")
