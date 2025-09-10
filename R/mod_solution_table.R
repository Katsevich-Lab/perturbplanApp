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
mod_solution_table_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("solutions_table")),
    uiOutput(ns("analysis_summary"))
  )
}
    
#' solution_table Server Functions
#'
#' @description Server logic for solution table and analysis summary rendering.
#' Handles different analysis result structures and error states.
#'
#' @param id Module namespace ID
#' @param analysis_results Reactive containing analysis results
#' @param plot_objects Reactive containing plot objects from plotting engine
#' @param user_config Reactive containing user configuration
#' @param param_manager Parameter manager for accessing current values (legacy)
#'
#' @noRd 
#'
#' @importFrom shiny moduleServer req renderUI tags h4
mod_solution_table_server <- function(id, analysis_results, plot_objects, user_config, param_manager = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # ========================================================================
    # SOLUTIONS TABLE
    # ========================================================================
    
    output$solutions_table <- renderUI({
      req(analysis_results(), plot_objects())
      
      results <- analysis_results()
      plots <- plot_objects()
      
      if (!is.null(results$error) || !is.null(plots$error)) {
        return(tags$div(
          style = "color: #C73E1D; padding: 10px;",
          tags$p("Error in analysis - table cannot be generated.")
        ))
      }
      
      # Create solutions table with current solution as first row
      # Pass user_config and param_manager to access actual slider values
      create_solutions_table(results, plots, user_config, param_manager)
    })
    
    # ========================================================================
    # ANALYSIS SUMMARY
    # ========================================================================
    
    output$analysis_summary <- renderUI({
      req(analysis_results(), plot_objects())
      
      results <- analysis_results()
      plots <- plot_objects()
      
      if (!is.null(results$error) || !is.null(plots$error)) {
        return(tags$div(
          style = "color: #C73E1D; padding: 10px;",
          h4("Analysis Error"),
          tags$p(results$error %||% plots$error)
        ))
      }
      
      # Generate summary based on plot type
      summary_data <- if (plots$plot_type == "single_parameter_curve") {
        plots$plots$summary_stats
      } else {
        plots$plots$cost_summary
      }
      
      workflow_info <- results$workflow_info
      
      render_solution_section(results, plots)
    })
    
  })
}
    
## To be copied in the UI
# mod_solution_table_ui("solution_table_1")
    
## To be copied in the server
# mod_solution_table_server("solution_table_1")