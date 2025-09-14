#' plot_display UI Function
#'
#' @description Interactive plot display module using Plotly.
#' Handles rendering of analysis visualization results.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @importFrom plotly plotlyOutput
#' @importFrom shinycssloaders withSpinner
mod_plot_display_ui <- function(id) {
  ns <- NS(id)
  tagList(
    withSpinner(
      plotlyOutput(ns("main_plot"), height = "380px"),
      type = 1,
      color = "#0dc5c1",
      size = 1
    )
  )
}

#' plot_display Server Functions
#'
#' @description Server logic for interactive plot rendering.
#' Handles different plot data structures from analysis results.
#'
#' @param id Module namespace ID
#' @param plot_objects Reactive containing plot objects from plotting engine
#'
#' @noRd
#'
#' @importFrom shiny moduleServer req
#' @importFrom plotly renderPlotly
mod_plot_display_server <- function(id, plot_objects) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ========================================================================
    # MAIN PLOT RENDERING
    # ========================================================================

    output$main_plot <- renderPlotly({

      plots <- plot_objects()
      if (!is.null(plots$error)) {
        return(NULL)
      }
      plots$plots$interactive_plot
    })

  })
}

## To be copied in the UI
# mod_plot_display_ui("plot_display_1")

## To be copied in the server
# mod_plot_display_server("plot_display_1")
