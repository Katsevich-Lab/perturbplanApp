#' Results Display Module UI Function
#'
#' @description Creates UI for displaying analysis results including
#' interactive plots, summary tables, and export functionality.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList fluidRow column h3 h4 wellPanel
#' @importFrom shiny conditionalPanel renderUI uiOutput downloadButton
#' @importFrom shinydashboard box
mod_results_display_ui <- function(id) {
  ns <- NS(id)
  tagList(
    # Two-row layout: Row 1: Plot + Sliders, Row 2: Solutions
    tags$div(
      style = "margin-top: -20px;",
      # Row 1: Plot and Sliders side by side
      fluidRow(
        # Plot Column (left 58%)
        column(
          width = 7,
          box(
            title = "Analysis Results",
            status = "primary",
            solidHeader = TRUE,
            width = NULL,
            height = 500,

            # Download button positioned in header area
            conditionalPanel(
              condition = "output.analysis_trigger == true && output.show_error == false",
              ns = ns,
              tags$div(
                style = "position: absolute; top: 8px; right: 15px; z-index: 1000;",
                downloadButton(
                  ns("export_plot"),
                  "",
                  icon = icon("download"),
                  class = "btn btn-primary btn-sm",
                  style = "padding: 4px 8px; margin: 0;",
                  title = "Download Interactive Plot"
                )
              )
            ),

            # Conditional display based on analysis state (mutually exclusive)
            conditionalPanel(
              condition = "output.analysis_trigger == false && output.show_error == false",
              ns = ns,
              wellPanel(
                style = "text-align: center; padding: 50px;",
                h4("Ready for Analysis", style = "color: #5A6B73;"),
                tags$p("Configure your experimental design and click 'Plan'.",
                       style = "color: #7A8B93; font-size: 14px;")
              )
            ),

            conditionalPanel(
              condition = "output.analysis_trigger == true && output.show_error == false",
              ns = ns,
              # Interactive plot output - adjusted for column layout
              mod_plot_display_ui(ns("plot_display"))
            ),

            # Error display panel (only when there's an actual error)
            conditionalPanel(
              condition = "output.show_error == true",
              ns = ns,
              tags$div(
                style = "background-color: #f8d7da; border: 1px solid #f5c6cb; border-radius: 5px; padding: 15px; margin: 15px 0;",
                tags$h4("Error", style = "color: #721c24; margin-top: 0; font-size: 14px;"),
                uiOutput(ns("error_message"))
              )
            )
          )
        ),

        # Sliders Column (right 42%)
        column(
          width = 5,
          box(
            title = "Parameters",
            status = "primary",
            solidHeader = TRUE,
            width = NULL,
            height = 500,

            # Parameter sliders with scrollable content
            conditionalPanel(
              condition = "output.show_sliders == true",
              ns = ns,
              tags$div(
                style = "max-height: 450px; overflow-y: auto;",
                mod_parameter_sliders_ui("sliders")
              )
            ),

            # Placeholder when no sliders
            conditionalPanel(
              condition = "output.show_sliders == false",
              ns = ns,
              wellPanel(
                style = "text-align: center; padding: 50px;",
                tags$p("Parameter sliders will appear here after analysis.",
                       style = "color: #7A8B93; font-size: 14px;")
              )
            )
          )
        )
      ),

      # Row 2: Solutions table spanning full width
      fluidRow(
        column(
          width = 12,
          box(
            title = "Summary Tables",
            status = "info",
            solidHeader = TRUE,
            width = NULL,
            height = 400,

            # Download button positioned in header area
            conditionalPanel(
              condition = "output.analysis_trigger == true && output.show_error == false",
              ns = ns,
              tags$div(
                style = "position: absolute; top: 8px; right: 15px; z-index: 1000;",
                downloadButton(
                  ns("header_export_excel_download"),
                  "",
                  icon = icon("file-excel"),
                  class = "btn btn-success btn-sm",
                  style = "padding: 4px 8px; margin: 0;",
                  title = "Export to Excel"
                )
              )
            ),

            # Solution table with scrollable content
            conditionalPanel(
              condition = "output.analysis_trigger == true",
              ns = ns,
              tags$div(
                style = "max-height: 300px; overflow-y: auto;",
                mod_solution_table_ui(ns("solution_table"))
              )
            ),

            # Placeholder when no results
            conditionalPanel(
              condition = "output.analysis_trigger == false && output.show_error == false",
              ns = ns,
              wellPanel(
                style = "text-align: center; padding: 50px;",
                tags$p("Solutions table will appear here after analysis.",
                       style = "color: #7A8B93; font-size: 14px;")
              )
            )
          )
        )
      )
    )
  )
}

#' Results Display Server Functions
#'
#' @description Server logic for displaying analysis results.
#' Coordinates focused display modules and handles export functionality.
#'
#' @param id Module namespace ID
#' @param plot_objects Reactive containing plot objects from mod_plotting_engine
#' @param cached_results Reactive containing cached results with pinned + current solutions
#'
#' @noRd
#'
#' @importFrom shiny moduleServer reactive observe req renderUI
#' @importFrom shiny showNotification downloadHandler renderPlot observeEvent
#' @importFrom openxlsx write.xlsx
#' @importFrom ggplot2 ggsave ggplot annotate theme_void
#' @importFrom plotly as_widget ggplotly
#' @importFrom htmlwidgets saveWidget
#' @importFrom webshot webshot
mod_results_display_server <- function(id, plot_objects, cached_results, user_config = reactive(NULL), app_state = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ========================================================================
    # REACTIVE DISPLAY STATE
    # ========================================================================
    # Determine if errors should be shown
    output$show_error <- reactive({
      tryCatch({
        plots <- plot_objects()
        !is.null(plots) && !is.null(plots$error)
      }, error = function(e) {
        # If there's an error accessing plots or results, don't show error state
        # This happens on app startup when no analysis has been run yet
        FALSE
      })
    })
    outputOptions(output, "show_error", suspendWhenHidden = FALSE)

    # App state-based slider visibility (Phase 3.5)
    output$show_sliders <- reactive({
      # Use app_state$phase to decide whether the slider is showing or not
      if (!is.null(app_state)) {
        return(ifelse(app_state$phase == 2, TRUE, FALSE))
      }
      # Fallback: return FALSE if app_state not available
      return(FALSE)
    })
    outputOptions(output, "show_sliders", suspendWhenHidden = FALSE)

    # Determine if analysis is actively running/triggered
    output$analysis_trigger <- reactive({
      # Analysis is triggered if we have plan clicks but no results yet
      config <- user_config()
      has_plan_clicks <- !is.null(config) && !is.null(config$plan_clicked) && config$plan_clicked > 0
      has_plan_clicks
    })
    outputOptions(output, "analysis_trigger", suspendWhenHidden = FALSE)

    # ========================================================================
    # FOCUSED COMPONENT INITIALIZATION
    # ========================================================================
    # Initialize the three focused display components

    # Plot display component for interactive visualizations
    mod_plot_display_server("plot_display", plot_objects)

    # Solution table component for data summaries
    mod_solution_table_server("solution_table", cached_results)

    # Error message display
    output$error_message <- renderUI({

      # Check for plotting errors first
      results <- cached_results()
      error_msg <- results$current_result$error

      if (!is.null(error_msg)) {
        tags$div(
          style = "color: #721c24; line-height: 1.5;",
          tags$p(error_msg),
          tags$p("Please check your input parameters and try again.",
                 style = "margin-top: 10px; font-style: italic;")
        )
      }
    })

    # ========================================================================
    # EXPORT FUNCTIONALITY
    # ========================================================================

    # Excel export using downloadHandler (moved from app_server.R - original working version)
    output$header_export_excel_download <- downloadHandler(
      filename = function() {
        paste0("perturbplan_analysis_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".xlsx")
      },
      content = function(file) {
        req(cached_results())

        results <- cached_results()

        tryCatch({
          # Create Excel data using new cached_results approach
          excel_data <- create_excel_export_data(results)
          # Write Excel file to the specified path
          write.xlsx(excel_data, file = file)

        }, error = function(e) {
          showNotification(
            paste("Export failed:", e$message),
            type = "error",
            duration = 5
          )
          stop(e$message)
        })
      },
      contentType = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
    )

    # Plot download using downloadHandler
    output$export_plot <- downloadHandler(
      filename = function() {
        paste0("perturbplan_plot_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".html")
      },
      content = function(file) {
        req(plot_objects())

        plots <- plot_objects()

        # Get the interactive plotly version and save as HTML
        if (!is.null(plots$plots$interactive_plot)) {
          # Create a taller version by re-creating the interactive plot with proper height
          taller_interactive_plot <- suppressWarnings(
            plots$plots$interactive_plot %>%
              plotly::layout(
                height = 750,
                margin = list(t = 60, b = 80, l = 80, r = 40))
          )

          # Save the plotly widget as interactive HTML file
          htmlwidgets::saveWidget(
            widget = plotly::as_widget(taller_interactive_plot),
            file = file,
            selfcontained = TRUE,
            title = "PerturbPlan Interactive Plot"
          )
        } else {
          stop("No plot available for download")
        }
      },
      contentType = "text/html"
    )

    # No return needed - focused modules handle all coordination
  })
}
