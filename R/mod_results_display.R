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
    tagList(
      # Row 1: Plot and Sliders side by side
      fluidRow(
        # Plot Column (left 70%)
        column(
          width = 8,
          box(
            title = "Analysis Results",
            status = "primary",
            solidHeader = TRUE,
            width = NULL,
            height = 500,

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

        # Sliders Column (right 30%)
        column(
          width = 4,
          box(
            title = "Parameters",
            status = "success",
            solidHeader = TRUE,
            width = NULL,
            height = 500,

            # Parameter sliders with scrollable content
            conditionalPanel(
              condition = "output.show_sliders == true",
              ns = ns,
              tags$div(
                style = "max-height: 400px; overflow-y: auto;",
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
            title = "Solutions",
            status = "info",
            solidHeader = TRUE,
            width = NULL,
            height = 400,

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
        results <- analysis_results()

        # Only show error if we have actual data with errors, not when data is missing
        has_plot_error <- !is.null(plots) && !is.null(plots$error)
        has_result_error <- !is.null(results) && !is.null(results$error)

        has_plot_error || has_result_error
      }, error = function(e) {
        # If there's an error accessing plots or results, don't show error state
        # This happens on app startup when no analysis has been run yet
        FALSE
      })
    })
    outputOptions(output, "show_error", suspendWhenHidden = FALSE)

    # App state-based slider visibility (Phase 3.5)
    output$show_sliders <- reactive({
      tryCatch({
        # Use app_state$phase to decide whether the slider is showing or not
        if (!is.null(app_state)) {
          return(ifelse(app_state$phase == 2, TRUE, FALSE))
        }

        # Fallback: return FALSE if app_state not available
        return(FALSE)

      }, error = function(e) {
        FALSE
      })
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

    # Initialize parameter sliders module
    # Extract sidebar config and workflow info from analysis results
    sidebar_config <- reactive({
      # Use direct user config for immediate slider functionality (primary source)
      config <- user_config()
      if (!is.null(config)) {
        return(config)
      }

      # Fallback: get from analysis results (if user_config not available for some reason)
      results <- analysis_results()
      if (!is.null(results) && !is.null(results$user_config)) {
        return(results$user_config)
      }

      return(NULL)
    })

    workflow_info <- reactive({
      # Primary: generate workflow info from design config for immediate slider visibility
      config <- user_config()
      if (!is.null(config) && !is.null(config$design_options)) {
        workflow_detection <- detect_slider_workflow(config$design_options)
        if (!is.null(workflow_detection$workflow_id)) {
          return(list(workflow_id = workflow_detection$workflow_id))
        }
      }

      # Fallback: try to get from analysis results (if design config not available)
      results <- analysis_results()
      if (!is.null(results) && !is.null(results$workflow_info)) {
        return(results$workflow_info)
      }

      return(NULL)
    })

    # ========================================================================
    # FOCUSED COMPONENT INITIALIZATION
    # ========================================================================
    # Initialize the three focused display components

    # Plot display component for interactive visualizations
    mod_plot_display_server("plot_display", plot_objects)

    # Solution table component for data summaries
    mod_solution_table_server("solution_table", cached_results, user_config)

    # Error message display
    output$error_message <- renderUI({
      error_msg <- NULL

      tryCatch({
        plots <- plot_objects()
        results <- analysis_results()

        # Check for plotting errors first
        if (!is.null(plots) && !is.null(plots$error)) {
          error_msg <- paste("Plotting Error:", plots$error)
        }
        # Check for analysis errors if no plotting error
        else if (!is.null(results) && !is.null(results$error)) {
          error_msg <- results$error
        }
      }, error = function(e) {
        error_msg <- paste("Display Error:", e$message)
      })

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

    # Excel export using downloadHandler
    output$export_excel <- downloadHandler(
      filename = function() {
        paste0("perturbplan_analysis_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".xlsx")
      },
      content = function(file) {
        req(analysis_results(), plot_objects())

        results <- analysis_results()
        plots <- plot_objects()

        tryCatch({
          # Prepare Excel data using utility functions from fct_excel_export.R
          excel_data <- list(
            "Summary" = create_excel_summary(results, plots),
            "Detailed_Results" = results$power_data,
            "Design_Options" = create_excel_design_options(results$user_config$design_options),
            "Experimental_Setup" = create_excel_experimental_setup(results$user_config$experimental_setup),
            "Analysis_Choices" = create_excel_analysis_choices(results$user_config$analysis_choices),
            "Effect_Sizes" = create_excel_effect_sizes(results$user_config$effect_sizes),
            "Metadata" = data.frame(
              Item = c("Analysis Mode", "Workflow Type", "Timestamp", "App Version"),
              Value = c(
                results$metadata$analysis_mode,
                results$workflow_info$workflow_id,
                as.character(results$metadata$analysis_timestamp),
                results$metadata$app_version
              )
            )
          )

          # Add cost information sheet if cost optimization is used
          if (!is.null(results$user_config$cost_info)) {
            excel_data[["Cost_Information"]] <- create_excel_cost_info(results$user_config$cost_info)
          }

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
        paste0("perturbplan_plot_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")
      },
      content = function(file) {
        req(plot_objects())

        plots <- plot_objects()

        tryCatch({
          # Get the static ggplot (not the interactive plotly version)
          if (!is.null(plots$plots$main_plot)) {
            # Save the ggplot as PNG with high resolution
            ggsave(
              filename = file,
              plot = plots$plots$main_plot,
              width = 12,
              height = 8,
              dpi = 300,
              units = "in",
              device = "png"
            )
          } else {
            stop("No plot available for download")
          }

        }, error = function(e) {
          showNotification(
            paste("Plot download failed:", e$message),
            type = "error",
            duration = 5
          )
          # Create a minimal error plot if main plot fails
          error_plot <- ggplot2::ggplot() +
            ggplot2::annotate("text", x = 0.5, y = 0.5,
                            label = "Plot generation failed",
                            size = 6) +
            ggplot2::theme_void()

          ggplot2::ggsave(filename = file, plot = error_plot,
                         width = 8, height = 6, dpi = 150, device = "png")
        })
      },
      contentType = "image/png"
    )

    # No return needed - focused modules handle all coordination
  })
}
