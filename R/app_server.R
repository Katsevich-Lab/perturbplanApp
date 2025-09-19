#' The application server-side
#'
#' Clean linear architecture: Sidebar → Param Source → Analysis → Plotting → Display
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @importFrom shiny bindCache renderUI req showModal modalDialog modalButton observeEvent
#' @importFrom shinyjs runjs
#' @importFrom magrittr %>%
#' @importFrom openxlsx write.xlsx
#' @importFrom ggplot2 ggsave ggplot annotate theme_void
#' @noRd
app_server <- function(input, output, session) {

  # ========================================================================
  # MODULE 0: GLOBAL APP STATE MANAGEMENT
  # ========================================================================
  # Global application state for dual-workflow system (Phase 1 foundation)
  app_state <- reactiveValues(
    phase = 1,                    # 1 = sidebar mode, 2 = slider mode
    plan_button_text = "Plan"     # Button text: "Plan" or "Restart"
  )


  # ========================================================================
  # MODULE 1: INPUT COLLECTION (Sidebar)
  # ========================================================================
  user_workflow_config <- mod_sidebar_server("sidebar", app_state)

  # ========================================================================
  # MODULE 2: PARAMETER SOURCE COORDINATION
  # ========================================================================
  # Clean linear architecture: sidebar → sliders → param_source → analysis

  # Initialize sliders with visibility-triggered initialization
  slider_results <- mod_parameter_sliders_server("sliders", user_workflow_config, app_state)

  # Central parameter coordination: sidebar-base + slider-override logic
  unified_config <- mod_parameter_source_manager_server("param_source", user_workflow_config, slider_results$slider_config)

  # ========================================================================
  # MODULE 3: ANALYSIS ENGINE
  # ========================================================================
  # Analysis engine now uses unified_config from parameter_source_manager
  analysis_results_raw <- mod_analysis_engine_server("analysis", unified_config)

  # ========================================================================
  # MODULE 3.5: RESULTS CACHE MANAGEMENT
  # ========================================================================
  # Wire pin buttons to cache management - use slider_config to track changes
  cached_results <- mod_results_cache_server("cache", analysis_results_raw,
                                           slider_results$pin_trigger,
                                           slider_results$clear_trigger)

  # ========================================================================
  # MODULE 4: PLOTTING ENGINE
  # ========================================================================
  plot_objects <- mod_plotting_engine_server("plotting", cached_results)

  # ========================================================================
  # MODULE 5: RESULTS DISPLAY
  # ========================================================================
  display_outputs <- mod_results_display_server("display", plot_objects, cached_results, unified_config, app_state)

  # ========================================================================
  # HEADER EXPORT FUNCTIONALITY
  # ========================================================================

  # Header export buttons UI
  output$header_export_buttons <- renderUI({
    # Show export buttons only when results are available
    req(plot_objects(), analysis_results_raw())

    plots <- plot_objects()
    results <- analysis_results_raw()

    if (!is.null(plots) && !is.null(results) &&
        is.null(plots$error) && is.null(results$error)) {

      tags$div(
        style = "display: flex; gap: 8px; align-items: center;",
        actionButton(
          "header_export_excel",
          "",
          icon = icon("file-excel"),
          class = "btn btn-success btn-sm",
          style = "padding: 4px 8px;",
          title = "Export to Excel"
        ),
        actionButton(
          "header_export_plot",
          "",
          icon = icon("image"),
          class = "btn btn-info btn-sm",
          style = "padding: 4px 8px;",
          title = "Download Plot"
        )
      )
    }
  })

  # Header export handlers (reuse logic from results display module)
  # Excel Export - Under Development Dialog
  observeEvent(input$header_export_excel, {
    showModal(modalDialog(
      title = tags$div(
        icon("exclamation-triangle", style = "color: #f39c12; margin-right: 8px;"),
        "Feature Under Development"
      ),
      tags$div(
        style = "text-align: center; padding: 20px;",
        tags$p(
          "Excel export functionality is currently under development.",
          style = "font-size: 16px; margin-bottom: 15px;"
        ),
        tags$p(
          "This feature will be available in a future update.",
          style = "color: #666; font-size: 14px;"
        )
      ),
      footer = modalButton("OK"),
      easyClose = TRUE,
      fade = TRUE
    ))
  })

  # Plot Download - Trigger download from results display module
  observeEvent(input$header_export_plot, {
    # Trigger the download button that's now in the Analysis Results header
    shinyjs::runjs("
      var downloadLink = document.getElementById('display-export_plot');
      if (downloadLink) {
        downloadLink.click();
      } else {
        alert('Download button not found. Please ensure analysis is complete.');
      }
    ")
  })

  # ========================================================================
  # APP STATE MANAGEMENT
  # ========================================================================

  # Error handling observer
  observe({
    analysis <- analysis_results_raw()

    # Handle analysis errors
    if (!is.null(analysis) && !is.null(analysis$error)) {
      showNotification(
        paste("Analysis Error:", analysis$error),
        type = "error",
        duration = 5
      )
    }
  })

  # Phase transition: Move to Phase 2 on successful analysis
  observeEvent(analysis_results_raw(), {
    results <- analysis_results_raw()

    if (!is.null(results) && is.null(results$error) && app_state$phase == 1) {
      app_state$phase <- 2

      showNotification(
        "Analysis complete! Switched to interactive mode.",
        type = "message",
        duration = 3
      )
    }
  }, ignoreNULL = TRUE, ignoreInit = TRUE)

  # ========================================================================
  # RESTART HANDLER - Placeholder for restart functionality
  # ========================================================================
  # This will be implemented in later phases when we add reset functionality
  # For now, the restart trigger in sidebar will show a notification

  # Development debug output disabled to reduce console output
}
