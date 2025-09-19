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
        downloadButton(
          "header_export_excel_download",
          "",
          icon = icon("file-excel"),
          class = "btn btn-success btn-sm",
          style = "padding: 4px 8px;",
          title = "Export to Excel"
        )
      )
    }
  })

  # Header export handlers - Excel Export
  output$header_export_excel_download <- downloadHandler(
    filename = function() {
      paste0("perturbplan_analysis_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".xlsx")
    },
    content = function(file) {
      req(analysis_results_raw(), plot_objects())

      results <- analysis_results_raw()
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

        # Write Excel file to the specified path
        write.xlsx(excel_data, file = file)

        showNotification(
          "Excel file exported successfully!",
          type = "message",
          duration = 3
        )

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
