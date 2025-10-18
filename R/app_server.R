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
  # MOBILE DEVICE DETECTION AND WARNING
  # ========================================================================
  # Show modal warning when mobile phone is detected by JavaScript
  observeEvent(input$mobile_device_detected, {
    showModal(modalDialog(
      title = tags$div(
        style = "text-align: center; font-size: 20px; font-weight: 600;",
        tags$span(style = "font-size: 48px; display: block; margin-bottom: 10px;", "\U0001F4F1"),
        "Mobile Viewing Not Supported"
      ),
      tags$div(
        style = "text-align: center; line-height: 1.6;",
        tags$p("This application is not optimized for viewing on mobile phones."),
        tags$p("For the best experience, please access the app from a computer."),
        tags$p(
          style = "margin-top: 20px;",
          "Visit the ",
          tags$a(
            href = "https://katsevich-lab.github.io/perturbplanApp/index.html",
            target = "_blank",
            style = "color: #0066cc; text-decoration: underline;",
            "PerturbPlan documentation site"
          ),
          " for more information."
        )
      ),
      footer = NULL,
      easyClose = FALSE,
      size = "m"
    ))
  }, ignoreInit = TRUE)

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
  cached_results <- mod_results_cache_server("cache", analysis_results_raw, slider_results$pin_trigger, slider_results$clear_trigger)

  # ========================================================================
  # MODULE 4: PLOTTING ENGINE
  # ========================================================================
  plot_objects <- mod_plotting_engine_server("plotting", cached_results)

  # ========================================================================
  # MODULE 5: RESULTS DISPLAY
  # ========================================================================
  display_outputs <- mod_results_display_server("display", plot_objects, cached_results, unified_config, app_state)

  # ========================================================================
  # APP STATE MANAGEMENT
  # ========================================================================
  # Phase transition: Move to Phase 2 on successful analysis
  observeEvent(analysis_results_raw(), {
    results <- analysis_results_raw()

    if (!is.null(results) && is.null(results$error) && app_state$phase == 1) {
      app_state$phase <- 2

      # Analysis complete notification removed
    }
  }, ignoreNULL = TRUE, ignoreInit = TRUE)
}
