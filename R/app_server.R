#' The application server-side
#'
#' Clean 3-module architecture: Input → Analysis → Plotting → Display
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @importFrom shiny bindCache downloadHandler renderUI req
#' @importFrom magrittr %>%
#' @importFrom openxlsx write.xlsx
#' @importFrom ggplot2 ggsave ggplot annotate theme_void
#' @noRd
app_server <- function(input, output, session) {
  
  # ========================================================================
  # MODULE 0: CENTRAL PARAMETER MANAGEMENT
  # ========================================================================
  # Initialize the central parameter manager (single source of truth)
  param_manager <- mod_parameter_manager_server("param_manager")
  
  # ========================================================================
  # MODULE 1: INPUT COLLECTION 
  # ========================================================================
  # Collect all user inputs through sidebar with parameter manager integration
  # No more slider_updates needed - parameter manager handles all coordination
  user_workflow_config <- mod_sidebar_server("sidebar", param_manager)
  
  # ========================================================================  
  # MODULE 2: ANALYSIS ENGINE (Perturbplan Integration)
  # ========================================================================
  # Generate real analysis results using perturbplan package functions
  analysis_results_raw <- mod_analysis_engine_server("analysis", user_workflow_config)
  
  # ========================================================================
  # MODULE 3: PLOTTING ENGINE (Always Same)
  # ========================================================================
  # Convert analysis data into plot objects
  plot_objects <- mod_plotting_engine_server("plotting", analysis_results_raw)
  
  # ========================================================================
  # MODULE 4: RESULTS DISPLAY (Always Same)  
  # ========================================================================
  # Handle UI presentation of plots and tables with parameter manager integration
  display_outputs <- mod_results_display_server("display", plot_objects, analysis_results_raw, user_workflow_config, param_manager)
  
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
          "header_export_excel",
          "",
          icon = icon("file-excel"),
          class = "btn btn-success btn-sm",
          style = "padding: 4px 8px;",
          title = "Export to Excel"
        ),
        downloadButton(
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
  output$header_export_excel <- downloadHandler(
    filename = function() {
      paste0("perturbplan_analysis_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".xlsx")
    },
    content = function(file) {
      req(analysis_results_raw(), plot_objects())
      
      results <- analysis_results_raw()
      plots <- plot_objects()
      
      tryCatch({
        # Reuse Excel export logic
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
        
        if (!is.null(results$user_config$cost_info)) {
          excel_data[["Cost_Information"]] <- create_excel_cost_info(results$user_config$cost_info)
        }
        
        openxlsx::write.xlsx(excel_data, file = file)
        
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
  
  output$header_export_plot <- downloadHandler(
    filename = function() {
      paste0("perturbplan_plot_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")
    },
    content = function(file) {
      req(plot_objects())
      
      plots <- plot_objects()
      
      tryCatch({
        if (!is.null(plots$plots$main_plot)) {
          ggplot2::ggsave(
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
        # Create error plot fallback
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
  
  # ========================================================================
  # APP STATE MANAGEMENT
  # ========================================================================
  
  # Combined observer for loading states and error handling
  observe({
    config <- user_workflow_config()
    analysis <- analysis_results_raw()
    
    # Handle loading states
    if (!is.null(config) && config$plan_clicked > 0 && is.null(analysis)) {
      # Show loading notification when Plan clicked but analysis not ready
      showNotification(
        "Analyzing your experimental design...", 
        type = "message", 
        duration = 2
      )
    }
    
    # Handle analysis errors  
    if (!is.null(analysis) && !is.null(analysis$error)) {
      showNotification(
        paste("Analysis Error:", analysis$error), 
        type = "error",
        duration = 5
      )
    }
  })
  
  # Development debug output disabled to reduce console output
}
