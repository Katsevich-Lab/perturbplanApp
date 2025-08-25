#' Plotting Engine Module UI Function
#'
#' @description Backend-only module that converts analysis data into plot objects.
#' No UI components needed - this is a pure server-side plotting module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_plotting_engine_ui <- function(id) {
  ns <- NS(id)
  tagList(
    # No UI - this is a backend plotting module
  )
}
    
#' Plotting Engine Server Functions
#'
#' @description Converts analysis results data into interactive plot objects.
#' Handles 2 main plot types for all 11 workflow scenarios:
#' 1. Single parameter power curves (8 workflows)
#' 2. Cost-power tradeoff curves (3 workflows)
#'
#' @param id Module namespace ID
#' @param analysis_results Reactive containing analysis results from mod_analysis_engine
#' 
#' @return Reactive list containing plot objects for display
#' @noRd 
#' 
#' @importFrom shiny moduleServer reactive req bindCache
#' @importFrom ggplot2 ggplot aes geom_line geom_point geom_vline geom_hline geom_area
#' @importFrom ggplot2 labs theme_minimal theme_bw theme element_text element_blank scale_color_manual
#' @importFrom ggplot2 geom_abline scale_color_gradient2 scale_size_manual annotate
#' @importFrom plotly ggplotly layout config plot_ly
#' @importFrom magrittr %>%
#' @importFrom scales percent_format comma
#' @importFrom stats rnorm
#' @importFrom rlang .data
mod_plotting_engine_server <- function(id, analysis_results) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # ========================================================================
    # MAIN PLOTTING REACTIVE
    # ========================================================================
    
    plot_objects <- reactive({
      req(analysis_results())
      
      results <- analysis_results()
      
      
      # Return NULL if no analysis results or error
      if (is.null(results) || !is.null(results$error)) {
        return(NULL)
      }
      
      # Determine plot type and generate appropriate plots
      # Wrap in error handling to prevent app crashes
      tryCatch({
        workflow_info <- results$workflow_info
        
        
        if (workflow_info$plot_type == "single_parameter_curve") {
          # Generate single parameter power curve plots (8 workflows)
          plots <- create_single_parameter_plots(results)
        } else if (workflow_info$plot_type == "cost_tradeoff_curves") {
          # Generate cost-power tradeoff plots (3 workflows)
          plots <- create_cost_tradeoff_plots(results)
        } else {
          # Error case
          return(list(
            error = paste("Unknown plot type:", workflow_info$plot_type),
            plots = list()
          ))
        }
      }, error = function(e) {
        # Return error object instead of crashing
        return(list(
          error = paste("Plotting Error:", e$message),
          plots = list(),
          metadata = list(
            plot_type = workflow_info$plot_type %||% "unknown",
            timestamp = Sys.time(),
            error_details = as.character(e)
          )
        ))
      })
      
      # Return plot objects with metadata
      final_result <- list(
        plots = plots,
        workflow_info = workflow_info,
        plot_type = workflow_info$plot_type,
        success = TRUE,
        error = NULL
      )
      
      
      return(final_result)
    }) %>% bindCache(analysis_results())
    
    return(plot_objects)
  })
}


    # All plotting functions have been moved to fct_plotting_functions.R
    # for better separation of concerns and maintainability