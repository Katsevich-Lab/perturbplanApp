#' Plotting Engine Module UI Function
#'
#' @description Backend-only module that generates plot objects.
#' No UI components needed - this is a pure server-side module.
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
#' @param cached_results Reactive containing cached results with pinned + current solutions
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
mod_plotting_engine_server <- function(id, cached_results) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ========================================================================
    # MAIN PLOTTING REACTIVE
    # ========================================================================

    plot_objects <- reactive({

      results <- cached_results()

      # Don't process if no results available (prevent initialization issues)
      req(!is.null(results$current_result) || !is.null(results$pinned_solutions))

      # Determine plot type and generate appropriate plots
      # Extract workflow_info from the correct location (cached_results structure)
      workflow_info <- if (!is.null(results$current_result)) {
        results$current_result$user_config$workflow_info
      } else {
        results$pinned_solutions[[1]]$user_config$workflow_info
      }

      if (workflow_info$plot_type == "single_parameter_curve") {
        # Generate single parameter power curve plots (8 workflows)
        plots <- create_single_parameter_plots(results)
      } else if (workflow_info$plot_type == "cost_tradeoff_curves") {
        # Generate cost-power tradeoff plots for workflows 5, 10-11 using cached architecture
        plots <- create_cached_cost_tradeoff_plots(results)
      }

      # Return plot objects with metadata (success case)
      final_result <- list(
        plots = plots,
        workflow_info = workflow_info,
        plot_type = workflow_info$plot_type,
        success = TRUE,
        error = NULL
      )
      return(final_result)
    }) %>% bindCache(cached_results())

    return(plot_objects)
  })
}

