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
#' @importFrom shiny moduleServer reactive req
#' @importFrom ggplot2 ggplot aes geom_line geom_point geom_vline geom_hline geom_area
#' @importFrom ggplot2 labs theme_minimal theme element_text scale_color_manual
#' @importFrom ggplot2 geom_abline scale_color_gradient2 scale_size_manual
#' @importFrom plotly ggplotly layout config
#' @importFrom magrittr %>%
#' @importFrom stats rnorm
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
      
      # Return plot objects with metadata
      return(list(
        plots = plots,
        workflow_info = workflow_info,
        plot_type = workflow_info$plot_type,
        success = TRUE,
        error = NULL
      ))
    })
    
    return(plot_objects)
  })
}


# ============================================================================
# SINGLE PARAMETER POWER CURVE PLOTS (8 workflows)
# ============================================================================

#' Create single parameter power curve plots
#'
#' @description Creates power curve plots for workflows 1-4, 6-7, 9-10
#' where one parameter varies and others are fixed.
#'
#' @param results Analysis results from mod_analysis_engine
#' @return List containing ggplot and plotly objects
#' @noRd
create_single_parameter_plots <- function(results) {
  
  power_data <- results$power_data
  optimal_design <- results$optimal_design
  target_power <- results$user_config$design_options$target_power
  workflow_info <- results$workflow_info
  
  # Create base ggplot
  varying_param <- workflow_info$minimizing_parameter
  param_label <- format_parameter_name(varying_param)
  
  p <- ggplot(power_data, aes(x = parameter_value, y = power)) +
    # Power curve line (connecting the data points)
    geom_line(color = "#2E86AB", linewidth = 1.2) +
    # Data points showing actual power calculations
    geom_point(
      aes(color = meets_threshold),
      size = 2.5, alpha = 0.8
    ) +
    # Target power line (horizontal line at 0.8)
    geom_hline(
      yintercept = target_power, 
      linetype = "dashed", 
      color = "#A23B72", 
      linewidth = 1.2
    ) +
    # Intersection point marker (where power curve crosses target)
    {if (optimal_design$found) {
      geom_point(
        aes(x = optimal_design$value, y = optimal_design$power),
        color = "#F18F01",
        size = 4,
        shape = 21,
        fill = "white",
        stroke = 2
      )
    }} +
    # Styling
    scale_color_manual(
      values = c("TRUE" = "#2E86AB", "FALSE" = "#C73E1D"),
      labels = c("TRUE" = "Meets Target", "FALSE" = "Below Target"),
      name = "Power Status"
    ) +
    labs(
      title = workflow_info$title,
      subtitle = workflow_info$description,
      x = param_label,
      y = "Statistical Power",
      caption = paste("Target Power:", scales::percent(target_power))
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold", color = "#2E4A62"),
      plot.subtitle = element_text(size = 12, color = "#5A6B73"),
      axis.title = element_text(size = 11, face = "bold"),
      legend.position = "bottom"
    )
  
  # Convert to interactive plotly
  p_interactive <- ggplotly(p, tooltip = c("x", "y", "color")) %>%
    layout(
      title = list(
        text = paste0("<b>", workflow_info$title, "</b><br>",
                     "<sup>", workflow_info$description, "</sup>"),
        font = list(size = 14)
      ),
      showlegend = TRUE,
      hovermode = "x unified"
    ) %>%
    config(
      displayModeBar = TRUE,
      modeBarButtonsToRemove = c("pan2d", "select2d", "lasso2d", "autoScale2d"),
      displaylogo = FALSE
    )
  
  # Summary statistics
  summary_stats <- create_power_curve_summary(power_data, optimal_design, target_power)
  
  return(list(
    main_plot = p,
    interactive_plot = p_interactive,
    summary_stats = summary_stats,
    plot_data = power_data,
    optimal_point = optimal_design
  ))
}


# ============================================================================
# COST-POWER TRADEOFF PLOTS (3 workflows)
# ============================================================================

#' Create cost-power tradeoff plots
#'
#' @description Creates cost optimization plots for workflows 5, 8, 11
#' where cells and reads vary simultaneously.
#'
#' @param results Analysis results from mod_analysis_engine
#' @return List containing ggplot and plotly objects
#' @noRd
create_cost_tradeoff_plots <- function(results) {
  
  power_data <- results$power_data
  cost_data <- results$cost_data
  optimal_design <- results$optimal_design
  target_power <- results$user_config$design_options$target_power
  cost_budget <- results$user_config$design_options$cost_budget
  workflow_info <- results$workflow_info
  
  # Create base ggplot for cost-power tradeoff
  p <- ggplot(power_data, aes(x = cells, y = reads)) +
    # Power contour/surface (colored by power achievement)
    geom_point(
      aes(color = power, size = meets_threshold),
      alpha = 0.7
    ) +
    # Cost contour lines (if budget specified)
    {if (!is.null(cost_budget)) {
      # Add budget constraint line
      geom_abline(
        slope = -cost_budget / (50 * 1e-6),  # Simplified cost line
        intercept = cost_budget / 0.10,
        linetype = "dashed",
        color = "#A23B72",
        linewidth = 1
      )
    }} +
    # Optimal design point (if found)
    {if (optimal_design$found) {
      geom_point(
        data = data.frame(cells = optimal_design$cells, reads = optimal_design$reads),
        aes(x = cells, y = reads),
        color = "#F18F01",
        size = 4,
        shape = 17  # Triangle
      )
    }} +
    # Styling
    scale_color_gradient2(
      low = "#C73E1D", 
      mid = "#F7B32B", 
      high = "#2E86AB",
      midpoint = target_power,
      name = "Power",
      labels = scales::percent_format()
    ) +
    scale_size_manual(
      values = c("TRUE" = 3, "FALSE" = 1.5),
      labels = c("TRUE" = "Meets Target", "FALSE" = "Below Target"),
      name = "Power Status"
    ) +
    labs(
      title = workflow_info$title,
      subtitle = workflow_info$description,
      x = "Cells per Target",
      y = "Reads per Cell",
      caption = if (!is.null(cost_budget)) {
        paste("Target Power:", scales::percent(target_power), 
              "| Budget: $", scales::comma(cost_budget))
      } else {
        paste("Target Power:", scales::percent(target_power))
      }
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold", color = "#2E4A62"),
      plot.subtitle = element_text(size = 12, color = "#5A6B73"),
      axis.title = element_text(size = 11, face = "bold"),
      legend.position = "right"
    )
  
  # Convert to interactive plotly
  p_interactive <- ggplotly(p, tooltip = c("x", "y", "color", "size")) %>%
    layout(
      title = list(
        text = paste0("<b>", workflow_info$title, "</b><br>",
                     "<sup>", workflow_info$description, "</sup>"),
        font = list(size = 14)
      ),
      showlegend = TRUE,
      hovermode = "closest"
    ) %>%
    config(
      displayModeBar = TRUE,
      modeBarButtonsToRemove = c("pan2d", "select2d", "lasso2d", "autoScale2d"),
      displaylogo = FALSE
    )
  
  # Cost analysis summary
  cost_summary <- create_cost_analysis_summary(cost_data, optimal_design, target_power, cost_budget)
  
  return(list(
    main_plot = p,
    interactive_plot = p_interactive,
    cost_summary = cost_summary,
    plot_data = power_data,
    optimal_point = optimal_design
  ))
}


# ============================================================================
# UTILITY FUNCTIONS FOR PLOT CREATION
# ============================================================================

#' Create power curve summary statistics
#'
#' @param power_data Power analysis data
#' @param optimal_design Optimal design information
#' @param target_power Target power threshold
#' @return List with summary statistics
#' @noRd
create_power_curve_summary <- function(power_data, optimal_design, target_power) {
  
  # Calculate summary metrics
  feasible_designs <- power_data[power_data$meets_threshold, ]
  
  summary <- list(
    total_designs_evaluated = nrow(power_data),
    feasible_designs = nrow(feasible_designs),
    feasibility_rate = nrow(feasible_designs) / nrow(power_data),
    
    power_range = list(
      min = min(power_data$power, na.rm = TRUE),
      max = max(power_data$power, na.rm = TRUE),
      mean = mean(power_data$power, na.rm = TRUE)
    ),
    
    optimal_recommendation = if (optimal_design$found) {
      list(
        parameter = optimal_design$parameter,
        optimal_value = optimal_design$value,
        achieved_power = optimal_design$power,
        recommendation_text = paste(
          "Optimal design: Set", format_parameter_name(optimal_design$parameter),
          "to", optimal_design$value, "for", scales::percent(optimal_design$power), "power"
        )
      )
    } else {
      list(
        recommendation_text = "No feasible design found within parameter constraints"
      )
    }
  )
  
  return(summary)
}

#' Create cost analysis summary
#'
#' @param cost_data Cost analysis data
#' @param optimal_design Optimal design information  
#' @param target_power Target power threshold
#' @param cost_budget Cost budget constraint
#' @return List with cost summary statistics
#' @noRd
create_cost_analysis_summary <- function(cost_data, optimal_design, target_power, cost_budget) {
  
  # Calculate cost metrics
  feasible_designs <- cost_data[cost_data$power >= target_power, ]
  
  if (!is.null(cost_budget)) {
    budget_feasible <- feasible_designs[feasible_designs$cost <= cost_budget, ]
  } else {
    budget_feasible <- feasible_designs
  }
  
  summary <- list(
    total_designs_evaluated = nrow(cost_data),
    power_feasible_designs = nrow(feasible_designs),
    budget_feasible_designs = if (!is.null(cost_budget)) nrow(budget_feasible) else nrow(feasible_designs),
    
    cost_range = list(
      min = min(cost_data$cost, na.rm = TRUE),
      max = max(cost_data$cost, na.rm = TRUE),
      mean = mean(cost_data$cost, na.rm = TRUE)
    ),
    
    optimal_recommendation = if (optimal_design$found) {
      list(
        optimal_cells = optimal_design$cells,
        optimal_reads = optimal_design$reads,
        total_cost = optimal_design$cost,
        achieved_power = optimal_design$power,
        recommendation_text = paste(
          "Optimal design:", optimal_design$cells, "cells,", optimal_design$reads, "reads",
          "| Cost: $", scales::comma(optimal_design$cost),
          "| Power:", scales::percent(optimal_design$power)
        )
      )
    } else {
      list(
        recommendation_text = if (!is.null(cost_budget)) {
          "No design meets power target within budget constraint"
        } else {
          "No design meets power target within parameter constraints"
        }
      )
    }
  )
  
  return(summary)
}
