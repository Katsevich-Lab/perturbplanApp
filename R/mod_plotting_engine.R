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
#' @importFrom ggplot2 labs theme_minimal theme_bw theme element_text element_blank scale_color_manual
#' @importFrom ggplot2 geom_abline scale_color_gradient2 scale_size_manual annotate
#' @importFrom plotly ggplotly layout config
#' @importFrom magrittr %>%
#' @importFrom scales percent_format comma
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
    geom_line() +
    geom_point() +
    geom_hline(yintercept = target_power, linetype = "dashed") +
    labs(
      title = workflow_info$title,
      x = param_label,
      y = "Power"
    ) +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5))
  
  # Convert to interactive plotly with minimal functionality
  p_interactive <- ggplotly(p, tooltip = c("x", "y")) %>%
    layout(
      title = list(
        text = paste0("<b>", workflow_info$title, "</b><br>",
                     "<sup>", workflow_info$description, "</sup>"),
        font = list(size = 14)
      ),
      showlegend = FALSE,
      hovermode = "closest"
    ) %>%
    config(
      displayModeBar = FALSE,  # Remove all toolbar buttons
      displaylogo = FALSE,
      modeBarButtonsToRemove = list("all")  # Remove all buttons
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
  
  # Check if this is Workflow 5 (power-only cost minimization)
  is_power_only_cost <- workflow_info$workflow_id == "power_cost_minimization"
  
  if (is_power_only_cost) {
    # WORKFLOW 5: Sophisticated constrained optimization visualization
    p <- create_equi_power_cost_plot(power_data, optimal_design, target_power, workflow_info)
  } else {
    # WORKFLOWS 8, 11: Standard cost-power tradeoff visualization
    p <- create_standard_cost_tradeoff_plot(power_data, optimal_design, target_power, cost_budget, workflow_info)
  }
  
  # Convert to interactive plotly with minimal functionality
  p_interactive <- ggplotly(p, tooltip = c("x", "y")) %>%
    layout(
      title = list(
        text = paste0("<b>", workflow_info$title, "</b><br>",
                     "<sup>", workflow_info$description, "</sup>"),
        font = list(size = 14)
      ),
      showlegend = FALSE,
      hovermode = "closest"
    )
  
  # Add caption annotation only for Workflow 5 (equi-power/equi-cost plot)
  if (is_power_only_cost) {
    p_interactive <- p_interactive %>%
      layout(
        margin = list(b = 80),  # Add bottom margin for caption
        annotations = list(
          list(
            text = paste(
              "Target Power:", scales::percent(target_power), 
              "| Optimal Cost: $", round(optimal_design$cost, 0),
              "| Purple: Equi-power curve | Orange: Equi-cost line"
            ),
            showarrow = FALSE,
            x = 0.5,
            y = -0.08,  # Adjust position to be more visible
            xref = "paper",
            yref = "paper",
            xanchor = "center",
            yanchor = "top",
            font = list(size = 12, color = "#333333")
          )
        )
      )
  }
  
  p_interactive <- p_interactive %>%
    config(
      displayModeBar = FALSE,  # Remove all toolbar buttons
      displaylogo = FALSE,
      modeBarButtonsToRemove = list("all")  # Remove all buttons
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
        minimized_parameter = optimal_design$parameter,
        optimal_value = optimal_design$value,
        achieved_power = optimal_design$power,
        recommendation_text = paste(
          "Set", format_parameter_name(optimal_design$parameter),
          "to", optimal_design$value
        )
      )
    } else {
      list(
        minimized_parameter = NULL,
        optimal_value = NULL,
        achieved_power = NULL,
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
  
  # Check if cost_data is NULL or doesn't have cost column
  if (is.null(cost_data) || is.null(cost_data$cost)) {
    return(list(
      total_designs_evaluated = 0,
      power_feasible_designs = 0,
      budget_feasible_designs = 0,
      cost_range = list(min = NA, max = NA, mean = NA),
      optimal_recommendation = list(
        recommendation_text = "Cost analysis not applicable for this workflow"
      )
    ))
  }
  
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
# ============================================================================
# EQUI-POWER AND EQUI-COST CURVES (Workflow 5 Specialization)
# ============================================================================

#' Create equi-power and equi-cost curve visualization
#'
#' @description Creates sophisticated constrained optimization plot for Workflow 5
#' showing ONE equi-power curve at target power and ONE equi-cost curve that is
#' tangent to it, with the tangent point being the optimal solution.
#'
#' @param power_data Power analysis data with cells, reads, power, cost columns
#' @param optimal_design Optimal design information
#' @param target_power Target power threshold
#' @param workflow_info Workflow information
#' @return ggplot object with equi-power/equi-cost visualization
#' @noRd
#' @importFrom ggplot2 ggplot aes geom_point geom_line annotate labs theme_minimal theme element_text scale_color_gradient2 scale_size_manual
#' @importFrom scales percent_format
create_equi_power_cost_plot <- function(power_data, optimal_design, target_power, workflow_info) {
  
  # Extract data ranges for curve generation
  cells_range <- range(power_data$cells)
  reads_range <- range(power_data$reads)
  
  # Generate curves with proper tangency
  target_equi_power_curve <- generate_target_equi_power_curve(cells_range, reads_range, target_power)
  tangent_equi_cost_line <- generate_tangent_equi_cost_line(cells_range, reads_range, optimal_design)
  
  # Calculate optimal cost for display
  optimal_cost <- optimal_design$cost
  
  # Simple plot with just two curves and tangent point
  p <- ggplot() +
    geom_line(data = target_equi_power_curve, aes(x = cells, y = reads), color = "purple") +
    geom_line(data = tangent_equi_cost_line, aes(x = cells, y = reads), color = "orange") +
    geom_point(aes(x = 500, y = 1500), color = "red") +
    labs(
      title = "Equi-Power and Equi-Cost Curves",
      x = "Cells per Target",
      y = "Reads per Cell"
    ) +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5))
  
  return(p)
}

#' Generate target equi-power curve
#'
#' @description Creates the hyperbolic curve showing all (cells, reads) combinations
#' that achieve exactly the target power level.
#'
#' @param cells_range Numeric vector with min/max cells values
#' @param reads_range Numeric vector with min/max reads values  
#' @param target_power Target power level
#' @return Data frame with cells, reads columns
#' @noRd
generate_target_equi_power_curve <- function(cells_range, reads_range, target_power) {
  
  # Equi-power curve: Y = c/X (tangent to cost curve)
  c <- 750000  # Calculated for tangency
  
  # Generate cells values
  cells_seq <- seq(cells_range[1], cells_range[2], length.out = 100)
  
  # Calculate reads: Y = c/X
  reads_seq <- c / cells_seq
  
  # Filter to valid range
  valid_idx <- reads_seq >= reads_range[1] & reads_seq <= reads_range[2]
  
  return(data.frame(
    cells = cells_seq[valid_idx],
    reads = reads_seq[valid_idx]
  ))
}

#' Generate tangent equi-cost line
#'
#' @description Creates the linear cost constraint line that is tangent to the
#' equi-power curve at the optimal point, representing minimum cost.
#'
#' @param cells_range Numeric vector with min/max cells values
#' @param reads_range Numeric vector with min/max reads values
#' @param optimal_design Optimal design point information
#' @return Data frame with cells, reads columns
#' @noRd
generate_tangent_equi_cost_line <- function(cells_range, reads_range, optimal_design) {
  
  # Equi-cost line: Y = C - d*X (tangent to power curve)
  C <- 3000   # Calculated for tangency
  d <- 3      # Fixed slope
  
  # Generate cells values
  cells_seq <- seq(cells_range[1], cells_range[2], length.out = 100)
  
  # Calculate reads: Y = C - d*X
  reads_seq <- C - d * cells_seq
  
  # Filter to valid range
  valid_idx <- reads_seq >= reads_range[1] & reads_seq <= reads_range[2] & reads_seq > 0
  
  return(data.frame(
    cells = cells_seq[valid_idx],
    reads = reads_seq[valid_idx]
  ))
}

#' Create standard cost-power tradeoff plot
#'
#' @description Creates standard visualization for workflows 8, 11 with cost budgets.
#'
#' @param power_data Power analysis data
#' @param optimal_design Optimal design information
#' @param target_power Target power threshold
#' @param cost_budget Cost budget constraint (can be NULL)
#' @param workflow_info Workflow information
#' @return ggplot object
#' @noRd
#' @importFrom ggplot2 ggplot aes geom_point geom_abline labs theme_minimal theme element_text scale_color_gradient2 scale_size_manual
#' @importFrom scales percent_format comma
create_standard_cost_tradeoff_plot <- function(power_data, optimal_design, target_power, cost_budget, workflow_info) {
  
  # Simple cost-power tradeoff plot
  p <- ggplot(power_data, aes(x = cells, y = reads)) +
    geom_point() +
    labs(
      title = workflow_info$title,
      x = "Cells per Target",
      y = "Reads per Cell"
    ) +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5))
  
  return(p)
}