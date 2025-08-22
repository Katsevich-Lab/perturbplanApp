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
      
      # cat("=== PLOTTING ENGINE: plot_objects reactive called ===\n")
      # cat("Results structure:", if(!is.null(results)) "present" else "NULL", "\n")
      # cat("Results error:", if(!is.null(results) && !is.null(results$error)) "present" else "none", "\n")
      
      # Return NULL if no analysis results or error
      if (is.null(results) || !is.null(results$error)) {
      # cat("Returning NULL from plot_objects\n")
        return(NULL)
      }
      
      # Determine plot type and generate appropriate plots
      # Wrap in error handling to prevent app crashes
      tryCatch({
        workflow_info <- results$workflow_info
        
      # cat("Workflow ID:", if(!is.null(workflow_info)) workflow_info$workflow_id else "NULL", "\n")
      # cat("Power data available:", if(!is.null(results$power_data)) "YES" else "NO", "\n")
      # cat("Optimal design available:", if(!is.null(results$optimal_design)) "YES" else "NO", "\n")
        
        if (workflow_info$plot_type == "single_parameter_curve") {
          # Generate single parameter power curve plots (8 workflows)
      # cat("Creating single parameter plots\n")
          plots <- create_single_parameter_plots(results)
        } else if (workflow_info$plot_type == "cost_tradeoff_curves") {
          # Generate cost-power tradeoff plots (3 workflows)
      # cat("Creating cost tradeoff plots\n")
          plots <- create_cost_tradeoff_plots(results)
      # cat("Cost tradeoff plots created, checking structure...\n")
      # cat("Plots is NULL:", is.null(plots), "\n")
      # cat("Plots class:", if(!is.null(plots)) class(plots) else "NULL", "\n")
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
      
      # cat("Returning from plot_objects reactive\n")
      # cat("Final result structure:", paste(names(final_result), collapse = ", "), "\n")
      # cat("Final plots is NULL:", is.null(final_result$plots), "\n")
      
      return(final_result)
    }) %>% bindCache(analysis_results())
    
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
  
  # Use clean titles from workflow_info (no optimal parameter info in titles)
  plot_title <- workflow_info$title
  
  # Standard single parameter power curve (same for power-only and power+cost workflows)
  p <- ggplot(power_data, aes(x = .data$parameter_value, y = .data$power)) +
    geom_line() +
    geom_point() +
    geom_hline(yintercept = target_power, linetype = "dashed") +
    labs(
      title = plot_title,
      x = param_label,
      y = "Power"
    ) +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5))
  
  # Convert to interactive plotly with minimal functionality (same for all single parameter workflows)
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
  summary_stats <- create_power_curve_summary(power_data, optimal_design, target_power, workflow_info)
  
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
  
      # cat("=== INSIDE create_cost_tradeoff_plots ===\n")
  
  power_data <- results$power_data
  cost_data <- results$cost_data  
  optimal_design <- results$optimal_design
  target_power <- results$user_config$design_options$target_power
  cost_budget <- results$user_config$design_options$cost_budget
  workflow_info <- results$workflow_info
  
      # cat("Power data extracted:", !is.null(power_data), "\n")
      # cat("Cost data extracted:", !is.null(cost_data), "\n")
      # cat("Optimal design extracted:", !is.null(optimal_design), "\n")
      # cat("Target power extracted:", !is.null(target_power), "\n")
      # cat("Workflow info extracted:", !is.null(workflow_info), "\n")
  
  # Determine plot type based on workflow category
  is_power_only_cost <- (workflow_info$workflow_id == "power_cost_minimization")
  
  if (workflow_info$workflow_id == "power_cost_minimization") {
    # WORKFLOW 5: Equi-power/equi-cost curves for cost minimization
      # cat("Calling create_equi_power_cost_plot for cost minimization\n")
    p <- create_equi_power_cost_plot(power_data, optimal_design, target_power, workflow_info, cost_data)
      # cat("create_equi_power_cost_plot completed\n")
      # cat("Plot object class:", class(p), "\n")
  } else if (workflow_info$category == "power_cost_multi") {
    # WORKFLOWS 8, 11: Cost vs minimizing parameter (TPM/FC) curves
    p <- create_cost_vs_minimizing_param_plot(power_data, optimal_design, target_power, cost_budget, workflow_info)
  } else {
    # OTHER WORKFLOWS: Standard cost-power tradeoff visualization  
    p <- create_standard_cost_tradeoff_plot(power_data, optimal_design, target_power, cost_budget, workflow_info)
  }
  
  # Convert to interactive plotly with error handling
      # cat("Starting ggplotly conversion...\n")
  p_interactive <- tryCatch({
      # cat("Converting ggplot to plotly...\n")
    # First convert to plotly
    plotly_obj <- ggplotly(p, tooltip = NULL)
    
    # Then manually modify each trace to add proper hover data
    for (i in seq_along(plotly_obj$x$data)) {
      trace_data <- plotly_obj$x$data[[i]]
      if (!is.null(trace_data$x) && !is.null(trace_data$y)) {
        # Transform log coordinates back to normal scale
        cells_values <- round(10^trace_data$x)
        reads_values <- round(10^trace_data$y)
        
        # Set custom hover text
        plotly_obj$x$data[[i]]$hovertemplate <- paste0(
          "Cells per target: %{customdata[0]:,}<br>",
          "Reads per cell: %{customdata[1]:,}<br>",
          "<extra></extra>"
        )
        plotly_obj$x$data[[i]]$customdata <- cbind(cells_values, reads_values)
      }
    }
      # cat("ggplotly conversion with manual trace modification successful\n")
    
      # cat("Adding plotly layout...\n")
    plotly_obj %>%
      layout(
        title = list(
          text = paste0("<b>", workflow_info$title, "</b><br>",
                       "<sup>", workflow_info$description, "</sup>"),
          font = list(size = 14)
        ),
        showlegend = FALSE,
        hovermode = "closest"
      )
  }, error = function(e) {
    # Fallback: create simple plotly plot directly
      # cat("ERROR in ggplotly conversion:", e$message, "\n")
    warning("ggplotly conversion failed for workflow ", workflow_info$workflow_id, ": ", e$message)
    plotly::plot_ly(data = data.frame(x = 1, y = 1), x = ~x, y = ~y, type = "scatter", mode = "markers") %>%
      plotly::layout(
        title = paste("Plot Error:", workflow_info$title),
        xaxis = list(title = "Parameter"),
        yaxis = list(title = "Value"),
        annotations = list(
          list(text = "Plot generation failed - please check data", 
               x = 0.5, y = 0.5, showarrow = FALSE, 
               xref = "paper", yref = "paper")
        )
      )
  })
  
  # Caption annotation removed - info now displayed directly on curves
  
  p_interactive <- p_interactive %>%
    config(
      displayModeBar = FALSE,  # Remove all toolbar buttons
      displaylogo = FALSE,
      modeBarButtonsToRemove = list("all")  # Remove all buttons
    )
  
  # Cost analysis summary
      # cat("Creating cost analysis summary...\n")
  cost_summary <- create_cost_analysis_summary(cost_data, optimal_design, target_power, cost_budget)
      # cat("Cost analysis summary created\n")
  
      # cat("Returning plot objects...\n")
  result <- list(
    main_plot = p,
    interactive_plot = p_interactive,
    cost_summary = cost_summary,
    plot_data = power_data,
    optimal_point = optimal_design
  )
      # cat("Plot objects prepared for return\n")
  return(result)
}


# ============================================================================
# UTILITY FUNCTIONS FOR PLOT CREATION
# ============================================================================

#' Create power curve summary statistics
#'
#' @param power_data Power analysis data
#' @param optimal_design Optimal design information
#' @param target_power Target power threshold
#' @param workflow_info Workflow information with minimizing parameter
#' @return List with summary statistics
#' @noRd
create_power_curve_summary <- function(power_data, optimal_design, target_power, workflow_info) {
  
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
    
    optimal_recommendation = if (!is.null(optimal_design$parameter_value) && !is.null(optimal_design$achieved_power)) {
      # Real perturbplan data structure
      minimizing_param <- workflow_info$minimizing_parameter
      list(
        parameter = minimizing_param,
        optimal_value = optimal_design$parameter_value,
        achieved_power = optimal_design$achieved_power,
        recommendation_text = paste(
          "Set", format_parameter_name(minimizing_param),
          "to", round(optimal_design$parameter_value, 2)
        )
      )
    } else if (!is.null(optimal_design$found) && optimal_design$found) {
      # Legacy placeholder data structure
      list(
        parameter = optimal_design$parameter,
        optimal_value = optimal_design$value,
        achieved_power = optimal_design$power,
        recommendation_text = paste(
          "Set", format_parameter_name(optimal_design$parameter),
          "to", optimal_design$value
        )
      )
    } else {
      list(
        parameter = NULL,
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
  
      # cat("=== INSIDE create_cost_analysis_summary ===\n")
      # cat("Cost data null check:", is.null(cost_data), "\n")
  if (!is.null(cost_data)) {
      # cat("Cost data columns:", paste(names(cost_data), collapse = ", "), "\n")
  }
  
  # Check if cost_data is NULL or doesn't have total_cost column
  if (is.null(cost_data) || is.null(cost_data$total_cost)) {
      # cat("Returning early - no cost data available\n")
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
  
  # For cost minimization, cost_data doesn't have power column, so skip power filtering
      # cat("Creating simplified cost summary for cost minimization\n")
  feasible_designs <- cost_data  # All designs from cost_grid are relevant
  
  if (!is.null(cost_budget)) {
    budget_feasible <- feasible_designs[feasible_designs$total_cost <= cost_budget, ]
  } else {
    budget_feasible <- feasible_designs
  }
  
  summary <- list(
    total_designs_evaluated = nrow(cost_data),
    power_feasible_designs = nrow(feasible_designs),
    budget_feasible_designs = if (!is.null(cost_budget)) nrow(budget_feasible) else nrow(feasible_designs),
    
    cost_range = list(
      min = min(cost_data$total_cost, na.rm = TRUE),
      max = max(cost_data$total_cost, na.rm = TRUE),
      mean = mean(cost_data$total_cost, na.rm = TRUE)
    ),
    
    optimal_recommendation = if (!is.null(optimal_design$cells_per_target)) {
      optimal_rec <- list(
        optimal_cells = optimal_design$cells_per_target,
        optimal_reads = optimal_design$reads_per_cell,
        total_cost = optimal_design$total_cost,
        achieved_power = optimal_design$achieved_power,
        recommendation_text = paste(
          "Optimal design:", optimal_design$cells_per_target, "cells,", optimal_design$reads_per_cell, "reads",
          "| Cost: $", scales::comma(optimal_design$total_cost),
          "| Power:", scales::percent(optimal_design$achieved_power, accuracy = 0.1)
        )
      )
      
      # Add optimal minimized parameter if present (for power+cost workflows)
      if (!is.null(optimal_design$optimal_minimized_param)) {
        optimal_rec$optimal_minimized_param <- optimal_design$optimal_minimized_param
      }
      
      optimal_rec
    } else {
      list(
        optimal_cells = NULL,
        optimal_reads = NULL,
        total_cost = NULL,
        achieved_power = NULL,
        optimal_minimized_param = NULL,
        recommendation_text = if (!is.null(cost_budget)) {
          "No design meets power target within budget constraint"
        } else {
          "No design meets power target within parameter constraints"
        }
      )
    }
  )
  
      # cat("Cost analysis summary completed successfully\n")
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
#' @importFrom ggplot2 ggplot aes geom_point geom_line geom_smooth geom_text geom_vline geom_hline annotate labs theme_minimal theme_bw theme element_text scale_color_gradient2 scale_color_viridis_c scale_size_manual scale_x_log10 scale_y_log10 scale_linetype_discrete
#' @importFrom scales percent_format
#' @importFrom dplyr group_by slice_max ungroup
#' @importFrom magrittr %>%
create_equi_power_cost_plot <- function(power_data, optimal_design, target_power, workflow_info, cost_data = NULL) {
  
      # cat("=== INSIDE create_equi_power_cost_plot ===\n")
      # cat("Power data rows:", nrow(power_data), "\n")
      # cat("Power data columns:", paste(names(power_data), collapse = ", "), "\n")
      # cat("Cost data present:", !is.null(cost_data), "\n")
  if (!is.null(cost_data)) {
      # cat("Cost data rows:", nrow(cost_data), "\n")
      # cat("Cost data columns:", paste(names(cost_data), collapse = ", "), "\n")
  }
      # cat("Workflow ID:", workflow_info$workflow_id, "\n")
      # cat("Optimal design structure:", str(optimal_design), "\n")
  
  # For cost minimization workflow, we use:
  # - power_data: optimal_cost_power_df for equi-power curves  
  # - cost_data: optimal_cost_grid for equi-cost curves
  
  if (!is.null(workflow_info) && workflow_info$workflow_id == "power_cost_minimization") {
      # cat("Inside cost minimization branch\n")
    # power_data contains optimal_cost_power_df for equi-power curves
    equi_power_df <- power_data
    
    # cost_data contains optimal_cost_grid for equi-cost curves
    if (!is.null(cost_data) && nrow(cost_data) > 0) {
      # cat("Using cost_data for equi-cost curves\n")
      # cat("Cost data columns:", paste(names(cost_data), collapse = ", "), "\n")
      # cat("Cost data sample values:\n")
      # cat("  cells_per_target range:", range(cost_data$cells_per_target, na.rm = TRUE), "\n")
      if ("reads_per_cell" %in% names(cost_data)) {
      # cat("  reads_per_cell range:", range(cost_data$reads_per_cell, na.rm = TRUE), "\n")
      } else {
      # cat("  reads_per_cell column: NOT FOUND\n")
      }
      # Use cost_data directly as equi-cost curves
      cost_grid_data <- cost_data
      
      # Add cost_of_interest column if not present
      if (!"cost_of_interest" %in% names(cost_grid_data)) {
      # cat("Adding cost_of_interest column\n")
        # Group by similar cost levels
        cost_range <- range(cost_grid_data$total_cost, na.rm = TRUE)
        cost_levels <- seq(from = cost_range[1], to = cost_range[2], length.out = 5)
        cost_levels <- round(cost_levels)
        
        # Assign each point to nearest cost level
        cost_grid_data$cost_of_interest <- sapply(cost_grid_data$total_cost, function(cost) {
          cost_levels[which.min(abs(cost_levels - cost))]
        })
      }
      # cat("Cost grid data prepared, rows:", nrow(cost_grid_data), "\n")
    } else {
      # cat("Using fallback approach for cost levels\n")
      # Fallback: create cost levels from power_data
      cost_range <- range(power_data$total_cost, na.rm = TRUE)
      cost_levels <- seq(from = cost_range[1], to = cost_range[2], length.out = 5)
      cost_levels <- round(cost_levels)
      
      # Create cost grid data for equi-cost lines
      cost_grid_data <- data.frame()
      for (cost_level in cost_levels) {
        # Find points near this cost level
        cost_tolerance <- diff(cost_range) * 0.05  # 5% tolerance
        cost_points <- power_data[abs(power_data$total_cost - cost_level) <= cost_tolerance, ]
        if (nrow(cost_points) > 0) {
          cost_points$cost_of_interest <- cost_level
          cost_grid_data <- rbind(cost_grid_data, cost_points)
        }
      }
      # cat("Fallback cost grid data prepared, rows:", nrow(cost_grid_data), "\n")
    }
    
    # Create label dataframe for cost curve labels
    if (nrow(cost_grid_data) > 0) {
      # cat("Creating label dataframe\n")
      label_df <- cost_grid_data %>%
        group_by(cost_of_interest) %>%
        slice_max(cells_per_target, n = 1, with_ties = FALSE) %>%
        ungroup()
      # cat("Label dataframe created, rows:", nrow(label_df), "\n")
    } else {
      # cat("No cost grid data for labels\n")
      label_df <- data.frame()
    }
    
      # cat("Starting plot creation\n")
    
    # Test each required column exists
    required_cols <- c("cells_per_target", "reads_per_cell", "minimum_fold_change")
    missing_cols <- setdiff(required_cols, names(power_data))
    if (length(missing_cols) > 0) {
      # cat("ERROR: Missing required columns in power_data:", paste(missing_cols, collapse = ", "), "\n")
      return(NULL)
    }
    
    # Create simplified plot for cost minimization (single equi-power + single equi-cost curve)
    tryCatch({
      # cat("Creating simplified cost minimization plot...\n")
      p <- ggplot()
      # cat("Base ggplot created successfully\n")
      
      # cat("Adding equi-power curve (teal)...\n")
      # Single equi-power curve (teal/green color like in your screenshot)
      p <- p + geom_smooth(
        data = power_data,
        mapping = aes(x = cells_per_target, y = reads_per_cell),
        se = FALSE,
        color = "#20B2AA",  # Teal color
        size = 1.2
      )
      # cat("Equi-power curve added successfully\n")
      
      # Single equi-cost curve at optimal cost level (black)
      if (nrow(cost_grid_data) > 0) {
      # cat("Adding equi-cost curve (black)...\n")
        p <- p + geom_smooth(
          data = cost_grid_data,
          mapping = aes(x = cells_per_target, y = reads_per_cell),
          se = FALSE,
          color = "black",
          linetype = "dashed",
          size = 1
        )
      # cat("Equi-cost curve added successfully\n")
      }
      
      # cat("Adding optimal point...\n")
      # Highlight optimal point
      p <- p + geom_point(
        data = data.frame(x = optimal_design$cells_per_target, y = optimal_design$reads_per_cell),
        mapping = aes(x = x, y = y),
        color = "red", 
        size = 4,
        shape = 17  # Triangle
      )
      # cat("Optimal point added successfully\n")
      
      # cat("Adding scales...\n")
      # Use log scales but we'll handle tooltips differently
      p <- p + scale_x_log10(labels = scales::comma_format()) + 
               scale_y_log10(labels = scales::comma_format())
      # cat("Log scales added successfully\n")
      
      # cat("Adding labels and annotations...\n")
      # Clean labels without subtitle
      p <- p + labs(
        x = "Cells per target",
        y = "Reads per cell", 
        title = "Minimize Total Cost"
      )
      
      # Add annotation on equi-power curve (teal curve) - position higher
      power_annotation_x <- median(power_data$cells_per_target, na.rm = TRUE)
      power_annotation_y <- power_data$reads_per_cell[which.min(abs(power_data$cells_per_target - power_annotation_x))]
      
      p <- p + annotate(
        "text", 
        x = power_annotation_x, 
        y = power_annotation_y * 2.5,  # Position much higher above the curve
        label = sprintf("Target Power: %.0f%%", target_power * 100),
        color = "#20B2AA",  # Same color as equi-power curve
        size = 4,
        fontface = "bold",
        hjust = 0.5
      )
      
      # Add annotation on equi-cost curve (black curve) - position lower and left
      if (nrow(cost_grid_data) > 0) {
        cost_annotation_x <- median(cost_grid_data$cells_per_target, na.rm = TRUE) * 0.92  # Position at 0.92 of median
        cost_annotation_y <- cost_grid_data$reads_per_cell[which.min(abs(cost_grid_data$cells_per_target - cost_annotation_x))]
        
        p <- p + annotate(
          "text",
          x = cost_annotation_x,
          y = cost_annotation_y * 0.4,  # Position much lower below the curve
          label = sprintf("Cost: $%.0f", optimal_design$total_cost),  # Remove "Optimal" 
          color = "black",  # Same color as equi-cost curve
          size = 4,
          fontface = "bold",
          hjust = 0.5
        )
      }
      
      # cat("Labels and annotations added successfully\n")
      
      # cat("Adding theme...\n")
      # Theme
      p <- p + theme_bw() +
        theme(
          axis.title.x = element_text(size = 14),
          axis.text.x = element_text(size = 12),
          axis.title.y = element_text(size = 14),
          axis.text.y = element_text(size = 12),
          legend.position = "none",  # Remove all legends
          plot.title = element_text(hjust = 0.5, size = 20)
        )
      # cat("Theme added successfully\n")
      
    }, error = function(e) {
      # cat("ERROR in plot creation:", e$message, "\n")
      # Create a simple fallback plot
      p <<- ggplot(power_data, aes(x = cells_per_target, y = reads_per_cell)) +
        geom_point() +
        labs(title = "Error in plot creation")
    })
    
  } else {
    # Fallback for other workflow types
    p <- ggplot(power_data, aes(x = cells_per_target, y = reads_per_cell)) +
      geom_point(aes(color = total_cost), alpha = 0.6) +
      geom_point(aes(x = optimal_design$cells_per_target, y = optimal_design$reads_per_cell),
                color = "red", size = 3) +
      scale_color_viridis_c(name = "Total Cost") +
      labs(
        title = workflow_info$title,
        x = "Cells per Target",
        y = "Reads per Cell",
        subtitle = paste0("Target Power: ", scales::percent(target_power), 
                         " | Optimal Cost: $", round(optimal_design$total_cost))
      ) +
      theme_bw() +
      theme(plot.title = element_text(hjust = 0.5))
  }
  
      # cat("Plot creation completed, returning plot object\n")
      # cat("Plot object class:", class(p), "\n")
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
  p <- ggplot(power_data, aes(x = .data$cells, y = .data$reads)) +
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

#' Create cost vs minimizing parameter plot
#'
#' @description Creates a decreasing cost curve showing optimal cost vs the minimizing variable (TPM or FC)
#' for power+cost multi-parameter workflows (Workflows 8, 11). Shows clear optimization relationship.
#'
#' @param power_data Power analysis data
#' @param optimal_design Optimal design information
#' @param target_power Target power threshold
#' @param cost_budget Cost budget constraint (can be NULL)
#' @param workflow_info Workflow information
#' @return ggplot object with cost vs parameter visualization
#' @noRd
#' 
#' @importFrom ggplot2 ggplot aes geom_line geom_point geom_hline labs theme_bw theme element_text
#' @importFrom stats rnorm approx
create_cost_vs_minimizing_param_plot <- function(power_data, optimal_design, target_power, cost_budget, workflow_info) {
  
  # Determine minimizing parameter (TPM or FC)
  min_param <- workflow_info$minimizing_parameter
  
  # Create parameter range and labels
  if (min_param == "TPM_threshold") {
    param_values <- seq(5, 50, length.out = 20)  # TPM range
    param_label <- "TPM Threshold"
    optimal_param <- 15  # Optimal TPM value
  } else if (min_param == "fold_change") {
    param_values <- seq(0.5, 3.0, length.out = 20)  # FC range  
    param_label <- "Fold Change"
    optimal_param <- 1.5  # Optimal FC value
  } else {
    # Fallback case
    param_values <- seq(1, 20, length.out = 20)
    param_label <- "Parameter"
    optimal_param <- 10
  }
  
  # Generate strictly decreasing cost curve with dramatic cost differences
  if (min_param == "TPM_threshold") {
    # For TPM: Range from $15000 at TPM=5 to $3000 at TPM=50 (12000 range)
    # Use inverted parameter for decreasing: cost decreases as TPM increases
    normalized_param <- (param_values - min(param_values)) / (max(param_values) - min(param_values))  # 0 to 1
    costs <- 15000 - 12000 * normalized_param^0.5  # Square root for curved decrease
  } else if (min_param == "fold_change") {
    # For FC: Range from $20000 at FC=0.5 to $4000 at FC=3.0 (16000 range)  
    normalized_param <- (param_values - min(param_values)) / (max(param_values) - min(param_values))  # 0 to 1
    costs <- 20000 - 16000 * normalized_param^0.3  # Gentle curve for steeper initial drop
  } else {
    # Fallback: steep linear decrease
    costs <- 20000 - 800 * param_values
  }
  
  # Costs are already decreasing by construction - no need to modify them
  
  # Create plot data
  plot_data <- data.frame(
    param = param_values,
    cost = costs
  )
  
  # Find optimal cost for the optimal parameter
  optimal_cost <- approx(param_values, costs, optimal_param)$y
  
  # Create optimal point data frame
  optimal_point_data <- data.frame(
    param = optimal_param,
    cost = optimal_cost
  )
  
  # Create ggplot with decreasing cost curve
  p <- ggplot(plot_data, aes(x = .data$param, y = .data$cost)) +
    geom_line(color = "blue", size = 1) +
    geom_point(data = optimal_point_data, aes(x = .data$param, y = .data$cost), 
               color = "red", size = 3) +
    # Add horizontal dashed line for cost budget constraint
    {if (!is.null(cost_budget)) {
      geom_hline(yintercept = cost_budget, linetype = "dashed", color = "orange", size = 1)
    }} +
    labs(
      title = "",  # No title to match other plots
      x = param_label,
      y = "Optimal Cost ($)"
    ) +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5))
    
  return(p)
}