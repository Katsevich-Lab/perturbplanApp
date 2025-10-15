# ============================================================================
# PLOTTING FUNCTIONS FOR PERTURBPLAN APP
# ============================================================================
#
# This file contains all plotting functions used by mod_plotting_engine.R
# Organized by plot type and workflow category.
#
#' @importFrom ggplot2 ggplot aes geom_line geom_point geom_hline geom_vline geom_area
#' @importFrom ggplot2 labs theme_minimal theme_bw theme element_text element_blank scale_color_manual
#' @importFrom ggplot2 geom_abline scale_color_gradient2 scale_size_manual annotate geom_smooth geom_text
#' @importFrom ggplot2 scale_x_log10 scale_y_log10 scale_x_continuous scale_linetype_discrete scale_color_viridis_c coord_cartesian
#' @importFrom plotly ggplotly layout config plot_ly
#' @importFrom magrittr %>%
#' @importFrom scales percent_format comma comma_format dollar_format
#' @importFrom stats rnorm median power complete.cases setNames
#' @importFrom rlang .data
#' @importFrom dplyr group_by slice_max slice_min ungroup arrange case_when
#' @importFrom utils globalVariables
NULL

# Declare global variables to avoid R CMD check notes
if(getRversion() >= "2.15.1") utils::globalVariables(c(
  "hover_text", "parameter_value", "tooltip_text", "solution_label",
  "solution_tooltip", "point_tooltip", "achieved_power"
))

# ============================================================================
# MULTI-SOLUTION PLOTTING SYSTEM (Phase 3)
# ============================================================================

#' Color palette for multi-solution plotting
#' @description Colorblind-friendly palette for up to 10 solutions
#' @noRd
SOLUTION_COLORS <- c(
  "#2E86AB",  # Blue (Index 1)
  "#A23B72",  # Purple-red (Index 2)
  "#F18F01",  # Orange (Index 3)
  "#4CAF50",  # Green (Index 4)
  "#E91E63",  # Pink (Index 5)
  "#607D8B",  # Blue-grey (Index 6)
  "pink",   # Red (Index 7)
  "blue",   # Red (Index 8)
  "purple",   # Red (Index 9)
  "brown"   # Red (Index 10)
)

#' Get color for solution by index
#'
#' @param solution_index Numeric index of solution (1-based)
#' @return Color hex string
#' @noRd
get_solution_color <- function(solution_index) {
  color_index <- ((solution_index - 1) %% length(SOLUTION_COLORS)) + 1
  return(SOLUTION_COLORS[color_index])
}

# ============================================================================
# SINGLE PARAMETER POWER CURVE PLOTS (8 workflows)
# ============================================================================

#' Create single parameter power curve plots
#'
#' @description Creates power curve plots for workflows 1-4, 6-7, 9-10
#' where one parameter varies and others are fixed. Enhanced to support multi-solution plotting.
#'
#' @param cached_results Cached results from mod_results_cache (current + pinned solutions)
#' @return List containing ggplot and plotly objects
#' @noRd
create_single_parameter_plots <- function(cached_results) {
  # Handle both legacy single results and new cached results format
  if (!is.null(cached_results$current_result) || !is.null(cached_results$all_results)) {
    # New cached results format - transform to plotting format
    solutions_list <- list()

    # Add current result if available
    if (!is.null(cached_results$current_result)) {
      current <- cached_results$current_result
      solutions_list[["Current"]] <- list(
        id = 0,
        color = get_solution_color(1),  # Use first color for Current
        data = current$power_data,
        optimal_point = current$optimal_design,
        label = "Current"
      )
    }

    # Add pinned solutions if available
    if (!is.null(cached_results$pinned_solutions) && length(cached_results$pinned_solutions) > 0) {
      for (i in seq_along(cached_results$pinned_solutions)) {
        solution_name <- names(cached_results$pinned_solutions)[i]
        pinned <- cached_results$pinned_solutions[[i]]
        solutions_list[[solution_name]] <- list(
          id = i,
          color = get_solution_color(i + 1),  # Skip first color (used by Current)
          data = pinned$power_data,
          optimal_point = pinned$optimal_design,
          label = solution_name
        )
      }
    }

    # Use current result for metadata if available, otherwise use first result
    metadata_source <- cached_results$current_result %||% cached_results$pinned_solutions[[1]]
    if (is.null(metadata_source)) {
      stop("No valid results available for plotting")
    }

    target_power <- metadata_source$user_config$design_options$target_power
    workflow_info <- metadata_source$user_config$workflow_info

  }

  # Extract assay type for assay-aware labels
  assay_type <- metadata_source$user_config$design_options$assay_type

  varying_param <- workflow_info$minimizing_parameter
  param_label <- format_parameter_name(varying_param, assay_type)
  plot_title <- create_workflow_title(varying_param, workflow_info, assay_type)

  # Convert to expected format
  solutions_data <- solutions_list

  # Build base ggplot
  p <- ggplot() +
    geom_hline(yintercept = target_power, linetype = "dashed", alpha = 0.7, color = "grey") +
    labs(
      title = plot_title,
      x = param_label,
      y = "Power"
    )

  # Add log scale for TPM_threshold (keep dynamic label)
  if (varying_param == "TPM_threshold") {
    p <- p + scale_x_log10(labels = scales::comma_format())
  }

  # Combine all solutions into a single dataframe for consistent color mapping
  combined_data <- data.frame()
  optimal_points <- data.frame()

  for (solution in solutions_data) {
    solution_data <- solution$data
    solution_label <- solution$label  # "Current", "Setting 1", etc.

    # Create tooltip text
    formatted_values <- case_when(
      varying_param == "TPM_threshold" && !is.null(assay_type) && assay_type == "tap_seq" ~ as.character(round(solution_data$parameter_value, 2)),  # TAP-seq: 2 decimals
      varying_param == "TPM_threshold" ~ scales::comma(round(solution_data$parameter_value)),  # Perturb-seq: integer
      varying_param %in% c("cells_per_target", "reads_per_cell") ~ scales::comma(solution_data$parameter_value),
      varying_param == "minimum_fold_change" ~ as.character(round(solution_data$parameter_value, 2)),
      TRUE ~ as.character(solution_data$parameter_value)
    )

    # Get tooltip label: short version for TPM_threshold, otherwise use param_label
    tooltip_label <- if (varying_param == "TPM_threshold") {
      format_expression_threshold_tooltip_label(assay_type)  # "UMIs/cell" or "TPM"
    } else {
      param_label
    }

    # Add solution info to data
    solution_data$solution_label <- solution_label
    solution_data$tooltip_text <- paste0(
      solution_label, "<br>",
      tooltip_label, ": ", formatted_values, "<br>",
      "Power: ", scales::percent(solution_data$power, accuracy = 0.1)
    )

    # Combine with main dataset
    combined_data <- rbind(combined_data, solution_data)

    # Add optimal point if available
    if (!is.null(solution$optimal_point)) {
      optimal_design <- solution$optimal_point

      # Get display value: use Expression_threshold if minimizing TPM, otherwise use varying_param
      display_value <- if (varying_param == "TPM_threshold") {
        optimal_design$Expression_threshold  # Use transformed value for display
      } else {
        optimal_design[[varying_param]]
      }

      # Get tooltip label: short version for TPM_threshold, otherwise use param_label
      optimal_tooltip_label <- if (varying_param == "TPM_threshold") {
        format_expression_threshold_tooltip_label(assay_type)  # "UMIs/cell" or "TPM"
      } else {
        param_label
      }

      optimal_hover_text <- paste0(
        solution_label, " (Optimal)<br>",
        optimal_tooltip_label, ": ",
        case_when(
          varying_param == "TPM_threshold" && !is.null(assay_type) && assay_type == "tap_seq" ~ as.character(round(display_value, 2)),  # TAP-seq: 2 decimals
          varying_param == "TPM_threshold" ~ scales::comma(round(display_value)),  # Perturb-seq: integer
          varying_param %in% c("cells_per_target", "reads_per_cell") ~ scales::comma(display_value),
          varying_param == "minimum_fold_change" ~ as.character(round(display_value, 2)),
          TRUE ~ as.character(display_value)
        ),
        "<br>Power: ", scales::percent(optimal_design$achieved_power, accuracy = 0.1)
      )

      # Add optimal point to dataframe
      optimal_point_data <- data.frame(
        parameter_value = display_value,  # Use transformed value for plotting
        power = optimal_design$achieved_power,
        solution_label = solution_label,
        tooltip_text = optimal_hover_text
      )
      optimal_points <- rbind(optimal_points, optimal_point_data)
    }
  }

  # Add all power curves at once with proper color mapping
  if (nrow(combined_data) > 0) {
    # Add power curve lines
    p <- p + geom_line(
      data = combined_data,
      aes(x = parameter_value, y = power,
          color = solution_label),
      size = 0.6
    )

    # Add points for interactivity
    p <- p + suppressWarnings(geom_point(
      data = combined_data,
      aes(x = parameter_value, y = power,
          color = solution_label,
          text = tooltip_text),
      size = 1
    ))

    # Add optimal points if any
    if (nrow(optimal_points) > 0) {
      p <- p + suppressWarnings(geom_point(
        data = optimal_points,
        aes(x = parameter_value, y = power,
            color = solution_label,
            text = tooltip_text),
        size = 2,
        shape = 18  # Diamond shape for optimal points
      ))
    }
  }

  # Configure colors and styling
  p <- p +
    scale_color_manual(
      values = setNames(
        sapply(solutions_data, function(s) s$color),
        sapply(solutions_data, function(s) s$label)
      )
    ) +
    coord_cartesian(ylim = c(NA, 1)) +  # Ensure y-axis shows power = 1 as maximum
    theme_bw() +
    labs(color = "Parameter Setting") +
    theme(plot.title = element_text(hjust = 0.5),
          legend.position = "bottom")

  # Convert to interactive plotly
  p_interactive <- suppressWarnings(ggplotly(p, tooltip = "text", height = 430)) %>%
    layout(
      title = list(
        text = paste0("<b>", plot_title, "</b>"),
        font = list(size = 14)
      ),
      hovermode = "closest",
      legend = list(
        orientation = "h",     # horizontal legend
        x = 0.5, xanchor = "center",
        y = -0.25, yanchor = "top"  # put it below the plot with less space
      ),
      annotations = list(
        list(
          x = 1, y = 1,
          xref = "paper", yref = "paper",
          text = "\u25c6 Optimal solution",  # Unicode for black diamond (◆)
          xanchor = "right", yanchor = "top",
          showarrow = FALSE,
          font = list(size = 11, color = "black")
        )
      )
    ) %>%
    config(
      displayModeBar = FALSE,
      displaylogo = FALSE,
      modeBarButtonsToRemove = list("all")
    )

  return(list(
    interactive_plot = p_interactive,
    ggplot_object = p
  ))
}


# ============================================================================
# COST-POWER TRADEOFF PLOTS (3 workflows)
# ============================================================================

#' Create cached cost-power tradeoff plots (Router Function)
#'
#' @description Router function with shared solution extraction logic for workflows 5, 10-11.
#' Extracts current and pinned solutions from cached_results, then routes to specialized
#' plotting functions based on workflow type.
#'
#' @param cached_results Cached results from mod_results_cache (current + pinned solutions)
#' @return List containing ggplot and plotly objects
#' @noRd
create_cached_cost_tradeoff_plots <- function(cached_results) {

  # ========================================================================
  # SHARED SOLUTION EXTRACTION LOGIC
  # ========================================================================

  # Add pinned solutions if available
  if (!is.null(cached_results$current_result) || !is.null(cached_results$all_results)) {
    # Extract solutions in the same way as create_single_parameter_plots
    solutions_list <- list()

    # Add current result if available
    if (!is.null(cached_results$current_result)) {
      current <- cached_results$current_result
      solutions_list[["Current"]] <- list(
        id = 0,
        color = get_solution_color(1),  # Use first color for Current
        power_data = current$power_data,    # Power data for equi-power curves
        cost_data = current$cost_data,      # Cost data for equi-cost curves
        optimal_point = current$optimal_design,
        label = "Current"
      )
    }

    if (!is.null(cached_results$pinned_solutions) && length(cached_results$pinned_solutions) > 0) {
      for (i in seq_along(cached_results$pinned_solutions)) {
        solution_name <- names(cached_results$pinned_solutions)[i]
        pinned <- cached_results$pinned_solutions[[i]]
        solutions_list[[solution_name]] <- list(
          id = i,
          color = get_solution_color(i + 1),  # Skip first color (used by Current)
          power_data = pinned$power_data,     # Power data for equi-power curves
          cost_data = pinned$cost_data,       # Cost data for equi-cost curves
          optimal_point = pinned$optimal_design,
          label = solution_name
        )
      }
    }
  }

  # Extract metadata from available source (current result takes priority)
  metadata_source <- cached_results$current_result %||% cached_results$pinned_solutions[[1]]
  if (is.null(metadata_source)) {
    stop("No valid results available for cost tradeoff plotting")
  }

  workflow_info <- metadata_source$user_config$workflow_info
  workflow_id <- workflow_info$workflow_id

  # ========================================================================
  # WORKFLOW ROUTING LOGIC
  # ========================================================================

  if (workflow_id == "power_cost_minimization") {
    # Workflow 5: Cost minimization (cost vs power scatter plot)
    return(create_cost_minimization_plots(solutions_list, workflow_info, metadata_source))
  }
  else if (workflow_id %in% c("power_cost_TPM_cells_reads", "power_cost_fc_cells_reads")) {
    # Workflows 10-11: Constrained minimization (parameter vs cost line plots)
    return(create_constrained_minimization_plots(solutions_list, workflow_info, metadata_source))
  }
  else {
    stop(paste("Unsupported workflow for cost tradeoff plotting:", workflow_id))
  }
}

#' Create cost minimization plots for workflow 5
#'
#' @description Creates cost vs power scatter plots for workflow 5 (cost minimization).
#' Shows equi-power and equi-cost curves with optimal points for each solution.
#'
#' @param solutions_list List of solution data from router function
#' @param workflow_info Workflow information object
#' @param metadata_source Source for extracting target_power and other metadata
#' @return List containing ggplot and plotly objects
#' @noRd
create_cost_minimization_plots <- function(solutions_list, workflow_info, metadata_source) {

  # Extract target power and assay type from metadata
  target_power <- metadata_source$user_config$design_options$target_power
  assay_type <- metadata_source$user_config$design_options$assay_type

  # Create plot title once for reuse
  plot_title <- create_workflow_title(workflow_info$minimizing_parameter, workflow_info, assay_type)

  # Combine all solutions into unified dataframes for consistent plotting
  combined_power_data <- data.frame()
  combined_cost_data <- data.frame()
  optimal_points <- data.frame()

  for (solution in solutions_list) {
    solution_label <- solution$label
    solution_color <- solution$color

    # Process power data (equi-power curves)
    if (!is.null(solution$power_data)) {
      power_data <- solution$power_data

      # Add solution info
      power_data$solution_label <- solution_label
      power_data$solution_color <- solution_color
      combined_power_data <- rbind(combined_power_data, power_data)
    }

    # Process cost data (equi-cost curves)
    if (!is.null(solution$cost_data)) {
      cost_data <- solution$cost_data

      # Add solution info and cost level grouping
      cost_data$solution_label <- solution_label
      cost_data$solution_color <- solution_color

      # Group cost data by cost levels if not already present
      if (!"cost_of_interest" %in% names(cost_data) && "total_cost" %in% names(cost_data)) {
        cost_range <- range(cost_data$total_cost, na.rm = TRUE)
        cost_levels <- seq(from = cost_range[1], to = cost_range[2], length.out = 3)
        cost_levels <- round(cost_levels)

        # Assign each point to nearest cost level
        cost_data$cost_of_interest <- sapply(cost_data$total_cost, function(cost) {
          cost_levels[which.min(abs(cost_levels - cost))]
        })
      }

      combined_cost_data <- rbind(combined_cost_data, cost_data)
    }

    # Add optimal point
    optimal_design <- solution$optimal_point
    optimal_point <- data.frame(
      cells_per_target = optimal_design$cells_per_target,
      sequenced_reads_per_cell = optimal_design[["sequenced_reads_per_cell"]],
      total_cost = optimal_design$total_cost,
      achieved_power = optimal_design$achieved_power,
      solution_label = solution_label,
      solution_color = solution_color
    )
    optimal_points <- rbind(optimal_points, optimal_point)
  }

  # Create base plot
  p <- ggplot()

  # Add equi-power curves for each solution
  if (nrow(combined_power_data) > 0) {
    # Add smooth curves
    p <- p + suppressWarnings(geom_smooth(
      data = combined_power_data,
      mapping = aes(x = cells_per_target, y = sequenced_reads_per_cell,
                   color = solution_label),
      se = FALSE,
      size = 0.6
    ))

    # Add points for better interactivity with tooltips
    p <- p + suppressWarnings(geom_point(
      data = combined_power_data,
      mapping = aes(x = cells_per_target, y = sequenced_reads_per_cell,
                    color = solution_label,
                    text = paste0(solution_label, "<br>",
                                  "Cells: ", scales::comma(cells_per_target), "<br>",
                                  "Reads: ", scales::comma(sequenced_reads_per_cell), "<br>",
                                  "Cost: $", scales::comma(total_cost, accuracy = 1, na_default = "N/A"), "<br>",
                                  "Power: ", scales::percent(target_power, accuracy = 0.1))),
      size = 1
    ))
  }

  # Add equi-cost curves if available
  if (nrow(combined_cost_data) > 0) {
    # Add points for better interactivity with tooltips
    p <- p + suppressWarnings(geom_point(
      data = combined_cost_data,
      mapping = aes(x = cells_per_target, y = sequenced_reads_per_cell,
                   color = solution_label,
                   text = paste0(solution_label, "<br>",
                               "Cells: ", scales::comma(cells_per_target), "<br>",
                               "Reads: ", scales::comma(sequenced_reads_per_cell), "<br>",
                               "Actual Cost: $", scales::comma(total_cost, accuracy = 1, na_default = "N/A"))),
      size = 1
    ))

    # Add dashed lines for cost curves
    p <- p + suppressWarnings(geom_smooth(
      data = combined_cost_data,
      mapping = aes(x = cells_per_target, y = sequenced_reads_per_cell,
                   color = solution_label),
      linetype = "dashed",
      size = 0.6,
      se = FALSE
    ))
  }

  # Add optimal points
  if (nrow(optimal_points) > 0) {
    p <- p + suppressWarnings(geom_point(
      data = optimal_points,
      mapping = aes(x = cells_per_target, y = sequenced_reads_per_cell,
                   color = solution_label,
                   text = paste0(solution_label, " (Optimal)<br>",
                               "Cells: ", scales::comma(cells_per_target), "<br>",
                               "Reads: ", scales::comma(sequenced_reads_per_cell), "<br>",
                               "Cost: $", scales::comma(total_cost, accuracy = 1, na_default = "N/A"), "<br>",
                               "Power: ", scales::percent(achieved_power, accuracy = 0.1, na_default = "N/A"))),
      size = 3,
      shape = 18
    ))
  }

  # Apply log scales for both x and y axes
  p <- p + scale_x_log10(labels = scales::comma_format()) +
           scale_y_log10(labels = scales::comma_format())

  # Set color scale
  p <- p + scale_color_manual(values = setNames(sapply(solutions_list, function(sol) sol$color),
                                                sapply(solutions_list, function(sol) sol$label)),
                              name = "Parameter Setting")

  # Labels and theme
  p <- p + labs(
    title = plot_title,
    x = "Cells per target",
    y = "Reads per cell",
    color = "Parameter Setting"
  ) +
  theme_bw() +
  theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        legend.position = "bottom")

  # Create interactive plotly version
  interactive_plot <- suppressWarnings(ggplotly(p, tooltip = "text", height = 430)) %>%
    layout(
      title = list(
        text = paste0("<b>", plot_title, "</b>"),
        font = list(size = 14)
      ),
      xaxis = list(title = "Cells per target"),
      yaxis = list(title = "Reads per cell"),
      showlegend = TRUE,
      legend = list(orientation = "h", x = 0.5, xanchor = "center", y = -0.25),
      annotations = list(
        list(
          x = 1, y = 1,
          xref = "paper", yref = "paper",
          text = "\u25c6 Optimal solution   \u2500\u2500 Equi-power   \u22ef\u22ef Equi-cost",  # Unicode: ◆ ── ⋯⋯
          xanchor = "right", yanchor = "top",
          showarrow = FALSE,
          font = list(size = 11, color = "black")
        )
      )
    )  %>%
    config(
      displayModeBar = FALSE,
      displaylogo = FALSE,
      modeBarButtonsToRemove = list("all")
    )

  return(list(
    interactive_plot = interactive_plot,
    ggplot_object = p
  ))
}

#' Create constrained minimization plots for workflows 10-11
#'
#' @description Creates parameter vs cost line plots for workflows 10-11 (constrained minimization).
#' Shows how minimization parameter (TPM threshold or fold change) affects total cost
#' across multiple solutions with optimal design points highlighted.
#'
#' @param solutions_list List of solution data from router function
#' @param workflow_info Workflow information object
#' @param metadata_source Source for extracting constraints and metadata
#' @return List containing ggplot and plotly objects
#' @noRd
create_constrained_minimization_plots <- function(solutions_list, workflow_info, metadata_source) {

  # ========================================================================
  # STEP 1: FUNCTION SIGNATURE & PARAMETER DETECTION
  # ========================================================================

  # Extract assay type for assay-aware labels
  assay_type <- metadata_source$user_config$design_options$assay_type

  # Determine minimization parameter from workflow
  workflow_id <- workflow_info$workflow_id

  # Create plot title once for reuse
  plot_title <- create_workflow_title(workflow_info$minimizing_parameter, workflow_info, assay_type)

  if (workflow_id == "power_cost_TPM_cells_reads") {
    param_column <- "TPM_threshold"
    x_axis_label <- format_parameter_name("TPM_threshold", assay_type)  # "Expression threshold (UMIs/cell)" or "Expression threshold (TPM)"
    # Use short label for tooltips
    param_name <- format_expression_threshold_tooltip_label(assay_type)  # "UMIs/cell" or "TPM"
  } else if (workflow_id == "power_cost_fc_cells_reads") {
    param_column <- "minimum_fold_change"
    x_axis_label <- "Fold Change"
    param_name <- "Fold Change"
  } else {
    stop("Unsupported workflow for constrained minimization: ", workflow_id)
  }

  # Extract constraints from metadata
  target_power <- metadata_source$user_config$design_options$target_power %||% 0.8
  cost_budget <- metadata_source$user_config$design_options$cost_budget

  # ========================================================================
  # STEP 2: EXTRACT DATA FROM POWER_DATA ONLY
  # ========================================================================

  # Build combined data from power_data (NOT cost_data - workflows 10-11 don't have cost_data)
  combined_data <- data.frame()
  optimal_points <- data.frame()

  for (solution in solutions_list) {
    # Get power_data (contains parameter sweep + costs)
    power_data <- solution$power_data

    # Validate required columns exist
    if (!param_column %in% names(power_data)) {
      stop("Missing parameter column '", param_column, "' in power_data for solution: ", solution$label)
    }
    if (!"total_cost" %in% names(power_data)) {
      stop("Missing total_cost column in power_data for solution: ", solution$label)
    }

    # Flexible power column detection (workflows 10-11 use "overall_power", others use "power")
    power_col <- "overall_power"

    # Get display value: use Expression_threshold if minimizing TPM, otherwise use param_column
    display_values <- if (param_column == "TPM_threshold" && "Expression_threshold" %in% names(power_data)) {
      power_data$Expression_threshold  # Use transformed values for display
    } else {
      power_data[[param_column]]
    }

    # Extract relevant columns: parameter, cost, power
    solution_data <- data.frame(
      parameter_value = display_values,                 # Use transformed values for TPM_threshold
      total_cost = power_data$total_cost,               # Cost from power_data
      power = power_data[[power_col]],                  # Statistical power (flexible column name)
      solution_label = solution$label,
      solution_color = solution$color,
      stringsAsFactors = FALSE
    )

    # Remove any rows with missing values
    solution_data <- solution_data[complete.cases(solution_data), ]

    combined_data <- rbind(combined_data, solution_data)
  }

  # ========================================================================
  # STEP 3: CREATE LINE TOOLTIPS FOR ALL DATA POINTS
  # ========================================================================

  # Create solution_tooltip for line hover functionality (consistent with other plots)
  # Format parameter value based on type and assay: TAP-seq TPM = 2 decimals, Perturb-seq TPM = integer, FC = 2 decimals
  combined_data$solution_tooltip <- paste0(
    combined_data$solution_label, "<br>",
    param_name, ": ",
    if (param_column == "TPM_threshold" && !is.null(assay_type) && assay_type == "tap_seq") {
      round(combined_data$parameter_value, 2)  # TAP-seq: 2 decimals
    } else if (param_column == "TPM_threshold") {
      round(combined_data$parameter_value, 0)  # Perturb-seq: integer
    } else {
      round(combined_data$parameter_value, 2)  # Fold Change: 2 decimals
    }, "<br>",
    "Cost: $", scales::comma(combined_data$total_cost, accuracy = 1), "<br>",
    "Power: ", scales::percent(combined_data$power, accuracy = 0.1)
  )

  # ========================================================================
  # STEP 4: EXTRACT OPTIMAL POINTS FROM POWER_DATA
  # ========================================================================

  for (solution in solutions_list) {

    # Extract optimal design point
    optimal_design <- solution$optimal_point

    # Validate optimal design has required fields
    if (!param_column %in% names(optimal_design)) {
      stop("Missing parameter '", param_column, "' in optimal_design for solution: ", solution$label)
    }

    # Get display value: use Expression_threshold if minimizing TPM, otherwise use param_column
    display_value <- if (param_column == "TPM_threshold") {
      optimal_design$Expression_threshold  # Use transformed value for display
    } else {
      optimal_design[[param_column]]
    }

    optimal_point <- data.frame(
      parameter_value = display_value,  # Use transformed value for plotting
      total_cost = optimal_design$total_cost,            # Cost at optimal point
      power = optimal_design$power %||% optimal_design$achieved_power, # Power at optimal point
      solution_label = solution$label,
      solution_color = solution$color,
      stringsAsFactors = FALSE
    )

    optimal_points <- rbind(optimal_points, optimal_point)
  }

  # Validate we have data to plot
  if (nrow(combined_data) == 0) {
    stop("No valid data found for constrained minimization plotting")
  }
  if (nrow(optimal_points) == 0) {
    stop("No valid optimal points found for constrained minimization plotting")
  }

  # ========================================================================
  # STEP 5: CREATE OPTIMAL POINT TOOLTIPS
  # ========================================================================

  # Optimal point tooltips
  # Format parameter value based on type and assay: TAP-seq TPM = 2 decimals, Perturb-seq TPM = integer, FC = 2 decimals
  optimal_points$point_tooltip <- paste0(
    "Optimal Design: ", optimal_points$solution_label, "<br>",
    param_name, ": ",
    if (param_column == "TPM_threshold" && !is.null(assay_type) && assay_type == "tap_seq") {
      round(optimal_points$parameter_value, 2)  # TAP-seq: 2 decimals
    } else if (param_column == "TPM_threshold") {
      round(optimal_points$parameter_value, 0)  # Perturb-seq: integer
    } else {
      round(optimal_points$parameter_value, 2)  # Fold Change: 2 decimals
    }, "<br>",
    "Cost: $", scales::comma(optimal_points$total_cost, accuracy = 1), "<br>",
    "Power: ", scales::percent(optimal_points$power, accuracy = 0.1)
  )

  # ========================================================================
  # STEP 6: CREATE LINE PLOT (PARAMETER VS COST)
  # ========================================================================

  # Create ggplot: parameter vs cost with multiple solution lines
  p <- ggplot() +
    geom_line(data = combined_data,
              aes(x = parameter_value, y = total_cost,
                  color = solution_label),
              size = 0.6) +
    suppressWarnings(geom_point(data = combined_data,
              aes(x = parameter_value, y = total_cost,
                  color = solution_label, text = solution_tooltip),
              size = 1)) +
    suppressWarnings(geom_point(data = optimal_points,
               aes(x = parameter_value, y = total_cost,
                   color = solution_label, text = point_tooltip),
               size = 2, shape = 18)) +  # Diamond shape for optimal points
    # Add horizontal line for cost budget if available
    {if (!is.null(cost_budget) && !is.na(cost_budget))
      geom_hline(yintercept = cost_budget, linetype = "dashed", alpha = 0.7, color = "grey")
    } +
    {if (param_column == "TPM_threshold") {
      scale_x_log10(labels = scales::comma_format())
    } else if (param_column == "minimum_fold_change") {
      # Get all unique fold change values and set specific breaks
      all_values <- unique(combined_data$parameter_value)
      if (length(all_values) > 0) {
        breaks <- sort(all_values)
        scale_x_continuous(breaks = breaks, labels = round(breaks, 2))
      } else {
        scale_x_continuous()
      }
    } else {
      scale_x_continuous()
    }} +
    scale_y_log10(labels = scales::comma_format(accuracy = 1)) +
    labs(title = paste(param_name, "vs Cost"),
         x = x_axis_label,
         y = "Total Cost ($)",
         color = "Parameter Setting") +
    theme_bw() +
    theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
          legend.position = "bottom")

  # Set color scale for multiple solutions
  p <- p + scale_color_manual(values = setNames(sapply(solutions_list, function(sol) sol$color),
                                                sapply(solutions_list, function(sol) sol$label)),
                              name = "Parameter Setting")

  # ========================================================================
  # STEP 7: CONVERT TO PLOTLY AND CONFIGURE STYLING
  # ========================================================================

  # Convert to interactive plotly
  interactive_plot <- suppressWarnings(ggplotly(p, tooltip = "text", height = 430)) %>%
    layout(
      title = list(
        text = paste0("<b>", plot_title, "</b>"),
        font = list(size = 14)
      ),
      xaxis = list(title = x_axis_label),
      yaxis = list(title = "Total Cost ($)"),
      showlegend = TRUE,
      legend = list(orientation = "h", x = 0.5, xanchor = "center", y = -0.25),
      hovermode = "closest",
      annotations = list(
        list(
          x = 1, y = 1,
          xref = "paper", yref = "paper",
          text = "\u25c6 Optimal solution",  # Unicode for black diamond (◆)
          xanchor = "right", yanchor = "top",
          showarrow = FALSE,
          font = list(size = 11, color = "black")
        )
      )
    ) %>%
    config(
      displayModeBar = FALSE,
      displaylogo = FALSE,
      modeBarButtonsToRemove = list("all")
    )

  return(list(
    interactive_plot = interactive_plot,
    ggplot_object = p
  ))
}

# ============================================================================
# UTILITY FUNCTIONS
# ============================================================================

# format_parameter_name function moved to fct_workflow_detection.R to avoid duplication

#' Create workflow title for plotting
#'
#' @param minimizing_param The parameter being minimized
#' @param workflow_info Workflow information containing workflow_id to distinguish power vs power+cost
#' @param assay_type Character: "tap_seq" or "perturb_seq" (optional, used for TPM_threshold labels)
#' @return Character string with plot title
#' @noRd
create_workflow_title <- function(minimizing_param, workflow_info, assay_type = NULL) {
  # Check if this is a power+cost workflow
  is_power_cost <- !is.null(workflow_info$workflow_id) &&
                   grepl("power_cost", workflow_info$workflow_id) &&
                   workflow_info$workflow_id != "power_cost_minimization"  # Exclude cost minimization

  # Choose prefix based on workflow type
  prefix <- if (is_power_cost) "Power + Cost Optimization" else "Power Optimization"

  switch(minimizing_param,
    "cells_per_target" = paste0(prefix, ": Minimize Cells per Target"),
    "reads_per_cell" = paste0(prefix, ": Minimize Reads per Cell"),
    "TPM_threshold" = paste0(prefix, ": Minimize Expression threshold"),  # Plain title, no assay info
    "minimum_fold_change" = paste0(prefix, ": Minimize Fold Change"),
    paste0(prefix, ": Minimize ", format_parameter_name(minimizing_param, assay_type))
  )
}
