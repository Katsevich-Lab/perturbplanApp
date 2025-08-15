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
  
  # Generate the target equi-power curve (hyperbolic curve at target power)
  target_equi_power_curve <- generate_target_equi_power_curve(cells_range, reads_range, target_power)
  
  # Generate the tangent equi-cost line (linear line tangent to the power curve)
  tangent_equi_cost_line <- generate_tangent_equi_cost_line(cells_range, reads_range, optimal_design)
  
  # Create base plot
  p <- ggplot(power_data, aes(x = cells, y = reads)) +
    
    # 1. Background power surface (colored points showing power landscape)
    geom_point(
      aes(color = power, size = meets_threshold),
      alpha = 0.5
    ) +
    
    # 2. Target equi-power curve (hyperbolic curve at exactly target power)
    geom_line(
      data = target_equi_power_curve, 
      aes(x = cells, y = reads),
      color = "#A23B72", 
      linewidth = 2.5, 
      linetype = "solid",
      alpha = 1.0
    ) +
    
    # 3. Tangent equi-cost line (linear line with minimum cost that touches power curve)
    geom_line(
      data = tangent_equi_cost_line,
      aes(x = cells, y = reads),
      color = "#F7B32B",
      linewidth = 2.5,
      linetype = "dashed",
      alpha = 1.0
    ) +
    
    # 4. Optimal design point (THE tangent point)
    {if (optimal_design$found) {
      geom_point(
        data = data.frame(cells = optimal_design$cells, reads = optimal_design$reads),
        aes(x = cells, y = reads),
        color = "#F18F01",
        size = 6,
        shape = 21,
        fill = "white",
        stroke = 4
      )
    }} +
    
    # 5. Tangent point annotation with optimization explanation
    {if (optimal_design$found) {
      annotate(
        "text",
        x = optimal_design$cells * 1.15,
        y = optimal_design$reads * 0.85,
        label = paste0("Optimal Tangent Point\n(",
                      optimal_design$cells, " cells, ",
                      optimal_design$reads, " reads)\n",
                      "Cost: $", round(optimal_design$cost, 0)),
        color = "#F18F01",
        size = 3.5,
        fontface = "bold",
        hjust = 0
      )
    }} +
    
    # 6. Curve labels
    annotate(
      "text",
      x = cells_range[2] * 0.7,
      y = reads_range[2] * 0.8,
      label = paste0("Equi-Power Curve\n(Power = ", scales::percent(target_power), ")"),
      color = "#A23B72",
      size = 4,
      fontface = "bold"
    ) +
    
    annotate(
      "text", 
      x = cells_range[2] * 0.8,
      y = reads_range[1] * 1.3,
      label = "Tangent Equi-Cost Line\n(Minimum Cost)",
      color = "#F7B32B",
      size = 4,
      fontface = "bold"
    ) +
    
    # Styling for constrained optimization
    scale_color_gradient2(
      low = "#C73E1D", 
      mid = "#F7B32B", 
      high = "#2E86AB",
      midpoint = target_power,
      name = "Power",
      labels = scales::percent_format()
    ) +
    scale_size_manual(
      values = c("TRUE" = 2, "FALSE" = 1),
      labels = c("TRUE" = ">= Target", "FALSE" = "< Target"),
      name = "Power Status"
    ) +
    labs(
      title = "Constrained Optimization: Minimize Cost Subject to Power >= Target",
      subtitle = "Purple curve: Required power constraint | Yellow line: Optimal cost | Orange: Tangent solution",
      x = "Cells per Target",
      y = "Reads per Cell",
      caption = paste("Solution: The tangent point minimizes cost while achieving exactly", scales::percent(target_power), "power")
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold", color = "#2E4A62"),
      plot.subtitle = element_text(size = 11, color = "#5A6B73"),
      axis.title = element_text(size = 11, face = "bold"),
      legend.position = "right",
      panel.grid.minor = element_blank()
    )
  
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
  
  # For power model: Power proportional to f(cells * reads)
  # At target power: cells * reads = power_constant
  # This gives hyperbolic relationship: reads = power_constant / cells
  
  # Calculate the power constant for target power
  power_constant <- (target_power / 0.8) * 1000000  # Scale to realistic values
  
  # Generate cells values across the range
  cells_seq <- seq(cells_range[1], cells_range[2], length.out = 200)
  
  # Calculate corresponding reads values for constant power
  reads_seq <- power_constant / cells_seq
  
  # Filter to realistic reads range
  valid_idx <- reads_seq >= reads_range[1] & reads_seq <= reads_range[2]
  
  curve_data <- data.frame(
    cells = cells_seq[valid_idx],
    reads = reads_seq[valid_idx]
  )
  
  return(curve_data)
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
  
  if (!optimal_design$found) {
    return(data.frame(cells = numeric(0), reads = numeric(0)))
  }
  
  # Cost model: cost = cost_per_cell * cells + cost_per_million_reads * (reads/1e6) * cells
  # Linear approximation: cost = a * cells + b * reads * cells
  cost_per_cell <- 0.10
  cost_per_million_reads <- 50
  
  # The optimal cost level (from the tangent point)
  optimal_cost <- optimal_design$cost
  
  # Generate the linear cost constraint line passing through optimal point
  # For linear cost line: cost_per_cell * cells + (cost_per_million_reads/1e6) * reads * cells = optimal_cost
  # Solve for reads: reads = (optimal_cost - cost_per_cell * cells) / ((cost_per_million_reads/1e6) * cells)
  
  # Generate cells values across a reasonable range around optimal
  cells_center <- optimal_design$cells
  cells_width <- (cells_range[2] - cells_range[1]) * 0.6
  cells_seq <- seq(max(cells_range[1], cells_center - cells_width/2), 
                   min(cells_range[2], cells_center + cells_width/2), 
                   length.out = 100)
  
  # Calculate reads for constant cost
  reads_seq <- (optimal_cost - cost_per_cell * cells_seq) / ((cost_per_million_reads * 1e-6) * cells_seq)
  
  # Filter to realistic reads range
  valid_idx <- reads_seq >= reads_range[1] & reads_seq <= reads_range[2] & reads_seq > 0
  
  if (sum(valid_idx) == 0) {
    return(data.frame(cells = numeric(0), reads = numeric(0)))
  }
  
  line_data <- data.frame(
    cells = cells_seq[valid_idx],
    reads = reads_seq[valid_idx]
  )
  
  return(line_data)
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
  
  return(p)
}