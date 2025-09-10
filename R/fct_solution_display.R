#' Solution Display Helper Functions
#'
#' @description Legacy solution display functions for backward compatibility
#' with existing solution rendering patterns.
#'

#' Render solution section with optimal parameters
#'
#' @param results Analysis results object
#' @param plots Plot objects
#' @return Shiny UI tagList
#' @noRd
#' @importFrom shiny tagList tags
render_solution_section <- function(results, plots) {
  if (is.null(results$optimal_design) || is.null(results$workflow_info)) {
    return(render_no_solution_fallback())
  }
  
  optimal <- results$optimal_design
  workflow_info <- results$workflow_info
  minimizing_param <- workflow_info$minimizing_parameter
  
  tagList(
    # SOLUTION SECTION
    tags$div(
      style = "background-color: #E8F4FD; padding: 18px; border-radius: 5px; margin-bottom: 15px; border-left: 4px solid #2E86AB;",
      
      # Solution header
      tags$div(
        tags$strong("Solution", style = "color: #2E4A62; font-size: 14px; margin-bottom: 12px; display: block;")
      ),
      
      # Show minimizing parameter based on workflow type
      render_minimizing_parameter_display(optimal, workflow_info, minimizing_param),
      
      # Show varying parameter for power+cost workflows
      render_varying_parameter_display(optimal, workflow_info),
      
      # Power achieved (for non-unified workflows)
      render_power_achieved_display(optimal, workflow_info)
    ),
    
    # FIXED PARAMETERS SECTION
    render_fixed_parameters_section(optimal, plots, minimizing_param, workflow_info)
  )
}

#' Render minimizing parameter display based on workflow type
#'
#' @param optimal Optimal design results
#' @param workflow_info Workflow information
#' @param minimizing_param Minimizing parameter name
#' @return Shiny UI tagList
#' @noRd
render_minimizing_parameter_display <- function(optimal, workflow_info, minimizing_param) {
  # Check if this is workflows 10-11 (unified constrained minimization)
  if (workflow_info$workflow_id %in% c("power_cost_TPM_cells_reads", "power_cost_fc_cells_reads")) {
    # Need to pass the full results object, not just optimal_design and workflow_info
    # We need to reconstruct the full results object from the parent context
    return(render_minimization_solution_display(optimal, workflow_info))
  }
  
  # Handle different minimizing parameters
  if (minimizing_param == "TPM_threshold") {
    return(render_tpm_minimization_display(optimal))
  } else if (minimizing_param == "minimum_fold_change") {
    return(render_fc_minimization_display(optimal))
  } else if (minimizing_param == "cells_per_target") {
    return(render_cells_minimization_display(optimal))
  } else if (minimizing_param %in% c("reads_per_cell", "mapped_reads_per_cell")) {
    return(render_reads_minimization_display(optimal))
  } else if (minimizing_param == "cost") {
    return(render_cost_minimization_display(optimal))
  }
  
  return(NULL)
}

#' Render minimization solution display (for unified workflows 10-11)
#'
#' @param optimal Optimal design results
#' @param workflow_info Workflow information
#' @return Shiny UI tagList
#' @noRd
render_minimization_solution_display <- function(optimal, workflow_info) {
  minimizing_param <- workflow_info$minimizing_parameter
  
  # For unified workflows, show the minimizing parameter and supporting parameters
  if (minimizing_param == "TPM_threshold") {
    tagList(
      create_parameter_display("Optimal TPM threshold: ", optimal$TPM_threshold, 0, "18px"),
      create_cost_display(optimal$total_cost),
      # If both cells and reads are varying (no specific varying_parameter), show them here
      if (is.null(workflow_info$varying_parameter)) {
        tagList(
          create_parameter_display("Optimal cells per target: ", optimal$cells_per_target, 0, "16px"),
          create_parameter_display("Optimal sequenced reads per cell: ", optimal$sequenced_reads_per_cell, 0, "16px")
        )
      },
      create_power_achieved_display(optimal$achieved_power)
    )
  } else if (minimizing_param == "minimum_fold_change") {
    tagList(
      create_parameter_display("Optimal fold change: ", optimal$minimum_fold_change, 2, "18px"),
      create_cost_display(optimal$total_cost),
      # If both cells and reads are varying (no specific varying_parameter), show them here
      if (is.null(workflow_info$varying_parameter)) {
        tagList(
          create_parameter_display("Optimal cells per target: ", optimal$cells_per_target, 0, "16px"),
          create_parameter_display("Optimal sequenced reads per cell: ", optimal$sequenced_reads_per_cell, 0, "16px")
        )
      },
      create_power_achieved_display(optimal$achieved_power)
    )
  } else {
    # Fallback for any other minimizing parameters
    tagList(
      create_cost_display(optimal$total_cost),
      create_power_achieved_display(optimal$achieved_power)
    )
  }
}

#' Render TPM minimization display
#'
#' @param optimal Optimal design results
#' @return Shiny UI tagList
#' @noRd
render_tpm_minimization_display <- function(optimal) {
  if (is.null(optimal$TPM_threshold) || is.na(optimal$TPM_threshold)) {
    return(NULL)
  }
  
  tagList(
    create_parameter_display("Optimal TPM threshold: ", optimal$TPM_threshold, 0, "18px"),
    create_cost_display(optimal$total_cost)
  )
}

#' Render fold change minimization display
#'
#' @param optimal Optimal design results
#' @return Shiny UI tagList
#' @noRd
render_fc_minimization_display <- function(optimal) {
  if (is.null(optimal$minimum_fold_change) || is.na(optimal$minimum_fold_change)) {
    return(NULL)
  }
  
  tagList(
    create_parameter_display("Optimal fold change: ", optimal$minimum_fold_change, 2, "18px"),
    create_cost_display(optimal$total_cost)
  )
}

#' Render cells minimization display
#'
#' @param optimal Optimal design results
#' @return Shiny UI tagList
#' @noRd
render_cells_minimization_display <- function(optimal) {
  if (is.null(optimal$cells_per_target) || is.na(optimal$cells_per_target)) {
    return(NULL)
  }
  
  create_parameter_display("Optimal cells per target: ", optimal$cells_per_target, 0, "18px")
}

#' Render reads minimization display
#'
#' @param optimal Optimal design results
#' @return Shiny UI tagList
#' @noRd
render_reads_minimization_display <- function(optimal) {
  if (is.null(optimal$sequenced_reads_per_cell) || is.na(optimal$sequenced_reads_per_cell)) {
    return(NULL)
  }
  
  create_parameter_display("Optimal sequenced reads per cell: ", optimal$sequenced_reads_per_cell, 0, "18px")
}

#' Render cost minimization display
#'
#' @param optimal Optimal design results
#' @return Shiny UI tagList
#' @noRd
render_cost_minimization_display <- function(optimal) {
  if (is.null(optimal$total_cost) || is.na(optimal$total_cost)) {
    return(NULL)
  }
  
  tagList(
    tags$div(
      style = "margin-bottom: 8px;",
      tags$span("Minimum total cost: ", style = "color: #5A6B73; font-weight: 500;"),
      tags$span(paste0("$", round(optimal$total_cost)), style = "color: #2E86AB; font-weight: bold; font-size: 18px;")
    ),
    create_parameter_display("Optimal cells per target: ", optimal$cells_per_target, 0, "16px"),
    create_parameter_display("Optimal sequenced reads per cell: ", optimal$sequenced_reads_per_cell, 0, "16px")
  )
}

#' Render varying parameter display for power+cost workflows
#'
#' @param optimal Optimal design results
#' @param workflow_info Workflow information
#' @return Shiny UI tagList or NULL
#' @noRd
render_varying_parameter_display <- function(optimal, workflow_info) {
  if (is.null(workflow_info$category) || workflow_info$category != "power_cost_single" || 
      is.null(workflow_info$varying_parameter)) {
    return(NULL)
  }
  
  varying_param <- workflow_info$varying_parameter
  
  if (varying_param == "cells" && !is.null(optimal$cells_per_target) && !is.na(optimal$cells_per_target)) {
    return(create_parameter_display("Cost-constrained cells per target: ", optimal$cells_per_target, 0, "18px"))
  } else if (varying_param == "reads" && !is.null(optimal$sequenced_reads_per_cell) && !is.na(optimal$sequenced_reads_per_cell)) {
    return(create_parameter_display("Cost-constrained sequenced reads per cell: ", optimal$sequenced_reads_per_cell, 0, "18px"))
  }
  
  return(NULL)
}

#' Render power achieved display
#'
#' @param optimal Optimal design results
#' @param workflow_info Workflow information
#' @return Shiny UI tags$div or NULL
#' @noRd
render_power_achieved_display <- function(optimal, workflow_info) {
  if (is.null(optimal$achieved_power) || is.na(optimal$achieved_power) || 
      workflow_info$workflow_id %in% c("power_cost_TPM_cells_reads", "power_cost_fc_cells_reads")) {
    return(NULL)
  }
  
  tags$div(
    style = "margin-top: 12px;",
    tags$span("Power achieved: ", style = "color: #5A6B73; font-weight: 500;"),
    tags$span(paste0(round(optimal$achieved_power * 100, 1), "%"), style = "color: #28A745; font-weight: bold; font-size: 16px;")
  )
}

#' Create a standardized parameter display
#'
#' @param label Parameter label
#' @param value Parameter value
#' @param decimals Number of decimal places
#' @param font_size Font size for the value
#' @return Shiny UI tags$div
#' @noRd
create_parameter_display <- function(label, value, decimals = 0, font_size = "16px") {
  if (is.null(value) || is.na(value)) {
    return(NULL)
  }
  
  formatted_value <- if (is.numeric(value)) {
    round(value, decimals)
  } else {
    "N/A"
  }
  
  tags$div(
    style = "margin-bottom: 8px;",
    tags$span(label, style = "color: #5A6B73; font-weight: 500;"),
    tags$span(formatted_value, style = paste0("color: #2E86AB; font-weight: bold; font-size: ", font_size, ";"))
  )
}

#' Create a cost display
#'
#' @param cost_value Cost value
#' @return Shiny UI tags$div or NULL
#' @noRd
#' @importFrom scales comma
create_cost_display <- function(cost_value) {
  if (is.null(cost_value) || is.na(cost_value)) {
    return(NULL)
  }
  
  tags$div(
    style = "margin-bottom: 8px;",
    tags$span("Total cost: ", style = "color: #5A6B73; font-weight: 500;"),
    tags$span(
      paste0("$", scales::comma(round(cost_value))), 
      style = "color: #2E86AB; font-weight: bold; font-size: 16px;"
    )
  )
}

#' Create a power achieved display
#'
#' @param power_value Power achieved value
#' @return Shiny UI tags$div or NULL
#' @noRd
create_power_achieved_display <- function(power_value) {
  if (is.null(power_value) || is.na(power_value)) {
    return(NULL)
  }
  
  tags$div(
    style = "margin-bottom: 8px;",
    tags$span("Achieved power: ", style = "color: #5A6B73; font-weight: 500;"),
    tags$span(
      paste0(round(power_value * 100, 1), "%"), 
      style = "color: #28A745; font-weight: bold; font-size: 16px;"
    )
  )
}

#' Render fixed parameters section
#'
#' @param optimal Optimal design results
#' @param plots Plot objects
#' @param minimizing_param Minimizing parameter name
#' @return Shiny UI tags$div
#' @noRd
render_fixed_parameters_section <- function(optimal, plots, minimizing_param, workflow_info = NULL) {
  tags$div(
    style = "background-color: #F8F9FA; padding: 18px; border-radius: 5px; margin-bottom: 15px;",
    
    # Fixed Parameters header
    tags$div(
      tags$strong("Fixed Parameters", style = "color: #2E4A62; font-size: 14px; margin-bottom: 12px; display: block;")
    ),
    
    # Show non-minimizing parameters
    tagList(
      render_fixed_tpm_display(optimal, plots, minimizing_param),
      render_fixed_fc_display(optimal, plots, minimizing_param),
      render_fixed_cells_display(optimal, minimizing_param, workflow_info),
      render_fixed_reads_display(optimal, minimizing_param, workflow_info),
      render_mapping_efficiency_display(optimal)
    )
  )
}

#' Render fixed TPM display
#'
#' @param optimal Optimal design results
#' @param plots Plot objects
#' @param minimizing_param Minimizing parameter name
#' @return Shiny UI tags$div or NULL
#' @noRd
render_fixed_tpm_display <- function(optimal, plots, minimizing_param) {
  if (minimizing_param == "TPM_threshold") {
    return(NULL)
  }
  
  TPM_value <- if (!is.null(optimal$TPM_threshold) && !is.na(optimal$TPM_threshold)) {
    optimal$TPM_threshold
  } else if (!is.null(plots$plots$plot_data) && "TPM_threshold" %in% names(plots$plots$plot_data)) {
    unique(plots$plots$plot_data$TPM_threshold)[1]
  } else {
    10  # Default TPM threshold commonly used
  }
  
  if (!is.null(TPM_value) && !is.na(TPM_value)) {
    tags$div(
      style = "margin-bottom: 6px;",
      tags$span("TPM analysis threshold: ", style = "color: #5A6B73; font-weight: 400; font-size: 13px;"),
      tags$span(round(TPM_value), style = "color: #6C757D; font-weight: 500;")
    )
  }
}

#' Render fixed fold change display
#'
#' @param optimal Optimal design results
#' @param plots Plot objects
#' @param minimizing_param Minimizing parameter name
#' @return Shiny UI tags$div or NULL
#' @noRd
render_fixed_fc_display <- function(optimal, plots, minimizing_param) {
  if (minimizing_param == "minimum_fold_change") {
    return(NULL)
  }
  
  fc_value <- if (!is.null(optimal$minimum_fold_change) && !is.na(optimal$minimum_fold_change)) {
    optimal$minimum_fold_change
  } else {
    if (!is.null(plots$plots$plot_data) && "minimum_fold_change" %in% names(plots$plots$plot_data)) {
      unique(plots$plots$plot_data$minimum_fold_change)[1]
    } else {
      0.5  # Default fold change commonly used for downregulation
    }
  }
  
  if (!is.null(fc_value) && !is.na(fc_value)) {
    tags$div(
      style = "margin-bottom: 6px;",
      tags$span("Fold change: ", style = "color: #5A6B73; font-weight: 400; font-size: 13px;"),
      tags$span(round(fc_value, 2), style = "color: #6C757D; font-weight: 500;")
    )
  }
}

#' Render fixed cells display
#'
#' @param optimal Optimal design results
#' @param minimizing_param Minimizing parameter name
#' @return Shiny UI tags$div or NULL
#' @noRd
render_fixed_cells_display <- function(optimal, minimizing_param, workflow_info = NULL) {
  excluded_params <- c("cells_per_target", "cost")
  
  # For workflows 10-11 and constrained minimization, handle different varying scenarios
  if (!is.null(workflow_info) && 
      workflow_info$workflow_id %in% c("power_cost_TPM_cells_reads", "power_cost_fc_cells_reads", "power_cost_TPM_cells", "power_cost_TPM_reads", "power_cost_fc_cells", "power_cost_fc_reads")) {
    # If both cells and reads are varying (no specific varying_parameter), don't show in Fixed Parameters
    if (is.null(workflow_info$varying_parameter)) {
      return(NULL)  # Both varying - show in main Solution section
    }
    # If cells is varying (cost-constrained), don't show it here
    if (workflow_info$varying_parameter == "cells") {
      return(NULL)
    }
    # Otherwise, cells is fixed, so show it in Fixed Parameters section
  } else {
    # For workflows where both cells and reads are varying (like cost minimization),
    # don't show them in Fixed Parameters - they should be in the main Solution section
    if (!is.null(workflow_info) && workflow_info$category == "cost_only") {
      return(NULL)
    }
    # For power+cost workflows, exclude TPM_threshold and minimum_fold_change 
    # But for power-only workflows, we want to show cells/reads even when TPM/FC is being minimized
    if (!is.null(workflow_info) && workflow_info$category != "power_only_single") {
      excluded_params <- c(excluded_params, "TPM_threshold", "minimum_fold_change")
    }
  }
  
  if (minimizing_param %in% excluded_params || 
      is.null(optimal$cells_per_target) || is.na(optimal$cells_per_target)) {
    return(NULL)
  }
  
  tags$div(
    style = "margin-bottom: 6px;",
    tags$span("Cells per target: ", style = "color: #5A6B73; font-weight: 400; font-size: 13px;"),
    tags$span(round(optimal$cells_per_target), style = "color: #6C757D; font-weight: 500;")
  )
}

#' Render fixed reads display
#'
#' @param optimal Optimal design results
#' @param minimizing_param Minimizing parameter name
#' @return Shiny UI tags$div or NULL
#' @noRd
render_fixed_reads_display <- function(optimal, minimizing_param, workflow_info = NULL) {
  excluded_params <- c("reads_per_cell", "mapped_reads_per_cell", "cost")
  
  # For workflows 10-11 and constrained minimization, handle different varying scenarios
  if (!is.null(workflow_info) && 
      workflow_info$workflow_id %in% c("power_cost_TPM_cells_reads", "power_cost_fc_cells_reads", "power_cost_TPM_cells", "power_cost_TPM_reads", "power_cost_fc_cells", "power_cost_fc_reads")) {
    # If both cells and reads are varying (no specific varying_parameter), don't show in Fixed Parameters
    if (is.null(workflow_info$varying_parameter)) {
      return(NULL)  # Both varying - show in main Solution section
    }
    # If reads is varying (cost-constrained), don't show it here
    if (workflow_info$varying_parameter == "reads") {
      return(NULL)
    }
    # Otherwise, reads is fixed, so show it in Fixed Parameters section
  } else {
    # For workflows where both cells and reads are varying (like cost minimization),
    # don't show them in Fixed Parameters - they should be in the main Solution section
    if (!is.null(workflow_info) && workflow_info$category == "cost_only") {
      return(NULL)
    }
    # For power+cost workflows, exclude TPM_threshold and minimum_fold_change 
    # But for power-only workflows, we want to show cells/reads even when TPM/FC is being minimized
    if (!is.null(workflow_info) && workflow_info$category != "power_only_single") {
      excluded_params <- c(excluded_params, "TPM_threshold", "minimum_fold_change")
    }
  }
  
  if (minimizing_param %in% excluded_params || 
      is.null(optimal$sequenced_reads_per_cell) || is.na(optimal$sequenced_reads_per_cell)) {
    return(NULL)
  }
  
  tags$div(
    style = "margin-bottom: 6px;",
    tags$span("Sequenced reads per cell: ", style = "color: #5A6B73; font-weight: 400; font-size: 13px;"),
    tags$span(
      if (is.numeric(optimal$sequenced_reads_per_cell)) round(optimal$sequenced_reads_per_cell) else "N/A",
      style = "color: #6C757D; font-weight: 500;"
    )
  )
}

#' Render mapping efficiency display
#'
#' @param optimal Optimal design results
#' @return Shiny UI tags$div
#' @noRd
render_mapping_efficiency_display <- function(optimal) {
  mapping_eff <- if (!is.null(optimal$mapping_efficiency) && !is.na(optimal$mapping_efficiency)) {
    optimal$mapping_efficiency
  } else {
    0.72  # Default mapping efficiency commonly used
  }
  
  tags$div(
    style = "margin-bottom: 6px;",
    tags$span("Mapping efficiency: ", style = "color: #5A6B73; font-weight: 400; font-size: 13px;"),
    tags$span(paste0(round(mapping_eff * 100, 1), "%"), style = "color: #6C757D; font-weight: 500;")
  )
}

#' Render fallback when no solution is available
#'
#' @return Shiny UI tags$div
#' @noRd
render_no_solution_fallback <- function() {
  tags$div(
    style = "background-color: #F8F9FA; padding: 18px; border-radius: 5px; text-align: center;",
    tags$span("Solution information not available", style = "color: #999; font-style: italic;")
  )
}