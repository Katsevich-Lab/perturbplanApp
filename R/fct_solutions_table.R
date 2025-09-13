#' Solutions Table Creation Functions
#'
#' @description Functions for creating and managing solutions tables
#' that display analysis results in a structured format.
#'

#' Create solutions table with hierarchical parameter display
#'
#' @description Creates a comprehensive solutions table showing current solution
#' as the first row. Future rows will be added via Pin functionality.
#'
#' @param results Analysis results object
#' @param plots Plot objects
#' @param user_config Reactive containing user configuration
#' @return Shiny UI tagList with table
#' @noRd
create_solutions_table <- function(results, plots, user_config = reactive(NULL)) {
  if (is.null(results$optimal_design) || is.null(results$workflow_info)) {
    return(create_empty_solutions_table())
  }
  
  optimal <- results$optimal_design
  workflow_info <- results$workflow_info
  
  # Extract current solution data with access to slider values
  solution_row <- extract_solution_data(optimal, workflow_info, user_config, index = 1)
  
  # Create table structure
  create_solutions_table_ui(list(solution_row))
}

#' Extract solution data for table row
#'
#' @param optimal Optimal design results
#' @param workflow_info Workflow information
#' @param user_config Reactive containing user configuration
#' @param index Solution index number
#' @return List with solution data
#' @noRd
extract_solution_data <- function(optimal, workflow_info, user_config = reactive(NULL), index = 1) {
  list(
    index = index,
    achieved_power = extract_achieved_power(optimal),
    optimal_design = extract_optimal_design_value(optimal, workflow_info),
    experimental_choices = extract_experimental_choices(optimal, workflow_info, user_config),
    analysis_choices = extract_analysis_choices(optimal, workflow_info, user_config),
    effect_sizes = extract_effect_sizes(optimal, workflow_info, user_config)
  )
}

#' Create the actual table UI
#'
#' @param solution_rows List of solution row data
#' @return Shiny UI tags
#' @noRd
create_solutions_table_ui <- function(solution_rows) {
  # Determine which columns to show based on whether they have content
  visible_columns <- determine_visible_columns(solution_rows)
  
  # Create dynamic table header based on visible columns
  header_cells <- list(
    tags$th("Index", style = "width: 8%; text-align: center; font-weight: bold; background-color: #f8f9fa;"),
    tags$th("Achieved Power", style = "width: 12%; text-align: center; font-weight: bold; background-color: #f8f9fa;"),
    tags$th("Optimal Design", style = "width: 15%; text-align: center; font-weight: bold; background-color: #f8f9fa;")
  )
  
  # Add conditional parameter columns
  if (visible_columns$experimental_choices) {
    header_cells <- append(header_cells, list(
      tags$th("Experimental Choices", style = "width: 20%; text-align: left; font-weight: bold; background-color: #f8f9fa;")
    ))
  }
  
  if (visible_columns$analysis_choices) {
    header_cells <- append(header_cells, list(
      tags$th("Analysis Choices", style = "width: 20%; text-align: left; font-weight: bold; background-color: #f8f9fa;")
    ))
  }
  
  if (visible_columns$effect_sizes) {
    header_cells <- append(header_cells, list(
      tags$th("Effect Sizes", style = "width: 25%; text-align: left; font-weight: bold; background-color: #f8f9fa;")
    ))
  }
  
  table_header <- do.call(tags$tr, header_cells)
  
  # Create table rows with dynamic columns
  table_rows <- lapply(solution_rows, function(row_data) {
    row_cells <- list(
      # Index column
      tags$td(
        style = "text-align: center; padding: 12px; vertical-align: top; border-right: 1px solid #dee2e6;",
        tags$span(row_data$index, style = "font-size: 18px; font-weight: bold; color: #2E86AB;")
      ),
      
      # Achieved Power column
      tags$td(
        style = "text-align: center; padding: 12px; vertical-align: top; border-right: 1px solid #dee2e6;",
        if (!is.null(row_data$achieved_power)) {
          tags$span(paste0(round(row_data$achieved_power * 100, 1), "%"), 
                   style = "font-size: 16px; font-weight: bold; color: #28A745;")
        } else {
          tags$span("N/A", style = "color: #6c757d; font-style: italic;")
        }
      ),
      
      # Optimal Design column
      tags$td(
        style = "text-align: center; padding: 12px; vertical-align: top; border-right: 1px solid #dee2e6;",
        if (!is.null(row_data$optimal_design)) {
          tags$div(
            tags$div(row_data$optimal_design$label, style = "font-size: 11px; color: #6c757d; margin-bottom: 4px;"),
            if (grepl("<br>", row_data$optimal_design$value)) {
              # Multi-line content - render as HTML
              tags$div(
                HTML(row_data$optimal_design$value), 
                style = "font-size: 13px; font-weight: bold; color: #2E86AB; line-height: 1.4;"
              )
            } else {
              # Single line content
              tags$span(row_data$optimal_design$value, style = "font-size: 15px; font-weight: bold; color: #2E86AB;")
            }
          )
        } else {
          tags$span("N/A", style = "color: #6c757d; font-style: italic;")
        }
      )
    )
    
    # Add conditional parameter columns
    if (visible_columns$experimental_choices) {
      row_cells <- append(row_cells, list(
        tags$td(
          style = "padding: 12px; vertical-align: top; border-right: 1px solid #dee2e6;",
          create_parameter_section_display(row_data$experimental_choices, "Experimental")
        )
      ))
    }
    
    if (visible_columns$analysis_choices) {
      row_cells <- append(row_cells, list(
        tags$td(
          style = "padding: 12px; vertical-align: top; border-right: 1px solid #dee2e6;",
          create_parameter_section_display(row_data$analysis_choices, "Analysis")
        )
      ))
    }
    
    if (visible_columns$effect_sizes) {
      row_cells <- append(row_cells, list(
        tags$td(
          style = "padding: 12px; vertical-align: top;",
          create_parameter_section_display(row_data$effect_sizes, "Effect")
        )
      ))
    }
    
    do.call(tags$tr, row_cells)
  })
  
  # Return complete table
  tags$div(
    style = "overflow-x: auto;",
    tags$table(
      class = "table table-bordered",
      style = "width: 100%; margin: 0; border-collapse: collapse; font-size: 13px;",
      tags$thead(table_header),
      tags$tbody(table_rows)
    )
  )
}

#' Determine which columns should be visible
#'
#' @param solution_rows List of solution row data
#' @return List with visibility flags for each column type
#' @noRd
determine_visible_columns <- function(solution_rows) {
  # Check if any row has content for each category
  has_experimental <- any(sapply(solution_rows, function(row) {
    !is.null(row$experimental_choices) && length(row$experimental_choices) > 0
  }))
  
  has_analysis <- any(sapply(solution_rows, function(row) {
    !is.null(row$analysis_choices) && length(row$analysis_choices) > 0
  }))
  
  has_effect <- any(sapply(solution_rows, function(row) {
    !is.null(row$effect_sizes) && length(row$effect_sizes) > 0
  }))
  
  list(
    experimental_choices = has_experimental,
    analysis_choices = has_analysis,
    effect_sizes = has_effect
  )
}

#' Create parameter section display for table cells
#'
#' @param params List of parameters
#' @param section_type Type hint for styling
#' @return Shiny UI tags
#' @noRd
create_parameter_section_display <- function(params, section_type = "default") {
  if (is.null(params) || length(params) == 0) {
    return(tags$span("-", style = "color: #6c757d; font-style: italic;"))
  }
  
  tags$div(
    style = "line-height: 1.5;",
    lapply(names(params), function(param_name) {
      if (!is.null(params[[param_name]])) {
        tags$div(
          style = "margin-bottom: 4px;",
          tags$span(param_name, style = "color: #5A6B73; font-size: 12px; font-weight: 500;"),
          tags$span(": ", style = "color: #6c757d;"),
          tags$span(params[[param_name]], style = "color: #495057; font-weight: 500;")
        )
      }
    })
  )
}

#' Extract achieved power value
#'
#' @param optimal Optimal design results
#' @return Numeric power value or NULL
#' @noRd
extract_achieved_power <- function(optimal) {
  if (!is.null(optimal$achieved_power) && !is.na(optimal$achieved_power)) {
    return(optimal$achieved_power)
  }
  return(NULL)
}

#' Extract optimal design value based on workflow
#'
#' @param optimal Optimal design results
#' @param workflow_info Workflow information
#' @return List with label and value
#' @noRd
#' @importFrom scales comma
extract_optimal_design_value <- function(optimal, workflow_info) {
  minimizing_param <- workflow_info$minimizing_parameter
  
  # Check if this is a workflow where cells and reads are varying
  # (cost minimization or TPM/FC minimization with cells+reads varying)
  cells_and_reads_varying <- workflow_info$workflow_id %in% c(
    "power_cost_minimization",           # Cost minimization (cells+reads vary)
    "power_cost_TPM_cells_reads",       # TPM minimization with cells+reads varying
    "power_cost_fc_cells_reads"         # FC minimization with cells+reads varying
  )
  
  if (minimizing_param == "TPM_threshold") {
    if (cells_and_reads_varying) {
      # Show TPM + optimal cells and reads
      return(list(
        label = "Optimal Design",
        value = create_multi_param_display(list(
          "TPM Threshold" = if (!is.null(optimal$TPM_threshold)) round(optimal$TPM_threshold) else "N/A",
          "Cells per target" = if (!is.null(optimal$cells_per_target)) scales::comma(round(optimal$cells_per_target)) else "N/A",
          "Reads per cell" = if (!is.null(optimal$sequenced_reads_per_cell)) scales::comma(round(optimal$sequenced_reads_per_cell)) else "N/A"
        ))
      ))
    } else {
      return(list(
        label = "TPM Threshold",
        value = if (!is.null(optimal$TPM_threshold)) round(optimal$TPM_threshold) else "N/A"
      ))
    }
  } else if (minimizing_param == "minimum_fold_change") {
    if (cells_and_reads_varying) {
      # Show FC + optimal cells and reads
      return(list(
        label = "Optimal Design",
        value = create_multi_param_display(list(
          "Fold Change" = if (!is.null(optimal$minimum_fold_change)) round(optimal$minimum_fold_change, 2) else "N/A",
          "Cells per target" = if (!is.null(optimal$cells_per_target)) scales::comma(round(optimal$cells_per_target)) else "N/A",
          "Reads per cell" = if (!is.null(optimal$sequenced_reads_per_cell)) scales::comma(round(optimal$sequenced_reads_per_cell)) else "N/A"
        ))
      ))
    } else {
      return(list(
        label = "Fold Change",
        value = if (!is.null(optimal$minimum_fold_change)) round(optimal$minimum_fold_change, 2) else "N/A"
      ))
    }
  } else if (minimizing_param == "cells_per_target") {
    return(list(
      label = "Cells per Target",
      value = if (!is.null(optimal$cells_per_target)) scales::comma(round(optimal$cells_per_target)) else "N/A"
    ))
  } else if (minimizing_param %in% c("reads_per_cell", "reads_per_cell")) {
    return(list(
      label = "Reads per Cell",
      value = if (!is.null(optimal$sequenced_reads_per_cell)) scales::comma(round(optimal$sequenced_reads_per_cell)) else "N/A"
    ))
  } else if (minimizing_param == "cost") {
    # Cost minimization: Show total cost + optimal cells and reads
    return(list(
      label = "Optimal Design",
      value = create_multi_param_display(list(
        "Total Cost" = if (!is.null(optimal$total_cost)) paste0("$", scales::comma(round(optimal$total_cost))) else "N/A",
        "Cells per target" = if (!is.null(optimal$cells_per_target)) scales::comma(round(optimal$cells_per_target)) else "N/A",
        "Reads per cell" = if (!is.null(optimal$sequenced_reads_per_cell)) scales::comma(round(optimal$sequenced_reads_per_cell)) else "N/A"
      ))
    ))
  }
  
  return(list(label = "Unknown", value = "N/A"))
}

#' Create formatted display for multiple parameters in optimal design
#'
#' @param param_list Named list of parameters and their values
#' @return HTML formatted string
#' @noRd
create_multi_param_display <- function(param_list) {
  param_strings <- lapply(names(param_list), function(name) {
    paste0(name, ": ", param_list[[name]])
  })
  paste(param_strings, collapse = "<br>")
}

#' Extract experimental choices for display
#'
#' @param optimal Optimal design results
#' @param user_config Reactive containing user configuration
#' @return Named list of experimental choice parameters (only those with sliders)
#' @noRd
extract_experimental_choices <- function(optimal, workflow_info = NULL, user_config = reactive(NULL)) {
  params <- list()
  
  # Get the parameter being minimized and workflow type
  minimizing_param <- NULL
  is_cost_minimization <- FALSE
  if (!is.null(workflow_info)) {
    minimizing_param <- workflow_info$minimizing_parameter
    is_cost_minimization <- workflow_info$workflow_id == "power_cost_minimization"
  }
  
  # param_manager removed - this function now returns empty list
  # TODO: Implement user_config-based parameter extraction if needed
  
  return(params)
}

#' Extract analysis choices for display
#'
#' @param optimal Optimal design results
#' @param workflow_info Workflow information
#' @param user_config Reactive containing user configuration
#' @return Named list of analysis choice parameters (only those with sliders, excluding minimized parameter)
#' @noRd
extract_analysis_choices <- function(optimal, workflow_info, user_config = reactive(NULL)) {
  minimizing_param <- workflow_info$minimizing_parameter
  params <- list()
  
  # param_manager removed - this function now returns empty list
  # TODO: Implement user_config-based parameter extraction if needed
  
  return(params)
}

#' Extract effect sizes for display
#'
#' @param optimal Optimal design results
#' @param workflow_info Workflow information
#' @param user_config Reactive containing user configuration
#' @return Named list of effect size parameters (only those with sliders, excluding minimized parameter)
#' @noRd
extract_effect_sizes <- function(optimal, workflow_info, user_config = reactive(NULL)) {
  minimizing_param <- workflow_info$minimizing_parameter
  params <- list()
  
  # param_manager removed - this function now returns empty list
  # TODO: Implement user_config-based parameter extraction if needed
  
  # Note: Prop non-null is typically not a slider parameter, so we don't include it
  # unless it specifically appears in sliders
  
  return(params)
}

#' Create empty solutions table when no data available
#'
#' @return Shiny UI tags
#' @noRd
create_empty_solutions_table <- function() {
  tags$div(
    style = "text-align: center; padding: 40px; color: #7A8B93;",
    tags$p("No solutions available yet. Run an analysis to see results.", style = "font-style: italic;")
  )
}

#' Check if a parameter has a slider in the current workflow
#'
#' @param param_name Parameter name to check
#' @param user_config Reactive containing user configuration
#' @param workflow_info Workflow information
#' @return Boolean indicating if parameter has a slider
#' @noRd
parameter_has_slider <- function(param_name, user_config = reactive(NULL), workflow_info) {
  # This function determines if a parameter appears in Row 2 of the parameter sliders
  # based on the slider logic from mod_parameter_sliders.R
  
  if (is.null(workflow_info) || is.null(workflow_info$workflow_id)) {
    return(FALSE)
  }
  
  # Get the minimized parameter for this workflow
  minimized_param <- get_minimized_parameter_for_display(workflow_info$workflow_id)
  
  # Parameters in Row 2 are: cells_per_target, reads_per_cell, TPM_threshold, minimum_fold_change
  # But exclude the minimized parameter
  row2_params <- c("cells_per_target", "reads_per_cell", "TPM_threshold", "minimum_fold_change")
  
  # Map parameter names
  param_mapping <- list(
    "cells_per_target" = "cells_per_target",
    "reads_per_cell" = "reads_per_cell", 
    "TPM_threshold" = "TPM_threshold",
    "minimum_fold_change" = "minimum_fold_change"
  )
  
  # Check if parameter is in Row 2 and not minimized
  if (param_name %in% names(param_mapping)) {
    mapped_param <- param_mapping[[param_name]]
    is_in_row2 <- mapped_param %in% row2_params
    is_not_minimized <- is.null(minimized_param) || length(minimized_param) == 0 || mapped_param != minimized_param
    
    # For cost minimization workflow, also check power+cost filtering logic
    if (!is.null(user_config)) {
      config <- user_config()
      if (!is.null(config) && !is.null(config$design_options)) {
        design_config <- config$design_options
        param_controls <- design_config$parameter_controls
        optimization_type <- design_config$optimization_type
        
        # Apply power+cost mode filtering (show only "fixed" parameters)
        if (!is.null(optimization_type) && optimization_type == "power_cost" && !is.null(param_controls)) {
          control_name_mapping <- list(
            "cells_per_target" = "cells_per_target",
            "reads_per_cell" = "reads_per_cell",
            "TPM_threshold" = "TPM_threshold", 
            "minimum_fold_change" = "minimum_fold_change"
          )
          
          control_name <- control_name_mapping[[param_name]]
          if (!is.null(control_name) && !is.null(param_controls[[control_name]])) {
            param_type <- param_controls[[control_name]]$type
            return(is_in_row2 && is_not_minimized && param_type == "fixed")
          }
        }
        
        # For cost minimization, check if it's TPM or FC (they are shown as sliders)
        if (!is.null(minimized_param) && length(minimized_param) > 0 && minimized_param == "cost") {
          return(param_name %in% c("TPM_threshold", "minimum_fold_change"))
        }
      }
    }
    
    return(is_in_row2 && is_not_minimized)
  }
  
  return(FALSE)
}

#' Get minimized parameter for display checking (same logic as mod_parameter_sliders.R)
#'
#' @param workflow_id Workflow ID
#' @return Minimized parameter name
#' @noRd
get_minimized_parameter_for_display <- function(workflow_id) {
  minimized_map <- list(
    # Power-only workflows
    "power_single_cells_per_target" = "cells_per_target",
    "power_single_reads_per_cell" = "reads_per_cell", 
    "power_single_TPM_threshold" = "TPM_threshold",
    "power_single_minimum_fold_change" = "minimum_fold_change",
    
    # Cost minimization workflow
    "power_cost_minimization" = "cost",
    
    # Power+cost workflows
    "power_cost_TPM_cells" = "TPM_threshold",
    "power_cost_TPM_reads" = "TPM_threshold",
    "power_cost_fc_cells" = "minimum_fold_change",
    "power_cost_fc_reads" = "minimum_fold_change"
  )
  
  return(minimized_map[[workflow_id]] %||% character(0))
}

#' Create enhanced solutions table (Dev branch approach)
#'
#' @description Creates an enhanced solutions table similar to the dev branch approach.
#' This unifies the solution table and summary into a single comprehensive display.
#'
#' @param results Analysis results object
#' @param plots Plot objects
#' @param user_config Reactive containing user configuration
#' @return Shiny UI tagList with enhanced table
#' @export
create_enhanced_solutions_table <- function(results, plots, user_config = reactive(NULL)) {
  # For now, use the existing create_solutions_table as the foundation
  # In future iterations, this will be enhanced with dev branch-style parameter extraction
  
  if (is.null(results$optimal_design) || is.null(results$workflow_info)) {
    return(create_empty_solutions_table())
  }
  
  # Create the standard solution table for now
  table_ui <- create_solutions_table(results, plots, user_config)
  
  # Return with enhanced styling container
  tags$div(
    class = "enhanced-solutions-table",
    style = "background: white; border-radius: 8px; box-shadow: 0 2px 4px rgba(0,0,0,0.1);",
    table_ui
  )
}