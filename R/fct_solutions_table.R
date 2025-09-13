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

# ========================================================================
# HELPER FUNCTIONS FOR PARAMETER EXTRACTION
# ========================================================================

#' Check if Parameter Has Slider in Current Workflow
#'
#' @description Determines if a parameter should have a slider visible
#' based on workflow logic: parameter must have slider AND not be minimizing
#'
#' @param config User configuration containing parameter states
#' @param parameter_name Parameter name to check
#'
#' @return Logical indicating if parameter should show slider
has_parameter_with_slider <- function(config, parameter_name) {
  # Check if parameter exists in design options with non-minimizing state
  if (!is.null(config$design_options)) {
    param_control_key <- paste0(parameter_name, "_control")
    if (param_control_key %in% names(config$design_options)) {
      param_state <- config$design_options[[param_control_key]]
      return(param_state != "minimizing")
    }
  }
  
  # Default: assume parameter has slider if not explicitly minimizing
  return(TRUE)
}

#' Extract Parameter Value from User Config
#'
#' @description Extracts parameter value from user_config structure
#' using canonical naming patterns from naming_standards.R
#'
#' @param parameter_name Parameter name to extract
#' @param config User configuration structure
#'
#' @return Parameter value or NULL if not found
get_parameter_value_from_config <- function(parameter_name, config) {
  if (is.null(config)) {
    return(NULL)
  }
  
  # Map parameter names to their expected locations in config
  param_mapping <- list(
    "cells_per_target" = list(section = "experimental_setup", key = "cells_fixed"),
    "reads_per_cell" = list(section = "experimental_setup", key = "mapped_reads_per_cell_fixed"),
    "MOI" = list(section = "experimental_setup", key = "MOI_fixed"),
    "number_of_targets" = list(section = "experimental_setup", key = "number_of_targets_fixed"),
    "gRNAs_per_target" = list(section = "experimental_setup", key = "gRNAs_per_target_fixed"),
    "TPM_threshold" = list(section = "analysis_choices", key = "TPM_threshold_fixed"),
    "minimum_fold_change" = list(section = "effect_sizes", key = "minimum_fold_change_fixed"),
    "proportion_non_null" = list(section = "effect_sizes", key = "proportion_non_null_fixed")
  )
  
  if (parameter_name %in% names(param_mapping)) {
    mapping <- param_mapping[[parameter_name]]
    section <- config[[mapping$section]]
    if (!is.null(section) && mapping$key %in% names(section)) {
      return(section[[mapping$key]])
    }
  }
  
  return(NULL)
}

#' Format Number for Display
#'
#' @description Formats numeric values for consistent display in table
#'
#' @param value Numeric value to format
#' @param decimal_places Number of decimal places (default: 2)
#'
#' @return Formatted string representation
format_number <- function(value, decimal_places = 2) {
  if (is.null(value) || is.na(value)) {
    return("--")
  }
  
  if (!is.numeric(value)) {
    return(as.character(value))
  }
  
  # Format with appropriate decimal places
  if (value == round(value)) {
    # Integer values: no decimal places
    return(formatC(value, format = "f", digits = 0, big.mark = ","))
  } else {
    # Decimal values: specified decimal places
    return(formatC(value, format = "f", digits = decimal_places, big.mark = ","))
  }
}

# ========================================================================
# ENHANCED PARAMETER EXTRACTION FUNCTIONS
# ========================================================================

#' Extract experimental choices for display (Enhanced version)
#'
#' @param optimal Optimal design results
#' @param workflow_info Workflow information
#' @param user_config Reactive containing user configuration
#' @return Named list of experimental choice parameters (only those with sliders and not minimized)
#' @noRd
extract_experimental_choices <- function(optimal, workflow_info = NULL, user_config = reactive(NULL)) {
  params <- list()
  
  if (is.null(workflow_info) || is.null(user_config)) {
    return(params)
  }
  
  config <- user_config()
  if (is.null(config)) {
    return(params)
  }
  
  minimizing_param <- workflow_info$minimizing_parameter
  
  # Extract experimental parameters that should appear as subcolumns
  # Only show parameters that have sliders AND are not being minimized
  
  # Cells per target (unless it's being minimized)
  if (has_parameter_with_slider(config, "cells_per_target") && minimizing_param != "cells_per_target") {
    value <- get_parameter_value_from_config("cells_per_target", config)
    if (!is.null(value)) {
      params[["Cells per target"]] <- format_number(value)
    }
  }
  
  # Reads per cell (unless it's being minimized)
  if (has_parameter_with_slider(config, "reads_per_cell") && !minimizing_param %in% c("reads_per_cell", "sequenced_reads_per_cell")) {
    value <- get_parameter_value_from_config("reads_per_cell", config)
    if (!is.null(value)) {
      params[["Reads per cell"]] <- format_number(value)
    }
  }
  
  # MOI (if available and not minimized)
  if (has_parameter_with_slider(config, "MOI") && minimizing_param != "MOI") {
    value <- get_parameter_value_from_config("MOI", config)
    if (!is.null(value)) {
      params[["MOI"]] <- value
    }
  }
  
  # Number of targets (if available and not minimized)
  if (has_parameter_with_slider(config, "number_of_targets") && minimizing_param != "number_of_targets") {
    value <- get_parameter_value_from_config("number_of_targets", config)
    if (!is.null(value)) {
      params[["Number of targets"]] <- format_number(value)
    }
  }
  
  # gRNAs per target (if available and not minimized)
  if (has_parameter_with_slider(config, "gRNAs_per_target") && minimizing_param != "gRNAs_per_target") {
    value <- get_parameter_value_from_config("gRNAs_per_target", config)
    if (!is.null(value)) {
      params[["gRNAs per target"]] <- value
    }
  }
  
  return(params)
}

#' Extract analysis choices for display (Enhanced version)
#'
#' @param optimal Optimal design results
#' @param workflow_info Workflow information
#' @param user_config Reactive containing user configuration
#' @return Named list of analysis choice parameters (only those with sliders, excluding minimized parameter)
#' @noRd
extract_analysis_choices <- function(optimal, workflow_info, user_config = reactive(NULL)) {
  params <- list()
  
  if (is.null(workflow_info) || is.null(user_config)) {
    return(params)
  }
  
  config <- user_config()
  if (is.null(config)) {
    return(params)
  }
  
  minimizing_param <- workflow_info$minimizing_parameter
  
  # TPM threshold (unless it's being minimized)
  if (has_parameter_with_slider(config, "TPM_threshold") && minimizing_param != "TPM_threshold") {
    value <- get_parameter_value_from_config("TPM_threshold", config)
    if (!is.null(value)) {
      params[["TPM threshold"]] <- round(value)
    }
  }
  
  return(params)
}

#' Extract effect sizes for display (Enhanced version)
#'
#' @param optimal Optimal design results
#' @param workflow_info Workflow information
#' @param user_config Reactive containing user configuration
#' @return Named list of effect size parameters (only those with sliders, excluding minimized parameter)
#' @noRd
extract_effect_sizes <- function(optimal, workflow_info, user_config = reactive(NULL)) {
  params <- list()
  
  if (is.null(workflow_info) || is.null(user_config)) {
    return(params)
  }
  
  config <- user_config()
  if (is.null(config)) {
    return(params)
  }
  
  minimizing_param <- workflow_info$minimizing_parameter
  
  # Fold change (unless it's being minimized)
  if (has_parameter_with_slider(config, "minimum_fold_change") && minimizing_param != "minimum_fold_change") {
    value <- get_parameter_value_from_config("minimum_fold_change", config)
    if (!is.null(value)) {
      params[["Fold change"]] <- round(value, 2)
    }
  }
  
  # Proportion non-null pairs (if available as slider and not minimized)
  if (has_parameter_with_slider(config, "prop_non_null") && minimizing_param != "prop_non_null") {
    value <- get_parameter_value_from_config("prop_non_null", config)
    if (!is.null(value)) {
      params[["Proportion non-null pairs"]] <- round(value, 3)
    }
  }
  
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
  # Enhanced Phase 2 implementation with dev branch-style parameter extraction and subcolumns
  
  if (is.null(results$optimal_design) || is.null(results$workflow_info)) {
    return(create_empty_solutions_table())
  }
  
  # Extract solution data using enhanced parameter extraction (same pattern as original)
  optimal <- results$optimal_design
  workflow_info <- results$workflow_info
  
  # Create solution data with enhanced parameter extraction
  solution_row <- extract_solution_data(optimal, workflow_info, user_config, index = 1)
  solution_rows <- list(solution_row)
  
  # Create the enhanced table with two-row header structure and intelligent subcolumns
  table_ui <- create_enhanced_solutions_table_ui(solution_rows, results$workflow_info)
  
  # Return with enhanced styling container
  tags$div(
    class = "enhanced-solutions-table",
    style = "background: white; border-radius: 8px; box-shadow: 0 2px 4px rgba(0,0,0,0.1);",
    table_ui
  )
}

#' Determine which experimental parameter subcolumns to show
#'
#' @description Checks which experimental parameters appear across all solutions
#' and returns configuration for visible subcolumns with headers and widths.
#'
#' @param solution_rows List of solution row data
#' @return List of experimental parameter column information
#' @noRd
determine_experimental_subcolumns <- function(solution_rows) {
  # Check which experimental parameters appear across all solutions
  all_exp_params <- list()
  for (row in solution_rows) {
    if (!is.null(row$experimental_choices) && length(row$experimental_choices) > 0) {
      all_exp_params <- c(all_exp_params, names(row$experimental_choices))
    }
  }
  
  # Get unique parameter names and create ordered list
  unique_params <- unique(all_exp_params)
  
  # Define parameter order and column headers (from dev branch)
  param_order <- list(
    "MOI" = list(header = "MOI", width = "6%"),
    "Number of targets" = list(header = "# Targets", width = "6%"),
    "gRNAs per target" = list(header = "gRNAs/Target", width = "6%"),
    "Cells per target" = list(header = "Cells/Target", width = "6%"),
    "Reads per cell" = list(header = "Reads/Cell", width = "6%"),
    "Cells/target" = list(header = "Cells/Target", width = "6%"),
    "Reads/cell" = list(header = "Reads/Cell", width = "6%")
  )
  
  # Return only the parameters that actually appear
  visible_params <- list()
  for (param_name in names(param_order)) {
    if (param_name %in% unique_params) {
      visible_params[[param_name]] <- param_order[[param_name]]
    }
  }
  
  return(visible_params)
}

#' Determine which effect sizes parameter subcolumns to show
#'
#' @description Checks which effect sizes parameters appear across all solutions
#' and returns configuration for visible subcolumns with headers and widths.
#'
#' @param solution_rows List of solution row data
#' @return List of effect sizes parameter column information
#' @noRd
determine_effect_sizes_subcolumns <- function(solution_rows) {
  # Check which effect sizes parameters appear across all solutions
  all_effect_params <- list()
  for (row in solution_rows) {
    if (!is.null(row$effect_sizes) && length(row$effect_sizes) > 0) {
      all_effect_params <- c(all_effect_params, names(row$effect_sizes))
    }
  }
  
  # Get unique parameter names and create ordered list
  unique_params <- unique(all_effect_params)
  
  # Define parameter order and column headers for effect sizes (from dev branch)
  param_order <- list(
    "Fold change" = list(header = "Fold Change", width = "8%"),
    "Proportion non-null pairs" = list(header = "Non-null Prop", width = "8%")
  )
  
  # Return only the parameters that actually appear
  visible_params <- list()
  for (param_name in names(param_order)) {
    if (param_name %in% unique_params) {
      visible_params[[param_name]] <- param_order[[param_name]]
    }
  }
  
  return(visible_params)
}

#' Get optimal design column name based on minimizing parameter
#'
#' @description Maps the minimizing parameter to a readable column header name
#' following dev branch naming conventions.
#'
#' @param workflow_info Workflow information containing minimizing parameter
#' @return Column title string
#' @noRd
get_optimal_design_column_name <- function(workflow_info) {
  if (is.null(workflow_info) || is.null(workflow_info$minimizing_parameter)) {
    return("Optimal Design")
  }
  
  # Map minimizing parameters to readable column names (from dev branch)
  param_name_mapping <- list(
    "cells_per_target" = "Optimal Cells per Target",
    "reads_per_cell" = "Optimal Reads per Cell", 
    "sequenced_reads_per_cell" = "Optimal Reads per Cell",
    "TPM_threshold" = "Optimal TPM Threshold",
    "minimum_fold_change" = "Optimal Fold Change",
    "cost" = "Optimal Cost"
  )
  
  column_name <- param_name_mapping[[workflow_info$minimizing_parameter]]
  if (is.null(column_name)) {
    # Fallback for unknown parameters
    return(paste("Optimal", tools::toTitleCase(gsub("_", " ", workflow_info$minimizing_parameter))))
  }
  
  return(column_name)
}

#' Update determine_visible_columns for enhanced table structure
#'
#' @description Enhanced version that considers workflow info and cost columns
#' in addition to basic content checking.
#'
#' @param solution_rows List of solution row data
#' @param workflow_info Workflow information for enhanced logic
#' @return List with visibility flags for each column type
#' @noRd
determine_visible_columns_enhanced <- function(solution_rows, workflow_info = NULL) {
  # Basic visibility checks
  has_experimental <- any(sapply(solution_rows, function(row) {
    !is.null(row$experimental_choices) && length(row$experimental_choices) > 0
  }))
  
  has_analysis <- any(sapply(solution_rows, function(row) {
    !is.null(row$analysis_choices) && length(row$analysis_choices) > 0
  }))
  
  has_effect <- any(sapply(solution_rows, function(row) {
    !is.null(row$effect_sizes) && length(row$effect_sizes) > 0
  }))
  
  # Check for cost column based on workflow type
  has_cost <- FALSE
  if (!is.null(workflow_info) && !is.null(workflow_info$workflow_id)) {
    cost_workflows <- c("power_cost_minimization", "power_cost_TPM_cells", 
                       "power_cost_TPM_reads", "power_cost_TPM_cells_reads",
                       "power_cost_fc_cells", "power_cost_fc_reads", 
                       "power_cost_fc_cells_reads")
    has_cost <- workflow_info$workflow_id %in% cost_workflows
    
    # Also check if any solution has cost data
    if (!has_cost) {
      has_cost <- any(sapply(solution_rows, function(row) {
        !is.null(row$total_cost) && !is.na(row$total_cost)
      }))
    }
  }
  
  return(list(
    experimental_choices = has_experimental,
    analysis_choices = has_analysis,
    effect_sizes = has_effect,
    total_cost = has_cost
  ))
}

#' Create enhanced solutions table UI with two-row header structure
#'
#' @description Creates the sophisticated two-row header table structure
#' matching the dev branch approach with subcolumns and dynamic visibility.
#'
#' @param solution_rows List of solution row data
#' @param workflow_info Workflow information for column naming
#' @return Shiny UI tags for the complete table
#' @noRd
create_enhanced_solutions_table_ui <- function(solution_rows, workflow_info = NULL) {
  # Determine which columns to show and their subcolumn structure
  visible_columns <- determine_visible_columns_enhanced(solution_rows, workflow_info)
  experimental_subcolumns <- determine_experimental_subcolumns(solution_rows)
  effect_sizes_subcolumns <- determine_effect_sizes_subcolumns(solution_rows)
  
  # Calculate dynamic widths based on dev branch logic
  exp_param_count <- length(experimental_subcolumns)
  exp_total_width <- if (exp_param_count > 0) 30 else 0  # 30% total for experimental params
  exp_subcolumn_width <- if (exp_param_count > 0) paste0(round(exp_total_width / exp_param_count, 1), "%") else "0%"
  
  effect_param_count <- length(effect_sizes_subcolumns)
  effect_total_width <- if (effect_param_count > 0) 16 else 0  # 16% total for effect sizes
  effect_subcolumn_width <- if (effect_param_count > 0) paste0(round(effect_total_width / effect_param_count, 1), "%") else "0%"
  
  # Build the two-row header structure
  header_rows <- build_two_row_header(visible_columns, experimental_subcolumns, effect_sizes_subcolumns, 
                                     exp_total_width, effect_total_width, exp_subcolumn_width, effect_subcolumn_width, workflow_info)
  
  # Create table rows with subcolumn values
  table_rows <- create_enhanced_solution_rows(solution_rows, experimental_subcolumns, effect_sizes_subcolumns, visible_columns)
  
  # Return complete table structure
  tags$div(
    style = "overflow-x: auto;",
    tags$table(
      class = "table table-bordered",
      style = "width: 100%; margin: 0; border-collapse: collapse; font-size: 13px;",
      tags$thead(header_rows),
      tags$tbody(table_rows)
    )
  )
}

#' Build the two-row header structure
#'
#' @description Creates the complex two-row header with main columns and subcolumns
#' using proper colspan and rowspan attributes.
#'
#' @param visible_columns List of column visibility flags
#' @param experimental_subcolumns List of experimental subcolumn configs
#' @param effect_sizes_subcolumns List of effect sizes subcolumn configs  
#' @param exp_total_width Total width for experimental parameters
#' @param effect_total_width Total width for effect sizes
#' @param exp_subcolumn_width Individual experimental subcolumn width
#' @param effect_subcolumn_width Individual effect sizes subcolumn width
#' @param workflow_info Workflow information for column naming
#' @return List of table header row elements
#' @noRd
build_two_row_header <- function(visible_columns, experimental_subcolumns, effect_sizes_subcolumns,
                                exp_total_width, effect_total_width, exp_subcolumn_width, effect_subcolumn_width, workflow_info) {
  
  optimal_design_column_name <- get_optimal_design_column_name(workflow_info)
  exp_param_count <- length(experimental_subcolumns)
  effect_param_count <- length(effect_sizes_subcolumns)
  
  # ========================================================================
  # FIRST HEADER ROW (Main columns with colspan for subcolumn groups)
  # ========================================================================
  header_row_1 <- list(
    tags$th("Solution ID", rowspan = "2", style = "width: 8%; text-align: center; font-weight: bold; background-color: #f8f9fa; vertical-align: middle;"),
    tags$th("Power", rowspan = "2", style = "width: 8%; text-align: center; font-weight: bold; background-color: #f8f9fa; vertical-align: middle;")
  )
  
  # Add cost column if visible (after power, before optimal design)
  if (visible_columns$total_cost) {
    header_row_1 <- append(header_row_1, list(
      tags$th("Cost", rowspan = "2", style = "width: 10%; text-align: center; font-weight: bold; background-color: #f8f9fa; vertical-align: middle;")
    ))
  }
  
  # Add optimal design column
  header_row_1 <- append(header_row_1, list(
    tags$th(optimal_design_column_name, rowspan = "2", style = "width: 15%; text-align: center; font-weight: bold; background-color: #f8f9fa; vertical-align: middle;")
  ))
  
  # Add experimental parameters main header if subcolumns exist
  if (visible_columns$experimental_choices && exp_param_count > 0) {
    header_row_1 <- append(header_row_1, list(
      tags$th("Experimental Parameters", colspan = as.character(exp_param_count), 
              style = paste0("width: ", exp_total_width, "%; text-align: center; font-weight: bold; background-color: #f8f9fa;"))
    ))
  }
  
  # Add TPM threshold column if visible (single column, no subcolumns)
  if (visible_columns$analysis_choices) {
    header_row_1 <- append(header_row_1, list(
      tags$th("TPM threshold", rowspan = "2", style = "width: 10%; text-align: center; font-weight: bold; background-color: #f8f9fa; vertical-align: middle;")
    ))
  }
  
  # Add effect sizes main header if subcolumns exist
  if (visible_columns$effect_sizes && effect_param_count > 0) {
    header_row_1 <- append(header_row_1, list(
      tags$th("Effect Sizes", colspan = as.character(effect_param_count), 
              style = paste0("width: ", effect_total_width, "%; text-align: center; font-weight: bold; background-color: #f8f9fa;"))
    ))
  } else if (visible_columns$effect_sizes) {
    # Single effect sizes column if no subcolumns
    header_row_1 <- append(header_row_1, list(
      tags$th("Effect Sizes", rowspan = "2", style = "width: 16%; text-align: center; font-weight: bold; background-color: #f8f9fa; vertical-align: middle;")
    ))
  }
  
  # ========================================================================
  # SECOND HEADER ROW (Subcolumn headers for experimental parameters and effect sizes)
  # ========================================================================
  header_row_2 <- list()
  
  # Add experimental parameter subcolumn headers
  if (visible_columns$experimental_choices && exp_param_count > 0) {
    for (param_name in names(experimental_subcolumns)) {
      param_info <- experimental_subcolumns[[param_name]]
      header_row_2 <- append(header_row_2, list(
        tags$th(param_info$header, style = paste0("width: ", exp_subcolumn_width, "; text-align: center; font-weight: bold; background-color: #e9ecef; font-size: 12px;"))
      ))
    }
  }
  
  # Add effect sizes subcolumn headers
  if (visible_columns$effect_sizes && effect_param_count > 0) {
    for (param_name in names(effect_sizes_subcolumns)) {
      param_info <- effect_sizes_subcolumns[[param_name]]
      header_row_2 <- append(header_row_2, list(
        tags$th(param_info$header, style = paste0("width: ", effect_subcolumn_width, "; text-align: center; font-weight: bold; background-color: #e9ecef; font-size: 12px;"))
      ))
    }
  }
  
  # Create final header structure
  if (length(header_row_2) > 0) {
    # Two-row header when subcolumns exist
    list(
      do.call(tags$tr, header_row_1),
      do.call(tags$tr, header_row_2)
    )
  } else {
    # Single-row header when no subcolumns
    list(do.call(tags$tr, header_row_1))
  }
}

#' Create enhanced solution rows with subcolumn values
#'
#' @description Creates solution table rows that properly align with the two-row
#' header structure, including subcolumn values for experimental parameters and effect sizes.
#'
#' @param solution_rows List of solution row data
#' @param experimental_subcolumns List of experimental subcolumn configs
#' @param effect_sizes_subcolumns List of effect sizes subcolumn configs
#' @param visible_columns List of column visibility flags
#' @return List of table row elements
#' @noRd
create_enhanced_solution_rows <- function(solution_rows, experimental_subcolumns, effect_sizes_subcolumns, visible_columns) {
  lapply(solution_rows, function(row_data) {
    # Build cells in the same order as header columns
    cells <- list()
    
    # Solution ID cell
    cells[[length(cells) + 1]] <- tags$td(
      style = "text-align: center; padding: 12px; vertical-align: top; border-right: 1px solid #dee2e6;",
      tags$span(row_data$index, style = "font-size: 18px; font-weight: bold; color: #2E86AB;")
    )
    
    # Power cell
    cells[[length(cells) + 1]] <- tags$td(
      style = "text-align: center; padding: 12px; vertical-align: top; border-right: 1px solid #dee2e6;",
      if (!is.null(row_data$achieved_power)) {
        tags$span(paste0(round(row_data$achieved_power * 100, 1), "%"), 
                 style = "font-size: 16px; font-weight: bold; color: #28A745;")
      } else {
        tags$span("N/A", style = "color: #6c757d; font-style: italic;")
      }
    )
    
    # Cost cell (if visible)
    if (visible_columns$total_cost) {
      cells[[length(cells) + 1]] <- tags$td(
        style = "text-align: center; padding: 12px; vertical-align: top; border-right: 1px solid #dee2e6;",
        if (!is.null(row_data$total_cost)) {
          tags$span(paste0("$", scales::comma(round(row_data$total_cost))), 
                   style = "font-size: 15px; font-weight: bold; color: #2E86AB;")
        } else {
          tags$span("N/A", style = "color: #6c757d; font-style: italic;")
        }
      )
    }
    
    # Optimal Design cell
    cells[[length(cells) + 1]] <- tags$td(
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
    
    # Experimental parameter subcolumn cells
    if (visible_columns$experimental_choices && length(experimental_subcolumns) > 0) {
      for (param_name in names(experimental_subcolumns)) {
        value <- row_data$experimental_choices[[param_name]]
        cells[[length(cells) + 1]] <- tags$td(
          style = "text-align: center; padding: 12px; vertical-align: top; border-right: 1px solid #dee2e6;",
          if (!is.null(value)) {
            tags$span(value, style = "font-size: 13px; font-weight: 500; color: #495057;")
          } else {
            tags$span("-", style = "color: #6c757d; font-style: italic;")
          }
        )
      }
    }
    
    # TPM threshold cell (if visible)
    if (visible_columns$analysis_choices) {
      tpm_value <- row_data$analysis_choices[["TPM threshold"]]
      cells[[length(cells) + 1]] <- tags$td(
        style = "text-align: center; padding: 12px; vertical-align: top; border-right: 1px solid #dee2e6;",
        if (!is.null(tpm_value)) {
          tags$span(tpm_value, style = "font-size: 13px; font-weight: 500; color: #495057;")
        } else {
          tags$span("-", style = "color: #6c757d; font-style: italic;")
        }
      )
    }
    
    # Effect sizes subcolumn cells
    if (visible_columns$effect_sizes && length(effect_sizes_subcolumns) > 0) {
      for (param_name in names(effect_sizes_subcolumns)) {
        value <- row_data$effect_sizes[[param_name]]
        cells[[length(cells) + 1]] <- tags$td(
          style = "text-align: center; padding: 12px; vertical-align: top;",
          if (!is.null(value)) {
            tags$span(value, style = "font-size: 13px; font-weight: 500; color: #495057;")
          } else {
            tags$span("-", style = "color: #6c757d; font-style: italic;")
          }
        )
      }
    }
    
    # Return complete table row
    do.call(tags$tr, cells)
  })
}