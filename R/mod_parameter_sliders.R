#' parameter_sliders UI Function
#'
#' @description Smart horizontal parameter slider module with 2-row layout:
#' Row 1: 3 experimental parameters (MOI, targets, gRNAs)
#' Row 2: 3 power-determining parameters (dynamic based on workflow)
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList tags div h5 fluidRow column uiOutput sliderInput moduleServer reactive observeEvent req renderUI observe isolate updateSliderInput reactiveValues
mod_parameter_sliders_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    tags$div(
      id = ns("slider_container"),
      class = "parameter-sliders-horizontal",
      style = "padding: 15px; background-color: #f8f9fa; border-radius: 8px; margin-top: 20px;",
      
      # ROW 1: Fixed experimental parameters (always 3 sliders)
      uiOutput(ns("experimental_sliders")),
      
      # ROW 2: Power-determining parameters (dynamic - 3 out of 4 shown)
      tags$div(
        class = "slider-row power-params",
        tags$h5("Power-Determining Parameters", 
                style = "text-align: center; margin-bottom: 15px; color: #495057; font-size: 14px;"),
        uiOutput(ns("dynamic_power_sliders"))
      )
    )
  )
}

#' Create Compact Slider Helper
#'
#' @description Creates a compact slider with label and value display
#' for horizontal layout
#'
#' @param inputId Slider input ID
#' @param label Parameter label 
#' @param min Minimum value
#' @param max Maximum value
#' @param value Default value
#' @param step Step size
#'
#' @noRd
create_compact_slider <- function(inputId, label, min, max, value, step) {
  tags$div(
    class = "compact-slider-container",
    tags$label(
      label, 
      class = "slider-label",
      style = "font-size: 13px; font-weight: 500; color: #495057; margin-bottom: 8px; display: block; text-align: center;"
    ),
    sliderInput(
      inputId = inputId,
      label = NULL,
      min = min,
      max = max, 
      value = value,
      step = step,
      width = "100%"
    )
  )
}

    
#' parameter_sliders Server Functions
#'
#' @description Server logic for smart horizontal parameter sliders with
#' dynamic Row 2 generation based on workflow type. Now uses central parameter manager.
#'
#' @param id Module namespace ID
#' @param param_manager Parameter manager instance (central hub)
#' @param workflow_info Reactive containing workflow information
#' @param user_config Reactive containing full user configuration (optional, for power+cost filtering)
#'
#' @noRd 
mod_parameter_sliders_server <- function(id, param_manager, workflow_info, user_config = reactive(NULL)){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    # ========================================================================
    # DYNAMIC SLIDER GENERATION
    # ========================================================================
    
    # Generate dynamic Row 1 experimental sliders with current values
    output$experimental_sliders <- renderUI({
      tags$div(
        class = "slider-row experimental-params",
        style = "margin-bottom: 20px;",
        tags$h5("Experimental Parameters", 
                style = "text-align: center; margin-bottom: 15px; color: #495057; font-size: 14px;"),
        fluidRow(
          column(4, create_compact_slider(ns("moi_slider"), "MOI", 1, 50, param_manager$parameters$MOI, 1)),
          column(4, create_compact_slider(ns("targets_slider"), "Number of Targets", 50, 20000, param_manager$parameters$num_targets, 50)),
          column(4, create_compact_slider(ns("grnas_slider"), "gRNAs per Target", 1, 20, param_manager$parameters$gRNAs_per_target, 1))
        )
      )
    })
    
    # Generate dynamic Row 2 based on workflow
    output$dynamic_power_sliders <- renderUI({
      workflow <- workflow_info()
      
      if (is.null(workflow)) return(NULL)
      
      # Get design configuration from user_config to check parameter control types
      config <- user_config()
      if (is.null(config) || is.null(config$design_options)) {
        # Fallback to basic workflow-based filtering if config not available
        design_config <- NULL
        param_controls <- NULL
        optimization_type <- NULL
      } else {
        design_config <- config$design_options
        param_controls <- design_config$parameter_controls
        optimization_type <- design_config$optimization_type
      }
      
      # Define all 4 power-determining parameters (use current parameter manager values)
      all_power_params <- list(
        cells_per_target = list(id = "cells_slider", label = "Cells per Target", min = 20, max = 10000, value = param_manager$parameters$cells_per_target, step = 20),
        reads_per_cell = list(id = "reads_slider", label = "Reads per Cell", min = 1000, max = 500000, value = param_manager$parameters$reads_per_cell, step = 1000),
        TPM_threshold = list(id = "TPM_slider", label = "TPM Threshold", min = 1, max = 500, value = param_manager$parameters$TPM_threshold, step = 1),
        minimum_fold_change = list(id = "fc_slider", label = "Fold Change", min = 0.3, max = 2, value = param_manager$parameters$minimum_fold_change, step = 0.1)
      )
      
      # Determine which parameter is being minimized (exclude from Row 2)
      minimized_param <- get_minimized_parameter(workflow$workflow_id)
      
      # Filter out the minimized parameter
      visible_power_params <- all_power_params[!names(all_power_params) %in% minimized_param]
      
      # POWER+COST MODE FILTERING: Show sliders only for "fixed" parameters
      if (!is.null(optimization_type) && optimization_type == "power_cost" && !is.null(param_controls)) {
        # Map parameter names to their control types
        param_name_mapping <- list(
          "cells_per_target" = "cells_per_target",
          "reads_per_cell" = "mapped_reads_per_cell",  # UI uses different name
          "TPM_threshold" = "TPM_threshold",
          "minimum_fold_change" = "minimum_fold_change"
        )
        
        # Filter to show only parameters that are set to "fixed"
        filtered_params <- list()
        for (param_name in names(visible_power_params)) {
          control_name <- param_name_mapping[[param_name]]
          if (!is.null(control_name) && !is.null(param_controls[[control_name]])) {
            param_type <- param_controls[[control_name]]$type
            # Show slider only if parameter is set to "fixed"
            if (!is.null(param_type) && param_type == "fixed") {
              filtered_params[[param_name]] <- visible_power_params[[param_name]]
            }
          }
        }
        visible_power_params <- filtered_params
      }
      
      # If no parameters to show, return empty
      if (length(visible_power_params) == 0) {
        return(NULL)
      }
      
      # Create dynamic column layout based on number of visible parameters
      num_params <- length(visible_power_params)
      col_width <- if (num_params == 1) 12 else if (num_params == 2) 6 else 4
      
      fluidRow(
        lapply(names(visible_power_params), function(param_name) {
          param <- visible_power_params[[param_name]]
          column(col_width, create_compact_slider(
            ns(param$id), param$label, param$min, param$max, param$value, param$step
          ))
        })
      )
    })
    
    # ========================================================================
    # INPUT COLLECTION - SAFE: Using isolate() to break reactive cycles
    # ========================================================================
    
    # Use observeEvent + isolate to prevent circular reactive dependencies
    observeEvent(input$moi_slider, {
      isolate({
        param_manager$update_parameter("MOI", input$moi_slider, "slider")
      })
    })
    
    observeEvent(input$targets_slider, {
      isolate({
        param_manager$update_parameter("num_targets", input$targets_slider, "slider")
      })
    })
    
    observeEvent(input$grnas_slider, {
      isolate({
        param_manager$update_parameter("gRNAs_per_target", input$grnas_slider, "slider")
      })
    })
    
    observeEvent(input$cells_slider, {
      isolate({
        param_manager$update_parameter("cells_per_target", input$cells_slider, "slider")
      })
    })
    
    observeEvent(input$reads_slider, {
      isolate({
        param_manager$update_parameter("reads_per_cell", input$reads_slider, "slider")
      })
    })
    
    observeEvent(input$TPM_slider, {
      isolate({
        param_manager$update_parameter("TPM_threshold", input$TPM_slider, "slider")
      })
    })
    
    observeEvent(input$fc_slider, {
      isolate({
        param_manager$update_parameter("minimum_fold_change", input$fc_slider, "slider")
      })
    })
    
    # ========================================================================
    # UI UPDATES - SAFE: Using observeEvent + isolate for controlled updates
    # ========================================================================
    
    # Safe UI updates: Only update when parameter manager changes AND value is different
    observeEvent(param_manager$parameters$MOI, {
      new_value <- param_manager$parameters$MOI
      if (!identical(isolate(input$moi_slider), new_value)) {
        updateSliderInput(session, "moi_slider", value = new_value)
      }
    })
    
    observeEvent(param_manager$parameters$num_targets, {
      new_value <- param_manager$parameters$num_targets
      if (!identical(isolate(input$targets_slider), new_value)) {
        updateSliderInput(session, "targets_slider", value = new_value)
      }
    })
    
    observeEvent(param_manager$parameters$gRNAs_per_target, {
      new_value <- param_manager$parameters$gRNAs_per_target
      if (!identical(isolate(input$grnas_slider), new_value)) {
        updateSliderInput(session, "grnas_slider", value = new_value)
      }
    })
    
    observeEvent(param_manager$parameters$cells_per_target, {
      new_value <- param_manager$parameters$cells_per_target
      if (!identical(isolate(input$cells_slider), new_value)) {
        updateSliderInput(session, "cells_slider", value = new_value)
      }
    })
    
    observeEvent(param_manager$parameters$reads_per_cell, {
      new_value <- param_manager$parameters$reads_per_cell
      if (!identical(isolate(input$reads_slider), new_value)) {
        updateSliderInput(session, "reads_slider", value = new_value)
      }
    })
    
    observeEvent(param_manager$parameters$TPM_threshold, {
      new_value <- param_manager$parameters$TPM_threshold
      if (!identical(isolate(input$TPM_slider), new_value)) {
        updateSliderInput(session, "TPM_slider", value = new_value)
      }
    })
    
    observeEvent(param_manager$parameters$minimum_fold_change, {
      new_value <- param_manager$parameters$minimum_fold_change
      if (!identical(isolate(input$fc_slider), new_value)) {
        updateSliderInput(session, "fc_slider", value = new_value)
      }
    })
    
    # No return needed - central manager handles all data flow
    
  })
}

# Helper function to identify minimized parameter by workflow
get_minimized_parameter <- function(workflow_id) {
  minimized_map <- list(
    # Power-only workflows
    "power_single_cells_per_target" = "cells_per_target",
    "power_single_reads_per_cell" = "reads_per_cell", 
    "power_single_TPM_threshold" = "TPM_threshold",
    "power_single_minimum_fold_change" = "minimum_fold_change",
    
    # Power+cost workflows
    "power_cost_TPM_cells" = "TPM_threshold",      # TPM being minimized
    "power_cost_TPM_reads" = "TPM_threshold",      # TPM being minimized
    "power_cost_fc_cells" = "minimum_fold_change", # FC being minimized
    "power_cost_fc_reads" = "minimum_fold_change"  # FC being minimized
  )
  
  return(minimized_map[[workflow_id]] %||% character(0))
}
    
## To be copied in the UI
# mod_parameter_sliders_ui("parameter_sliders_1")
    
## To be copied in the server
# mod_parameter_sliders_server("parameter_sliders_1")
