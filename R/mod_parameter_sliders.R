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
      tags$div(
        class = "slider-row experimental-params",
        style = "margin-bottom: 20px;",
        tags$h5("Experimental Parameters", 
                style = "text-align: center; margin-bottom: 15px; color: #495057; font-size: 14px;"),
        fluidRow(
          column(4, create_compact_slider(ns("moi_slider"), "MOI", 1, 50, 10, 1)),
          column(4, create_compact_slider(ns("targets_slider"), "Number of Targets", 50, 50000, 100, 50)),
          column(4, create_compact_slider(ns("grnas_slider"), "gRNAs per Target", 1, 20, 4, 1))
        )
      ),
      
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
#'
#' @noRd 
mod_parameter_sliders_server <- function(id, param_manager, workflow_info){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    # ========================================================================
    # DYNAMIC ROW 2 GENERATION
    # ========================================================================
    
    # Generate dynamic Row 2 based on workflow
    output$dynamic_power_sliders <- renderUI({
      workflow <- workflow_info()
      
      if (is.null(workflow)) return(NULL)
      
      # Define all 4 power-determining parameters (ranges match sidebar exactly)
      all_power_params <- list(
        cells_per_target = list(id = "cells_slider", label = "Cells per Target", min = 20, max = 10000, value = 1000, step = 20),
        reads_per_cell = list(id = "reads_slider", label = "Reads per Cell", min = 1000, max = 500000, value = 5000, step = 1000),
        TPM_threshold = list(id = "TPM_slider", label = "TPM Threshold", min = 1, max = 500, value = 10, step = 1),
        minimum_fold_change = list(id = "fc_slider", label = "Fold Change", min = 0.3, max = 3, value = 0.8, step = 0.1)
      )
      
      # Determine which parameter is being minimized (exclude from Row 2)
      minimized_param <- get_minimized_parameter(workflow$workflow_id)
      
      # Filter out the minimized parameter
      visible_power_params <- all_power_params[!names(all_power_params) %in% minimized_param]
      
      # Create 3-column layout for the remaining 3 parameters
      fluidRow(
        lapply(names(visible_power_params), function(param_name) {
          param <- visible_power_params[[param_name]]
          column(4, create_compact_slider(
            ns(param$id), param$label, param$min, param$max, param$value, param$step
          ))
        })
      )
    })
    
    # ========================================================================
    # UNIFIED INPUT COLLECTION - Sliders → Central Manager
    # ========================================================================
    
    # Feed all slider changes directly to central parameter manager
    observe({
      if (!is.null(input$moi_slider)) {
        param_manager$update_parameter("MOI", input$moi_slider, "slider")
      }
    })
    
    observe({
      if (!is.null(input$targets_slider)) {
        param_manager$update_parameter("num_targets", input$targets_slider, "slider")
      }
    })
    
    observe({
      if (!is.null(input$grnas_slider)) {
        param_manager$update_parameter("gRNAs_per_target", input$grnas_slider, "slider")
      }
    })
    
    observe({
      if (!is.null(input$cells_slider)) {
        param_manager$update_parameter("cells_per_target", input$cells_slider, "slider")
      }
    })
    
    observe({
      if (!is.null(input$reads_slider)) {
        param_manager$update_parameter("reads_per_cell", input$reads_slider, "slider")
      }
    })
    
    observe({
      if (!is.null(input$TPM_slider)) {
        param_manager$update_parameter("TPM_threshold", input$TPM_slider, "slider")
      }
    })
    
    observe({
      if (!is.null(input$fc_slider)) {
        param_manager$update_parameter("minimum_fold_change", input$fc_slider, "slider")
      }
    })
    
    # ========================================================================
    # UI UPDATES - Central Manager → Sliders (Direct Reactive Observers)
    # ========================================================================
    
    # Direct reactive observers to update sliders when parameter manager changes
    observe({
      if (!identical(input$moi_slider, param_manager$parameters$MOI)) {
        updateSliderInput(session, "moi_slider", value = param_manager$parameters$MOI)
      }
    })
    
    observe({
      if (!identical(input$targets_slider, param_manager$parameters$num_targets)) {
        updateSliderInput(session, "targets_slider", value = param_manager$parameters$num_targets)
      }
    })
    
    observe({
      if (!identical(input$grnas_slider, param_manager$parameters$gRNAs_per_target)) {
        updateSliderInput(session, "grnas_slider", value = param_manager$parameters$gRNAs_per_target)
      }
    })
    
    observe({
      if (!identical(input$cells_slider, param_manager$parameters$cells_per_target)) {
        updateSliderInput(session, "cells_slider", value = param_manager$parameters$cells_per_target)
      }
    })
    
    observe({
      if (!identical(input$reads_slider, param_manager$parameters$reads_per_cell)) {
        updateSliderInput(session, "reads_slider", value = param_manager$parameters$reads_per_cell)
      }
    })
    
    observe({
      if (!identical(input$TPM_slider, param_manager$parameters$TPM_threshold)) {
        updateSliderInput(session, "TPM_slider", value = param_manager$parameters$TPM_threshold)
      }
    })
    
    observe({
      if (!identical(input$fc_slider, param_manager$parameters$minimum_fold_change)) {
        updateSliderInput(session, "fc_slider", value = param_manager$parameters$minimum_fold_change)
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
