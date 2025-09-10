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
#' visibility-triggered initialization. Sliders initialize from sidebar when results
#' become available, then maintain independence.
#'
#' @param id Module namespace ID
#' @param sidebar_config Reactive containing complete sidebar configuration with workflow_info
#' @param app_state Global app state reactiveValues for phase management
#'
#' @noRd 
mod_parameter_sliders_server <- function(id, sidebar_config, app_state){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    # ========================================================================
    # SLIDER INDEPENDENCE STATE MANAGEMENT
    # ========================================================================
    
    # Independent slider state - initialized from sidebar when results available
    slider_state <- reactiveValues(
      initialized = FALSE,
      MOI = NULL,
      num_targets = NULL,
      gRNAs_per_target = NULL,
      cells_fixed = NULL,
      mapped_reads_fixed = NULL,
      TPM_threshold_fixed = NULL,
      minimum_fold_change_fixed = NULL
    )
    
    # Initialize sliders exactly when sidebar becomes frozen
    observe({
      # Trigger: sidebar_frozen is TRUE AND not yet initialized
      if (app_state$sidebar_frozen && !slider_state$initialized) {
        # Get current sidebar config at the moment of freezing
        config <- sidebar_config()
        
        if (!is.null(config)) {
          # Initialize sliders with sidebar values at freeze moment
          experimental <- config$experimental_setup %||% list()
          analysis <- config$analysis_choices %||% list()
          effects <- config$effect_sizes %||% list()
          
          slider_state$MOI <- experimental$MOI %||% 10
          slider_state$num_targets <- experimental$num_targets %||% 100
          slider_state$gRNAs_per_target <- experimental$gRNAs_per_target %||% 4
          slider_state$cells_fixed <- experimental$cells_fixed %||% 1000
          slider_state$mapped_reads_fixed <- experimental$mapped_reads_fixed %||% 5000
          slider_state$TPM_threshold_fixed <- analysis$TPM_threshold_fixed %||% 10
          slider_state$minimum_fold_change_fixed <- effects$minimum_fold_change_fixed %||% 0.8
          
          slider_state$initialized <- TRUE
          
        }
      }
    })
    
    # ========================================================================
    # DYNAMIC SLIDER GENERATION
    # ========================================================================
    
    # Generate dynamic Row 1 experimental sliders with current values
    output$experimental_sliders <- renderUI({
      config <- sidebar_config()
      if (is.null(config)) return(NULL)
      
      # Use slider_state if initialized, otherwise use sidebar values
      if (slider_state$initialized) {
        moi_value <- slider_state$MOI
        targets_value <- slider_state$num_targets  
        grnas_value <- slider_state$gRNAs_per_target
      } else {
        # Extract values from sidebar configuration with defaults
        experimental <- config$experimental_setup %||% list()
        moi_value <- experimental$MOI %||% 10
        targets_value <- experimental$num_targets %||% 100  
        grnas_value <- experimental$gRNAs_per_target %||% 4
      }
      
      tags$div(
        class = "slider-row experimental-params",
        style = "margin-bottom: 20px;",
        fluidRow(
          column(4, create_compact_slider(ns("moi_slider"), "MOI", 1, 50, moi_value, 1)),
          column(4, create_compact_slider(ns("targets_slider"), "Number of Targets", 50, 20000, targets_value, 50)),
          column(4, create_compact_slider(ns("grnas_slider"), "gRNAs per Target", 1, 20, grnas_value, 1))
        )
      )
    })
    
    # Generate dynamic Row 2 based on workflow
    output$dynamic_power_sliders <- renderUI({
      config <- sidebar_config()
      if (is.null(config)) return(NULL)
      
      # Use pre-computed workflow_info from sidebar (Option B)
      workflow <- config$workflow_info
      if (is.null(workflow)) return(NULL)
      
      # Get design configuration from sidebar_config
      design_config <- config$design_options
      param_controls <- design_config$parameter_controls %||% list()
      optimization_type <- design_config$optimization_type
      
      # Extract power parameter values from sidebar configuration
      experimental <- config$experimental_setup %||% list()
      analysis <- config$analysis_choices %||% list()
      effects <- config$effect_sizes %||% list()
      
      # Get parameter values with defaults (use slider_state if initialized)
      if (slider_state$initialized) {
        cells_value <- slider_state$cells_fixed
        reads_value <- slider_state$mapped_reads_fixed
        tpm_value <- slider_state$TPM_threshold_fixed
        fc_value <- slider_state$minimum_fold_change_fixed
      } else {
        cells_value <- experimental$cells_fixed %||% 1000
        reads_value <- experimental$mapped_reads_fixed %||% 5000
        tpm_value <- analysis$TPM_threshold_fixed %||% 10
        fc_value <- effects$minimum_fold_change_fixed %||% 0.8
      }
      
      # Define all 4 power-determining parameters (use sidebar values)
      all_power_params <- list(
        cells_per_target = list(id = "cells_slider", label = "Cells per Target", min = 20, max = 10000, value = cells_value, step = 20),
        reads_per_cell = list(id = "reads_slider", label = "Reads per Cell", min = 1000, max = 500000, value = reads_value, step = 1000),
        TPM_threshold = list(id = "TPM_slider", label = "TPM Threshold", min = 1, max = 500, value = tpm_value, step = 1),
        minimum_fold_change = list(id = "fc_slider", label = "Fold Change", min = 0.3, max = 2, value = fc_value, step = 0.1)
      )
      
      # Determine which parameter is being minimized (exclude from Row 2)
      minimized_param <- get_minimized_parameter(workflow$workflow_id)
      
      # Filter out the minimized parameter
      visible_power_params <- all_power_params[!names(all_power_params) %in% minimized_param]
      
      # POWER+COST MODE FILTERING OR COST MINIMIZATION: Show sliders for "fixed" parameters
      if ((!is.null(optimization_type) && optimization_type == "power_cost" && !is.null(param_controls)) ||
          (!is.null(minimized_param) && minimized_param == "cost")) {
        
        # Map parameter names to their control types
        param_name_mapping <- list(
          "cells_per_target" = "cells_per_target",
          "reads_per_cell" = "mapped_reads_per_cell",  # UI uses different name
          "TPM_threshold" = "TPM_threshold",
          "minimum_fold_change" = "minimum_fold_change"
        )
        
        # For cost minimization, show TPM and FC sliders (both are fixed)
        if (!is.null(minimized_param) && minimized_param == "cost") {
          # Cost minimization: show only TPM and FC sliders (they are fixed)
          cost_min_params <- list()
          if ("TPM_threshold" %in% names(visible_power_params)) {
            cost_min_params[["TPM_threshold"]] <- visible_power_params[["TPM_threshold"]]
          }
          if ("minimum_fold_change" %in% names(visible_power_params)) {
            cost_min_params[["minimum_fold_change"]] <- visible_power_params[["minimum_fold_change"]]
          }
          visible_power_params <- cost_min_params
        } else {
          # Power+cost mode: Filter to show only parameters that are set to "fixed"
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
    # INPUT COLLECTION - Slider Independence
    # ========================================================================
    # Collect slider inputs and update slider_state (only when initialized)
    
    observeEvent(input$moi_slider, {
      if (slider_state$initialized) {
        slider_state$MOI <- input$moi_slider
      }
    })
    
    observeEvent(input$targets_slider, {
      if (slider_state$initialized) {
        slider_state$num_targets <- input$targets_slider
      }
    })
    
    observeEvent(input$grnas_slider, {
      if (slider_state$initialized) {
        slider_state$gRNAs_per_target <- input$grnas_slider
      }
    })
    
    observeEvent(input$cells_slider, {
      if (slider_state$initialized) {
        slider_state$cells_fixed <- input$cells_slider
      }
    })
    
    observeEvent(input$reads_slider, {
      if (slider_state$initialized) {
        slider_state$mapped_reads_fixed <- input$reads_slider
      }
    })
    
    observeEvent(input$TPM_slider, {
      if (slider_state$initialized) {
        slider_state$TPM_threshold_fixed <- input$TPM_slider
      }
    })
    
    observeEvent(input$fc_slider, {
      if (slider_state$initialized) {
        slider_state$minimum_fold_change_fixed <- input$fc_slider
      }
    })
    
    # ========================================================================
    # UI UPDATES - REMOVED to prevent sync between sliders and sidebar
    # ========================================================================
    
    # UI update observers removed to make sliders independent from sidebar
    # Sliders will not automatically update when parameter manager changes
    # This prevents sync between sidebar inputs and slider values
    
    # ========================================================================
    # RETURN SLIDER CONFIGURATION
    # ========================================================================
    # Return reactive containing slider parameter overrides for param_source_manager
    
    slider_config <- reactive({
      if (!slider_state$initialized) {
        return(NULL)  # No overrides until sliders are initialized
      }
      
      list(
        experimental_setup = list(
          MOI = slider_state$MOI,
          num_targets = slider_state$num_targets,
          gRNAs_per_target = slider_state$gRNAs_per_target,
          cells_fixed = slider_state$cells_fixed,
          mapped_reads_fixed = slider_state$mapped_reads_fixed
        ),
        analysis_choices = list(
          TPM_threshold_fixed = slider_state$TPM_threshold_fixed
        ),
        effect_sizes = list(
          minimum_fold_change_fixed = slider_state$minimum_fold_change_fixed
        ),
        slider_active = TRUE  # Flag to indicate sliders are providing overrides
      )
    })
    
    return(slider_config)
    
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
    
    # Cost minimization workflow
    "power_cost_minimization" = "cost",
    
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
