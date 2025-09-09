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
#' @importFrom shiny NS tagList tags div h5 fluidRow column uiOutput sliderInput moduleServer reactive observeEvent req renderUI observe isolate updateSliderInput reactiveValues debounce showNotification
#' @importFrom magrittr %>%
mod_parameter_sliders_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    tags$div(
      id = ns("slider_container"),
      class = "parameter-sliders-grid",
      style = "padding: 10px; background-color: #f8f9fa; border-radius: 8px;",
      
      # 2-column, 3-row grid layout for all 6 parameters
      fluidRow(
        # Left Column (3 rows)
        column(
          width = 6,
          style = "padding-right: 10px;",
          uiOutput(ns("left_column_sliders"))
        ),
        
        # Right Column (3 rows)
        column(
          width = 6,
          style = "padding-left: 10px;",
          uiOutput(ns("right_column_sliders"))
        )
      ),
      
      # Pin button section (conditionally rendered based on workflow)
      uiOutput(ns("pin_buttons_section"))
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
#' @param plan_state ReactiveValues containing plan click state for real-time analysis
#'
#' @noRd 
mod_parameter_sliders_server <- function(id, param_manager, workflow_info, user_config = reactive(NULL), plan_state = NULL){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    # ========================================================================
    # DYNAMIC SLIDER GENERATION
    # ========================================================================
    
    # Generate left column sliders (3 parameters from Row 1)
    output$left_column_sliders <- renderUI({
      # PHASE 2: Hide sliders until first plan click
      if (!is.null(plan_state) && !plan_state$sliders_visible) {
        return(NULL)
      }
      
      tagList(
        tags$div(
          style = "margin-bottom: 15px;",
          create_compact_slider(ns("moi_slider"), "MOI", 1, 30, param_manager$parameters$MOI, 1)
        ),
        tags$div(
          style = "margin-bottom: 15px;",
          create_compact_slider(ns("targets_slider"), "Number of Targets", 50, 12000, param_manager$parameters$num_targets, 50)
        ),
        tags$div(
          style = "margin-bottom: 15px;",
          create_compact_slider(ns("grnas_slider"), "gRNAs per Target", 1, 20, param_manager$parameters$gRNAs_per_target, 1)
        )
      )
    })
    
    # Generate right column sliders (3 parameters from Row 2 - dynamic based on workflow)
    output$right_column_sliders <- renderUI({
      workflow <- workflow_info()
      
      # PHASE 2: Hide sliders until first plan click
      if (is.null(workflow) || (!is.null(plan_state) && !plan_state$sliders_visible)) {
        return(NULL)
      }
      
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
      
      # Get side parameter from user config to adjust fold change range
      config <- user_config()
      side <- if (!is.null(config) && !is.null(config$analysis_choices)) config$analysis_choices$side else "left"
      
      # Dynamic fold change range based on side parameter
      fc_min <- if (!is.null(side) && side == "right") 1.0 else 0.3
      fc_max <- if (!is.null(side) && side == "right") 2.0 else 1.0
      fc_value <- param_manager$parameters$minimum_fold_change
      
      # Ensure current value is within new range
      if (!is.null(fc_value)) {
        fc_value <- max(fc_min, min(fc_max, fc_value))
      }
      
      # Define all power-determining parameters + cost budget (use current parameter manager values)
      all_power_params <- list(
        cells_per_target = list(id = "cells_slider", label = "Cells per Target", min = 20, max = 2500, value = param_manager$parameters$cells_per_target, step = 20),
        reads_per_cell = list(id = "reads_slider", label = "Reads per Cell", min = 1000, max = 100000, value = param_manager$parameters$reads_per_cell, step = 1000),
        TPM_threshold = list(id = "TPM_slider", label = "TPM Threshold", min = 1, max = 200, value = param_manager$parameters$TPM_threshold, step = 1),
        minimum_fold_change = list(id = "fc_slider", label = "Fold Change", min = fc_min, max = fc_max, value = fc_value, step = 0.02),
        cost_budget = list(id = "cost_budget_slider", label = "Cost Budget ($)", min = 100, max = 100000, value = param_manager$parameters$cost_budget, step = 500)
      )
      
      # Determine which parameter is being minimized (exclude from Row 2)
      minimized_param <- get_minimized_parameter(workflow$workflow_id)
      
      # Filter out the minimized parameter
      visible_power_params <- all_power_params[!names(all_power_params) %in% minimized_param]
      
      # Filter out cost_budget unless in power+cost mode or cost minimization workflow
      show_cost_budget <- (!is.null(optimization_type) && optimization_type == "power_cost") ||
                          (!is.null(minimized_param) && minimized_param == "cost")
      
      if (!show_cost_budget) {
        visible_power_params <- visible_power_params[names(visible_power_params) != "cost_budget"]
      }
      
      # POWER+COST MODE FILTERING OR COST MINIMIZATION: Show sliders for "fixed" parameters
      if ((!is.null(optimization_type) && optimization_type == "power_cost" && !is.null(param_controls)) ||
          (!is.null(minimized_param) && minimized_param == "cost")) {
        
        # Map parameter names to their control types
        param_name_mapping <- list(
          "cells_per_target" = "cells_per_target",
          "reads_per_cell" = "sequenced_reads_per_cell",  # UI uses different name
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
          # Power+cost mode: Filter to show only parameters that are set to "fixed" + cost budget
          filtered_params <- list()
          for (param_name in names(visible_power_params)) {
            # Cost budget always shows in power+cost mode
            if (param_name == "cost_budget") {
              filtered_params[[param_name]] <- visible_power_params[[param_name]]
            } else {
              # Other parameters: show only if set to "fixed"
              control_name <- param_name_mapping[[param_name]]
              if (!is.null(control_name) && !is.null(param_controls[[control_name]])) {
                param_type <- param_controls[[control_name]]$type
                # Show slider only if parameter is set to "fixed"
                if (!is.null(param_type) && param_type == "fixed") {
                  filtered_params[[param_name]] <- visible_power_params[[param_name]]
                }
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
      
      # Create vertical layout for right column (stacked parameters)
      tagList(
        lapply(names(visible_power_params), function(param_name) {
          param <- visible_power_params[[param_name]]
          tags$div(
            style = "margin-bottom: 15px;",
            create_compact_slider(
              ns(param$id), param$label, param$min, param$max, param$value, param$step
            )
          )
        })
      )
    })
    
    # Generate Pin buttons section conditionally for single parameter workflows only
    output$pin_buttons_section <- renderUI({
      workflow <- workflow_info()
      
      # Hide pinning buttons until first plan click (same as sliders)
      if (is.null(workflow) || is.null(workflow$workflow_id) || 
          (!is.null(plan_state) && !plan_state$sliders_visible)) {
        return(NULL)
      }
      
      # Show Pin buttons for single parameter optimization workflows + cost minimization + constrained minimization workflows
      pinning_enabled_workflows <- c(
        "power_single_cells_per_target",
        "power_single_reads_per_cell", 
        "power_single_TPM_threshold",
        "power_single_minimum_fold_change",
        # Cost minimization workflow (5)
        "power_cost_minimization",
        # Power+cost TPM/FC minimization workflows (6-9)
        "power_cost_TPM_cells",
        "power_cost_TPM_reads",
        "power_cost_fc_cells",
        "power_cost_fc_reads",
        # Constrained minimization workflows (10-11)
        "power_cost_TPM_cells_reads",
        "power_cost_fc_cells_reads"
      )
      
      if (workflow$workflow_id %in% pinning_enabled_workflows) {
        tags$div(
          style = "padding: 15px 10px 10px 10px; text-align: center; border-top: 1px solid #dee2e6; margin-top: 10px; display: flex; gap: 10px; justify-content: center;",
          actionButton(
            ns("pin_solution"),
            "Pin Solution",
            class = "btn btn-success btn-sm",
            style = "flex: 1; max-width: 140px; font-size: 16px; font-weight: 500;"
          ),
          actionButton(
            ns("clear_pins"),
            "Clear All",
            class = "btn btn-outline-secondary btn-sm",
            style = "flex: 1; max-width: 90px; font-size: 16px; font-weight: 500;"
          )
        )
      } else {
        return(NULL)
      }
    })
    
    # ========================================================================
    # INPUT COLLECTION - SAFE: Using isolate() to break reactive cycles
    # ========================================================================
    
    # Helper function to enable real-time mode on first slider interaction
    enable_real_time_if_needed <- function(source = "unknown") {
      # Don't enable if Plan analysis hasn't completed yet
      if (is.null(plan_state$last_analysis_completed)) {
        return()
      }
      
      # Don't enable if this is too soon after analysis completion (within 2 seconds)
      time_since_analysis <- difftime(Sys.time(), plan_state$last_analysis_completed, units = "secs")
      if (time_since_analysis < 2) {
        return()
      }
      
      if (!is.null(plan_state) && plan_state$sliders_visible && !plan_state$real_time_enabled) {
        plan_state$real_time_enabled <- TRUE
        showNotification(
          "Real-time mode activated! Changes will update instantly.", 
          duration = 2, 
          type = "message"
        )
      }
    }
    
    # Use observeEvent + isolate to prevent circular reactive dependencies
    # ignoreInit = TRUE prevents firing when slider is first rendered
    observeEvent(input$moi_slider, {
      isolate({
        enable_real_time_if_needed("moi_slider")  # Enable real-time on first slider change
        param_manager$update_parameter("MOI", input$moi_slider, "slider")
      })
    }, ignoreInit = TRUE)
    
    observeEvent(input$targets_slider, {
      isolate({
        enable_real_time_if_needed()
        param_manager$update_parameter("num_targets", input$targets_slider, "slider")
      })
    }, ignoreInit = TRUE)
    
    observeEvent(input$grnas_slider, {
      isolate({
        enable_real_time_if_needed()
        param_manager$update_parameter("gRNAs_per_target", input$grnas_slider, "slider")
      })
    }, ignoreInit = TRUE)
    
    observeEvent(input$cells_slider, {
      isolate({
        enable_real_time_if_needed()
        param_manager$update_parameter("cells_per_target", input$cells_slider, "slider")
      })
    }, ignoreInit = TRUE)
    
    observeEvent(input$reads_slider, {
      isolate({
        enable_real_time_if_needed()
        param_manager$update_parameter("reads_per_cell", input$reads_slider, "slider")
      })
    }, ignoreInit = TRUE)
    
    observeEvent(input$TPM_slider, {
      isolate({
        enable_real_time_if_needed()
        param_manager$update_parameter("TPM_threshold", input$TPM_slider, "slider")
      })
    }, ignoreInit = TRUE)
    
    observeEvent(input$fc_slider, {
      isolate({
        enable_real_time_if_needed()
        param_manager$update_parameter("minimum_fold_change", input$fc_slider, "slider")
      })
    }, ignoreInit = TRUE)
    
    observeEvent(input$cost_budget_slider, {
      isolate({
        enable_real_time_if_needed()
        param_manager$update_parameter("cost_budget", input$cost_budget_slider, "slider")
      })
    }, ignoreInit = TRUE)
    
    # ========================================================================
    # REAL-TIME ANALYSIS TRIGGERS - Phase 3
    # ========================================================================
    
    # Debounced slider change detection for real-time analysis
    slider_changes <- reactive({
      if (is.null(plan_state) || !plan_state$real_time_enabled) return(NULL)
      
      # Collect all slider values
      list(
        MOI = input$moi_slider,
        num_targets = input$targets_slider,
        gRNAs_per_target = input$grnas_slider,
        cells_per_target = input$cells_slider,
        reads_per_cell = input$reads_slider,
        TPM_threshold = input$TPM_slider,
        minimum_fold_change = input$fc_slider,
        cost_budget = input$cost_budget_slider
      )
    }) %>% debounce(500)  # 500ms delay to prevent excessive calls
    
    # Trigger real-time analysis on debounced slider changes
    observeEvent(slider_changes(), {
      if (!is.null(plan_state) && plan_state$real_time_enabled && !is.null(param_manager$trigger_real_time_analysis)) {
        # Show subtle loading notification (shorter duration to reduce noise)
        
        # Trigger analysis through parameter manager
        param_manager$trigger_real_time_analysis()
      }
    })
    
    # ========================================================================
    # UI UPDATES - SAFE: Using observeEvent + isolate for controlled updates
    # ========================================================================
    
    # PHASE 2: SLIDER-TO-SLIDER SYNC REMOVED
    # Eliminated 8 updateSliderInput observers to prevent circular dependencies
    # Sliders now work independently - moving one slider doesn't update others
    # This preserves the direct path: Slider → Parameter Manager → Analysis Engine
    
    # ========================================================================
    # PIN BUTTON FUNCTIONALITY - Return values for module communication
    # ========================================================================
    
    # Return button actions and current parameters for parent module
    return(list(
      pin_requested = reactive(input$pin_solution),
      clear_requested = reactive(input$clear_pins),
      current_parameters = reactive({
        list(
          MOI = input$moi_slider,
          num_targets = input$targets_slider,
          gRNAs_per_target = input$grnas_slider,
          cells_per_target = input$cells_slider,
          reads_per_cell = input$reads_slider,
          TPM_threshold = input$TPM_slider,
          minimum_fold_change = input$fc_slider,
          cost_budget = input$cost_budget_slider
        )
      })
    ))
    
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
