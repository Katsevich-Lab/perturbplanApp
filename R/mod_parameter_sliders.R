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
#' dynamic Row 2 generation based on workflow type
#'
#' @param id Module namespace ID
#' @param sidebar_config Reactive containing sidebar configuration
#' @param workflow_info Reactive containing workflow information
#'
#' @noRd 
mod_parameter_sliders_server <- function(id, sidebar_config, workflow_info){
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
    # BIDIRECTIONAL SYNCHRONIZATION
    # ========================================================================
    
    # Create reactive values for parameter updates and user interaction tracking
    parameter_updates <- reactiveValues()
    user_interaction <- reactiveValues(
      active = FALSE,
      last_change = Sys.time()
    )
    
    # Sidebar → Sliders: Update sliders when sidebar changes (COMPLETELY DISABLED during user interaction)
    # Use invalidateLater to check periodically instead of direct reactive dependency
    observe({
      invalidateLater(500, session)  # Check every 500ms (slower to reduce conflicts)
      
      # Check if user is actively interacting - if so, COMPLETELY SKIP ALL UPDATES
      current_time <- Sys.time()
      if (user_interaction$active && 
          difftime(current_time, user_interaction$last_change, units = "secs") < 5) {
        return() # Skip ALL sidebar updates during active user interaction (5 second window)
      }
      
      # Access sidebar_config() only when user is not interacting
      sidebar <- isolate(sidebar_config())
      
      if (is.null(sidebar)) return()
      
      # Extract parameter values from sidebar configuration
      # Handle the complex nested structure of sidebar config  
      isolate({
        # MOI from experimental setup (only update if different to prevent snap-back)
        if (!is.null(sidebar$experimental_setup) && !is.null(sidebar$experimental_setup$MOI)) {
          if (is.null(input$moi_slider) || input$moi_slider != sidebar$experimental_setup$MOI) {
            updateSliderInput(session, "moi_slider", value = sidebar$experimental_setup$MOI)
          }
        }
        
        # Number of targets from experimental setup (only update if different)
        if (!is.null(sidebar$experimental_setup) && !is.null(sidebar$experimental_setup$num_targets)) {
          if (is.null(input$targets_slider) || input$targets_slider != sidebar$experimental_setup$num_targets) {
            updateSliderInput(session, "targets_slider", value = sidebar$experimental_setup$num_targets)
          }
        }
        
        # gRNAs per target from experimental setup (only update if different)
        if (!is.null(sidebar$experimental_setup) && !is.null(sidebar$experimental_setup$gRNAs_per_target)) {
          if (is.null(input$grnas_slider) || input$grnas_slider != sidebar$experimental_setup$gRNAs_per_target) {
            updateSliderInput(session, "grnas_slider", value = sidebar$experimental_setup$gRNAs_per_target)
          }
        }
        
        # Power-determining parameters - read from design_options parameter_controls
        if (!is.null(sidebar$design_options) && !is.null(sidebar$design_options$parameter_controls)) {
          
          # Cells per target from parameter controls
          cells_value <- sidebar$design_options$parameter_controls$cells_per_target$fixed_value %||% 1000
          if (is.null(input$cells_slider) || input$cells_slider != cells_value) {
            updateSliderInput(session, "cells_slider", value = cells_value)
          }
          
          # Reads per cell from parameter controls  
          reads_value <- sidebar$design_options$parameter_controls$mapped_reads_per_cell$fixed_value %||% 5000
          if (is.null(input$reads_slider) || input$reads_slider != reads_value) {
            updateSliderInput(session, "reads_slider", value = reads_value)
          }
          
          # TPM threshold from parameter controls
          TPM_value <- sidebar$design_options$parameter_controls$TPM_threshold$fixed_value %||% 10
          if (is.null(input$TPM_slider) || input$TPM_slider != TPM_value) {
            updateSliderInput(session, "TPM_slider", value = TPM_value)
          }
          
          # Fold change from parameter controls
          fc_value <- sidebar$design_options$parameter_controls$minimum_fold_change$fixed_value %||% 0.8
          if (is.null(input$fc_slider) || input$fc_slider != fc_value) {
            updateSliderInput(session, "fc_slider", value = fc_value)
          }
        }
      })
    })
    
    # Sliders → Parameter Updates: Track slider changes for return to main app + user interaction
    observe({
      if (!is.null(input$moi_slider)) {
        user_interaction$active <- TRUE
        user_interaction$last_change <- Sys.time()
        parameter_updates$moi <- input$moi_slider
      }
    })
    observe({
      if (!is.null(input$targets_slider)) {
        user_interaction$active <- TRUE
        user_interaction$last_change <- Sys.time()
        parameter_updates$num_targets <- input$targets_slider  
      }
    })
    observe({
      if (!is.null(input$grnas_slider)) {
        user_interaction$active <- TRUE
        user_interaction$last_change <- Sys.time()
        parameter_updates$gRNAs_per_target <- input$grnas_slider
      }
    })
    observe({
      if (!is.null(input$cells_slider)) {
        user_interaction$active <- TRUE
        user_interaction$last_change <- Sys.time()
        parameter_updates$cells_fixed <- input$cells_slider
      }
    })
    observe({
      if (!is.null(input$reads_slider)) {
        user_interaction$active <- TRUE
        user_interaction$last_change <- Sys.time()
        parameter_updates$mapped_reads_fixed <- input$reads_slider
      }
    })
    observe({
      if (!is.null(input$TPM_slider)) {
        user_interaction$active <- TRUE
        user_interaction$last_change <- Sys.time()
        parameter_updates$TPM_threshold_fixed <- input$TPM_slider
      }
    })
    observe({
      if (!is.null(input$fc_slider)) {
        user_interaction$active <- TRUE
        user_interaction$last_change <- Sys.time()
        parameter_updates$minimum_fold_change_fixed <- input$fc_slider
      }
    })
    
    # Return parameter updates for consumption by parent app
    return(reactive({
      list(
        experimental_setup = list(
          MOI = parameter_updates$moi,
          num_targets = parameter_updates$num_targets,
          gRNAs_per_target = parameter_updates$gRNAs_per_target,
          cells_fixed = parameter_updates$cells_fixed,
          mapped_reads_fixed = parameter_updates$mapped_reads_fixed
        ),
        analysis_choices = list(
          TPM_threshold_fixed = parameter_updates$TPM_threshold_fixed
        ),
        effect_sizes = list(
          minimum_fold_change_fixed = parameter_updates$minimum_fold_change_fixed
        ),
        timestamp = Sys.time()
      )
    }))
    
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
