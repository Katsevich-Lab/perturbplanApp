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
#' @importFrom shiny NS tagList tags div h5 fluidRow column uiOutput moduleServer reactive observeEvent req renderUI observe isolate reactiveValues actionButton
#' @importFrom shinyWidgets noUiSliderInput
mod_parameter_sliders_ui <- function(id) {
  ns <- NS(id)

  tagList(
    tags$div(
      id = ns("slider_container"),
      class = "parameter-sliders-grid",
      style = "padding: 15px; background-color: #f8f9fa; border-radius: 8px; margin-top: 20px;",

      # TWO-COLUMN LAYOUT: Experimental | Power-determining parameters
      tags$div(
        class = "slider-columns-container",
        fluidRow(
          # COLUMN 1: Experimental parameters (always 3 sliders)
          column(6,
            tags$div(
              class = "slider-column",
              uiOutput(ns("experimental_sliders_column"))
            )
          ),
          # COLUMN 2: Power-determining parameters (dynamic - 3 out of 4 shown)
          column(6,
            tags$div(
              class = "slider-column",
              uiOutput(ns("dynamic_power_sliders_column"))
            )
          )
        )
      ),
      
      # PINNING CONTROLS: Simple two-button layout at bottom
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
create_compact_slider <- function(inputId, label, min, max, value, step, format_decimals = 0) {
  tags$div(
    class = "compact-slider-container",
    tags$label(
      label,
      class = "slider-label",
      style = "font-size: 13px; font-weight: 500; color: #495057; margin-bottom: 8px; display: block; text-align: center;"
    ),
    shinyWidgets::noUiSliderInput(
      inputId = inputId,
      label = NULL,
      min = min,
      max = max,
      value = round(value, format_decimals),
      step = step,
      width = "100%",
      height = "8px",
      update_on = "end",
      tooltips = TRUE,
      format = list(decimals = format_decimals)
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
      reads_per_cell_fixed = NULL,
      TPM_threshold_fixed = NULL,
      minimum_fold_change_fixed = NULL
    )

    # Initialize sliders exactly when sidebar becomes frozen
    observe({
      # Get current sidebar config at the moment of freezing
      config <- sidebar_config()

      if (!is.null(config)) {
        # Initialize sliders with sidebar values at freeze moment
        experimental <- config$experimental_setup %||% list()
        analysis <- config$analysis_choices %||% list()
        effects <- config$effect_sizes %||% list()

        slider_state$MOI <- experimental$MOI
        slider_state$num_targets <- experimental$num_targets
        slider_state$gRNAs_per_target <- experimental$gRNAs_per_target
        slider_state$cells_fixed <- experimental$cells_fixed
        slider_state$reads_per_cell_fixed <- experimental$reads_per_cell_fixed
        slider_state$TPM_threshold_fixed <- analysis$TPM_threshold_fixed
        slider_state$minimum_fold_change_fixed <- effects$minimum_fold_change_fixed

        slider_state$initialized <- TRUE

      }
    })

    # ========================================================================
    # DYNAMIC SLIDER GENERATION
    # ========================================================================

    # Generate experimental parameters column (left column)
    output$experimental_sliders_column <- renderUI({
      config <- sidebar_config()
      if (is.null(config)) return(NULL)

      # Use slider_state if initialized, otherwise use sidebar values
      if (slider_state$initialized) {
        moi_value <- slider_state$MOI
        targets_value <- slider_state$num_targets
        grnas_value <- slider_state$gRNAs_per_target
      } else {
        # Extract values from sidebar configuration with defaults
        experimental <- config$experimental_setup
        moi_value <- experimental$MOI
        targets_value <- experimental$num_targets
        grnas_value <- experimental$gRNAs_per_target
      }

      tagList(
        tags$div(style = "margin-bottom: 8px;", create_compact_slider(ns("moi_slider"), "MOI", 1, 50, moi_value, 1)),
        tags$div(style = "margin-bottom: 8px;", create_compact_slider(ns("targets_slider"), "# of Targets", 50, 12000, targets_value, 50)),
        tags$div(style = "margin-bottom: 5px;", create_compact_slider(ns("grnas_slider"), "gRNAs per Target", 1, 20, grnas_value, 1))
      )
    })

    # Generate power parameters column (right column)
    output$dynamic_power_sliders_column <- renderUI({
      config <- sidebar_config()
      if (is.null(config)) return(NULL)

      # Use pre-computed workflow_info from sidebar (Option B)
      workflow <- config$workflow_info
      if (is.null(workflow)) return(NULL)

      # Get design configuration from sidebar_config
      design_config <- config$design_options
      param_controls <- design_config$parameter_controls
      optimization_type <- design_config$optimization_type

      # Extract power parameter values from sidebar configuration
      experimental <- config$experimental_setup
      analysis <- config$analysis_choices
      effects <- config$effect_sizes

      # Get parameter values with defaults (use slider_state if initialized)
      if (slider_state$initialized) {
        cells_value <- slider_state$cells_fixed
        reads_value <- slider_state$reads_per_cell_fixed
        tpm_value <- slider_state$TPM_threshold_fixed
        fc_value <- slider_state$minimum_fold_change_fixed
      } else {
        cells_value <- experimental$cells_fixed
        reads_value <- experimental$reads_per_cell_fixed
        tpm_value <- analysis$TPM_threshold_fixed
        fc_value <- effects$minimum_fold_change_fixed
      }

      # Determine fold change range based on test sidedness
      test_side <- analysis$side %||% "left"  # Default to left if not specified
      fc_range <- switch(test_side,
        "left" = list(min = 0.5, max = 0.96),     # Knockdown effects
        "right" = list(min = 1.04, max = 2.0),   # Overexpression effects
        "both" = list(min = 0.5, max = 2.0),     # Two-sided test
        list(min = 0.5, max = 2.0)               # Default fallback
      )

      # Ensure fold change value is within the range for the selected test side
      fc_value <- pmax(fc_range$min, pmin(fc_range$max, fc_value))

      # Define all 4 power-determining parameters (use sidebar values)
      all_power_params <- list(
        cells_per_target = list(id = "cells_slider", label = "Cells per Target", min = 20, max = 2000, value = cells_value, step = 20),
        reads_per_cell = list(id = "reads_slider", label = "Reads per Cell", min = 1000, max = 150000, value = reads_value, step = 1000),
        TPM_threshold = list(id = "TPM_slider", label = "TPM Threshold", min = 1, max = 200, value = tpm_value, step = 1),
        minimum_fold_change = list(id = "fc_slider", label = "Fold Change", min = fc_range$min, max = fc_range$max, value = fc_value, step = 0.02)
      )

      # Determine which parameter is being minimized (exclude from Row 2)
      minimized_param <- get_minimized_parameter(workflow$workflow_id)

      # Filter out the minimized parameter
      visible_power_params <- all_power_params[!names(all_power_params) %in% minimized_param]

      # POWER+COST MODE FILTERING OR COST MINIMIZATION: Show sliders for "fixed" parameters
      # Apply filtering logic when we have valid configuration data
      if (!is.null(optimization_type) && optimization_type != "" &&
          length(param_controls) > 0 &&
          length(minimized_param) > 0) {

        # Safe comparison for minimized_param to avoid NA issues
        is_cost_minimization <- length(minimized_param) > 0 && minimized_param[1] == "cost"

        if ((optimization_type == "power_cost" && !is.null(param_controls)) ||
            is_cost_minimization) {

        # Map parameter names to their control types
        param_name_mapping <- list(
          "cells_per_target" = "cells_per_target",
          "reads_per_cell" = "reads_per_cell",  # UI uses same name
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
        }  # Close the inner if statement (power_cost || cost minimization)
      }    # Close the outer if statement (phase == 2 && valid config)

      # If no parameters to show, return empty
      if (length(visible_power_params) == 0) {
        return(NULL)
      }

      # Create dynamic column layout based on number of visible parameters
      num_params <- length(visible_power_params)
      col_width <- if (num_params == 1) 12 else if (num_params == 2) 6 else 4

      tagList(
        lapply(seq_along(names(visible_power_params)), function(i) {
          param_name <- names(visible_power_params)[i]
          param <- visible_power_params[[param_name]]
          # Add spacing between sliders
          margin_bottom <- if (i == length(visible_power_params)) "5px" else "8px"
          tags$div(
            style = paste0("margin-bottom: ", margin_bottom, ";"),
            create_compact_slider(
              ns(param$id), param$label, param$min, param$max, param$value, param$step,
              format_decimals = if (param_name == "minimum_fold_change") 2 else 0
            )
          )
        })
      )
    })

    # Generate Pin buttons section conditionally
    output$pin_buttons_section <- renderUI({
      config <- sidebar_config()
      if (is.null(config)) return(NULL)
      
      # Show Pin buttons for all workflows (simplified approach)
      tags$div(
        style = "padding: 0px 10px 0px 10px; text-align: center; margin-top: -5px; display: flex; gap: 8px; justify-content: center; flex-wrap: wrap;",
        actionButton(
          ns("pin_solution"),
          "Pin Solution",
          class = "btn btn-success btn-sm",
          style = "flex: 0 1 auto; min-width: 100px; max-width: 120px; font-size: 14px; font-weight: 500;"
        ),
        actionButton(
          ns("clear_pins"),
          "Clear All",
          class = "btn btn-outline-secondary btn-sm",
          style = "flex: 0 1 auto; min-width: 80px; max-width: 100px; font-size: 14px; font-weight: 500;"
        )
      )
    })

    # ========================================================================
    # INPUT COLLECTION - Mouse Release Only Updates (via noUiSliderInput)
    # ========================================================================
    # noUiSliderInput with update_on="end" only sends values to server on mouse release
    # This prevents continuous analysis triggers during slider dragging

    # Update slider_state with input values (only triggered on mouse release due to update_on="end")
    observeEvent(input$moi_slider, {
      if (slider_state$initialized) {
        slider_state$MOI <- input$moi_slider
      }
    }, ignoreNULL = TRUE, ignoreInit = TRUE)

    observeEvent(input$targets_slider, {
      if (slider_state$initialized) {
        slider_state$num_targets <- input$targets_slider
      }
    }, ignoreNULL = TRUE, ignoreInit = TRUE)

    observeEvent(input$grnas_slider, {
      if (slider_state$initialized) {
        slider_state$gRNAs_per_target <- input$grnas_slider
      }
    }, ignoreNULL = TRUE, ignoreInit = TRUE)

    observeEvent(input$cells_slider, {
      if (slider_state$initialized) {
        slider_state$cells_fixed <- input$cells_slider
      }
    }, ignoreNULL = TRUE, ignoreInit = TRUE)

    observeEvent(input$reads_slider, {
      if (slider_state$initialized) {
        slider_state$reads_per_cell_fixed <- input$reads_slider
      }
    }, ignoreNULL = TRUE, ignoreInit = TRUE)

    observeEvent(input$TPM_slider, {
      if (slider_state$initialized) {
        slider_state$TPM_threshold_fixed <- input$TPM_slider
      }
    }, ignoreNULL = TRUE, ignoreInit = TRUE)

    observeEvent(input$fc_slider, {
      if (slider_state$initialized) {
        slider_state$minimum_fold_change_fixed <- input$fc_slider
      }
    }, ignoreNULL = TRUE, ignoreInit = TRUE)

    # ========================================================================
    # RETURN SLIDER CONFIGURATION
    # ========================================================================
    # Return reactive containing slider parameter overrides for param_source_manager

    slider_config <- reactive({

      list(
        experimental_setup = list(
          MOI = slider_state$MOI,
          num_targets = slider_state$num_targets,
          gRNAs_per_target = slider_state$gRNAs_per_target,
          cells_fixed = slider_state$cells_fixed,
          reads_per_cell_fixed = slider_state$reads_per_cell_fixed
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

    # Return both slider config and pin triggers
    return(list(
      slider_config = slider_config,
      pin_trigger = reactive({ input$pin_solution }),    # Auto-increments on click
      clear_trigger = reactive({ input$clear_pins })     # Auto-increments on click
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

    # Power+cost workflows (fixing cells/reads)
    "power_cost_TPM_cells" = "TPM_threshold",      # TPM being minimized
    "power_cost_TPM_reads" = "TPM_threshold",      # TPM being minimized
    "power_cost_fc_cells" = "minimum_fold_change", # FC being minimized
    "power_cost_fc_reads" = "minimum_fold_change", # FC being minimized

    # Power+cost workflow (varying cells and reads)
    "power_cost_TPM_cells_reads" = "TPM_threshold",      # TPM being minimized
    "power_cost_fc_cells_reads" = "minimum_fold_change"  # FC being minimized
  )

  return(minimized_map[[workflow_id]] %||% character(0))
}

## To be copied in the UI
# mod_parameter_sliders_ui("parameter_sliders_1")

## To be copied in the server
# mod_parameter_sliders_server("parameter_sliders_1")
