#' experimental_setup UI Function
#'
#' @description Creates the Experimental Setup section for biological system
#' selection and reference data uploads
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList tags div strong selectInput fileInput conditionalPanel numericInput moduleServer reactive observe observeEvent req reactiveVal renderUI outputOptions htmlOutput HTML updateSelectInput updateNumericInput
#' @importFrom shinyjs show hide
#' @importFrom tools file_ext
mod_experimental_setup_ui <- function(id) {
  ns <- NS(id)

  # Experimental setup (collapsible) - ADAPTED FROM ORIGINAL
  tagList(
    tags$div(
      class = "collapsible-section",
      tags$div(
        id = ns("exp-header"),
        class = "collapsible-header",
        onclick = paste0("toggleSection('", ns("experimental-content"), "', '", ns("exp-chevron"), "')"),
        tags$i(id = ns("exp-chevron"), class = "fa fa-chevron-right"),
        tags$strong("Experimental choices")
      ),
      tags$div(
        id = ns("experimental-content"),
        class = "collapsible-content",

        selectInput(ns("biological_system"),
                   add_tooltip("Reference expression data:", "reference_expression_data", use_icon = TRUE),
                   choices = get_biological_system_choices(),
                   selected = "K562"),
        conditionalPanel(
          condition = paste0("input['", ns("biological_system"), "'] == 'Custom'"),
          style = "margin-bottom: -40px;",
          tags$div(
            class = "file-upload-info",
            tags$small(
              tags$i(class = "fa fa-info-circle"),
              tags$strong("Format: "), "RDS file with baseline expression, saturation curve, and mapping efficiency."
            )
          ),

          # TAP-seq example data download link
          conditionalPanel(
            condition = "input['sidebar-design_options-assay_type'] == 'tap_seq'",
            tags$div(
              class = "file-upload-info",
              style = "margin-top: 5px; margin-bottom: 10px;",
              tags$small(
                tags$i(class = "fa fa-exclamation-triangle"),
                " TAP-seq requires custom reference data. Need example data? ",
                tags$a(href = "https://raw.githubusercontent.com/Katsevich-Lab/perturbplanApp/main/inst/extdata/K562_Ray.rds",
                      download = "K562_Ray.rds",
                      style = "text-decoration: underline;",
                      "Download K562_Ray.rds")
              )
            )
          ),

          fileInput(ns("pilot_data_file"),
                   label = NULL,
                   accept = c(".rds", ".RDS"),
                   placeholder = "No file selected"),

          # Upload status display (conditional)
          conditionalPanel(
            condition = "output.pilot_data_uploaded",
            ns = ns,
            tags$div(
              class = "file-upload-success status-success",
              style = "margin-bottom: 35px;",
              tags$i(class = "fa fa-check-circle"),
              htmlOutput(ns("pilot_data_status"), inline = TRUE)
            )
          )
        ),

        # Perturbation choices section (integrated from mod_perturbation_choices)
        tags$div(
          # MOI (Multiplicity of Infection)
          numericInput(ns("MOI"),
                      add_tooltip("Multiplicity of infection (MOI):", "moi", use_icon = TRUE),
                      value = 10,
                      min = 1,
                      max = 30,
                      step = 1),

          # Number of perturbation targets
          numericInput(ns("num_targets"),
                      add_tooltip("Number of perturbation targets:", "num_targets", use_icon = TRUE),
                      value = 500,
                      min = 50,
                      max = 12000,
                      step = 50),

          # gRNAs per target
          numericInput(ns("gRNAs_per_target"),
                      add_tooltip("gRNAs per target:", "grnas_per_target", use_icon = TRUE),
                      value = 4,
                      min = 1,
                      max = 20,
                      step = 1),

          # Non-targeting gRNAs
          numericInput(ns("non_targeting_gRNAs"),
                      add_tooltip("Non-targeting gRNAs:", "non_targeting_grnas", use_icon = TRUE),
                      value = 50,
                      min = 0,
                      max = 100,
                      step = 1)
        ),

        # Fixed value inputs for experimental parameters (conditional)
        tags$div(
          id = ns("experimental_fixed_params"),
          style = "display: none;",

          # Cells per target fixed value (conditional)
          tags$div(
            id = ns("cells_fixed_div"),
            style = "display: none;",
            numericInput(ns("cells_fixed"),
                        add_tooltip("Cells per target:", "cells_per_target", use_icon = TRUE),
                        value = 1000, min = 20, max = 2000, step = 20)
          ),

          # Reads per cell fixed value (conditional)
          tags$div(
            id = ns("reads_per_cell_fixed_div"),
            style = "display: none;",
            numericInput(ns("reads_per_cell_fixed"),
                        add_tooltip("Sequenced reads per cell:", "sequenced_reads_per_cell", use_icon = TRUE),
                        value = 20000, min = 1000, max = 150000, step = 1000)
          )
        )
      )
    )
  )
}

#' experimental_setup Server Functions
#'
#' @description Server logic for experimental setup parameters and file uploads
#'
#' @param design_config Reactive containing design options configuration
#' @param external_updates Reactive containing parameter updates from sliders (DEPRECATED)
#'
#' @noRd
mod_experimental_setup_server <- function(id, design_config, app_state = NULL){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    # ========================================================================
    # SIDEBAR MODULE - INDEPENDENT FROM SLIDERS
    # ========================================================================
    # No parameter manager integration - sidebar operates independently

    # Get default values from centralized configuration
    defaults <- get_experimental_defaults()

    # Track previous optimization type for mode switching
    previous_mode <- reactiveVal(NULL)

    # Conditional display logic for fixed value inputs - using extracted functions
    observe({
      config <- design_config()

      # Handle optimization mode changes
      handle_mode_change(session, config, previous_mode, defaults)

      # Update fixed parameter visibility
      parameter_controls <- if (!is.null(config)) config$parameter_controls else NULL
      update_fixed_parameter_visibility(session, parameter_controls)
    })

    # Custom pilot data reactive value
    custom_pilot_data <- reactiveVal(NULL)

    # File upload processing - using extracted function
    observeEvent(input$pilot_data_file, {
      req(input$pilot_data_file)
      handle_file_upload(session, input$pilot_data_file, input$biological_system,
                        custom_pilot_data, output, defaults)
    })

    # Reset pilot data when biological system changes from Custom or file is removed
    observe({
      should_reset <- is.null(input$pilot_data_file) || input$biological_system != "Custom"

      if (should_reset) {
        reset_pilot_data_status(session, custom_pilot_data, output)
      }
    })

    # TAP-seq: Auto-select "Custom" when TAP-seq is selected
    # Perturb-seq: Reset to "K562" when Perturb-seq is selected
    observeEvent(design_config()$assay_type, {
      assay_type <- design_config()$assay_type

      if (!is.null(assay_type) && assay_type == "tap_seq") {
        # Auto-select Custom for TAP-seq
        updateSelectInput(session, "biological_system", selected = "Custom")
      } else if (!is.null(assay_type) && assay_type == "perturb_seq") {
        # Reset to K562 for Perturb-seq
        updateSelectInput(session, "biological_system", selected = "K562")
      }
    }, ignoreNULL = FALSE, ignoreInit = TRUE)

    # TAP-seq: Freeze biological_system selector when TAP-seq is selected
    observe({
      config <- design_config()
      assay_type <- if (!is.null(config)) config$assay_type else NULL

      # Freeze biological_system selector when TAP-seq is selected
      if (!is.null(assay_type) && assay_type == "tap_seq") {
        shinyjs::disable("biological_system")
      } else {
        shinyjs::enable("biological_system")
      }
    })

    # Pilot data reactive - using extracted function
    pilot_data <- reactive({
      build_pilot_data_config(input$biological_system, input$pilot_data_file, custom_pilot_data())
    })

    # Return experimental setup configuration - using extracted function
    experimental_config <- reactive({
      assemble_experimental_config(input, pilot_data(), defaults)
    })

    # INPUT FREEZING: Disable all inputs in Phase 2 - using extracted function
    observeEvent(app_state$phase, {
      if (!is.null(app_state)) {
        inputs_disabled <- (app_state$phase == 2)
        toggle_input_states(session, inputs_disabled)
        # Note: Section headers remain functional for collapse/expand
      }
    }, ignoreInit = TRUE, ignoreNULL = TRUE)

    return(experimental_config)
  })
}

## To be copied in the UI
# mod_experimental_setup_ui("experimental_setup_1")

## To be copied in the server
# mod_experimental_setup_server("experimental_setup_1")
