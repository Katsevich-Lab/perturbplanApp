#' experimental_setup UI Function
#'
#' @description Creates the Experimental Setup section for biological system
#' selection and reference data uploads
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList tags div strong selectInput fileInput conditionalPanel numericInput moduleServer reactive observe observeEvent req reactiveVal showNotification renderUI outputOptions htmlOutput HTML updateSelectInput updateNumericInput
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
        selectInput(ns("biological_system"), "Biological system:",
                   choices = list("K562" = "K562",
                                "A549" = "A549",
                                "THP-1" = "THP-1",
                                "T CD8" = "T_CD8",
                                "iPSC" = "iPSC",
                                "iPSC neuron" = "iPSC_neuron",
                                "Custom" = "Custom"),
                   selected = "K562"),
        conditionalPanel(
          condition = paste0("input['", ns("biological_system"), "'] == 'Custom'"),
          tags$div(
            class = "file-upload-info",
            tags$small(
              tags$i(class = "fa fa-info-circle"),
              tags$strong("Format: "), "Combined RDS file with baseline expression and library parameters"
            )
          ),
          fileInput(ns("pilot_data_file"), 
                   label = NULL,
                   accept = c(".rds", ".RDS"),
                   placeholder = "Choose reference expression data RDS file..."),
          
          # Upload status display (conditional)
          conditionalPanel(
            condition = "output.pilot_data_uploaded",
            ns = ns,
            tags$div(
              class = "file-upload-success status-success",
              tags$i(class = "fa fa-check-circle"),
              htmlOutput(ns("pilot_data_status"), inline = TRUE)
            )
          )
        ),
        
        # Perturbation choices section (integrated from mod_perturbation_choices)
        tags$div(
          
          # MOI (Multiplicity of Infection)
          numericInput(ns("MOI"),
                      "Multiplicity of infection (MOI):",
                      value = 10,
                      min = 1,
                      max = 30,
                      step = 1),
          
          # Number of targets
          numericInput(ns("num_targets"), 
                      "Number of targets:",
                      value = 100,
                      min = 50,
                      max = 12000,
                      step = 50),
          
          # gRNAs per target
          numericInput(ns("gRNAs_per_target"), 
                      "gRNAs per target:",
                      value = 4,
                      min = 1,
                      max = 20,
                      step = 1),
          
          # Non-targeting gRNAs
          numericInput(ns("non_targeting_gRNAs"), 
                      "Non-targeting gRNAs:",
                      value = 10,
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
            numericInput(ns("cells_fixed"), "Cells per target:",
                        value = 1000, min = 20, max = 2000, step = 20)
          ),
          
          # Reads per cell fixed value (conditional)
          tags$div(
            id = ns("reads_per_cell_fixed_div"),
            style = "display: none;",
            numericInput(ns("reads_per_cell_fixed"), "Sequenced reads per cell:",
                        value = 5000, min = 1000, max = 150000, step = 1000)
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
