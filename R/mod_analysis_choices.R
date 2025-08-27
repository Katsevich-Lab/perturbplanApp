#' analysis_choices UI Function
#'
#' @description Creates the Analysis Choices section for perturbation-gene pairs,
#' test parameters, and statistical settings
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList tags div strong selectInput fileInput conditionalPanel numericInput moduleServer reactive observe updateNumericInput
#' @importFrom shinyjs show hide
mod_analysis_choices_ui <- function(id) {
  ns <- NS(id)

  # VARIABLE CONSISTENCY TRACKING:
  # - TPM input field: "TPM_threshold_fixed"
  # - TPM div container: "TPM_threshold_fixed_div"
  # - Parameter control key: "TPM_threshold"
  # - Return field: "TPM_threshold_fixed"

  # Analysis choices (collapsible) - ADAPTED FROM ORIGINAL (MINUS TPM/FC CONTROLS)
  tagList(
    tags$div(
      style = "border-radius: 4px; margin-bottom: 5px;",
      tags$div(
        id = ns("analysis-header"),
        style = "padding: 10px 15px; cursor: pointer; border-radius: 4px 4px 0 0;",
        onclick = paste0("toggleSection('", ns("analysis-content"), "', '", ns("analysis-chevron"), "')"),
        tags$i(id = ns("analysis-chevron"), class = "fa fa-chevron-right", style = "margin-right: 8px;"),
        tags$strong("Analysis choices")
      ),
      tags$div(
        id = ns("analysis-content"),
        style = "padding: 15px;",
        selectInput(ns("gene_list_mode"), "Perturbation-gene pairs to analyze:",
                   choices = c("Random" = "random", "Custom" = "custom"),
                   selected = "random"),
        conditionalPanel(
          condition = paste0("input['", ns("gene_list_mode"), "'] == 'custom'"),
          tags$div(
            class = "file-upload-info",
            style = "border-radius: 3px; padding: 6px; margin: 5px 0;",
            tags$small(
              tags$i(class = "fa fa-info-circle", style = "margin-right: 3px;"),
              tags$strong("Format: "), "CSV with 'grna_target' and 'response_id' columns",
              style = "font-size: 11px;"
            )
          ),
          fileInput(ns("gene_list_file"),
                   label = NULL,
                   accept = c(".csv"),
                   placeholder = "Choose CSV file...")
        ),
        selectInput(ns("side"), "Test side:",
                    choices = c("Left (knockdown)" = "left",
                               "Right (overexpression)" = "right",
                               "Both (two-sided)" = "both"),
                    selected = "left"),

        # Fixed value input for TPM analysis parameter (conditional)
        # CONSISTENT VARIABLE USAGE: TPM_threshold_fixed throughout
        tags$div(
          id = ns("TPM_threshold_fixed_div"),
          style = "margin-top: 15px; padding-top: 15px; border-top: 1px solid #E3E6EA; display: none;",
          numericInput(ns("TPM_threshold_fixed"), "TPM analysis threshold:",
                      value = 10, min = 1, max = 500, step = 1)
        )
      )
    )
  )
}

#' analysis_choices Server Functions
#'
#' @description Server logic for analysis choice parameters and gene list processing
#'
#' @param design_config Reactive containing design options configuration
#' @param param_manager Parameter manager instance (central hub)
#' @param external_updates Reactive containing parameter updates from sliders (DEPRECATED)
#'
#' @noRd
mod_analysis_choices_server <- function(id, design_config, param_manager){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    # ========================================================================
    # INPUT FEEDING - SAFE: Using isolate() to break reactive cycles
    # ========================================================================
    
    # Use observeEvent + isolate to prevent circular reactive dependencies
    observeEvent(input$TPM_threshold_fixed, {
      isolate({
        param_manager$update_parameter("TPM_threshold", input$TPM_threshold_fixed, "sidebar")
      })
    })
    
    # ========================================================================
    # UI UPDATES - SAFE: Using observeEvent + isolate for controlled updates
    # ========================================================================
    
    # Safe UI updates: Only update when parameter manager changes AND value is different
    observeEvent(param_manager$parameters$TPM_threshold, {
      new_value <- param_manager$parameters$TPM_threshold
      if (!identical(isolate(input$TPM_threshold_fixed), new_value)) {
        updateNumericInput(session, "TPM_threshold_fixed", value = new_value)
      }
    })


    # VARIABLE CONSISTENCY TRACKING:
    # - Parameter control key: config$parameter_controls$TPM_threshold$type
    # - Show/hide target: "TPM_threshold_fixed_div"
    # - Input reference: input$TPM_threshold_fixed
    # - Return field: TPM_threshold_fixed = input$TPM_threshold_fixed

    # Conditional display logic for fixed value input
    observe({
      config <- design_config()

      if (!is.null(config) && !is.null(config$parameter_controls)) {
        # CONSISTENT: Use TPM_threshold parameter control key
        TPM_type <- config$parameter_controls$TPM_threshold$type

        # Show TPM fixed input only when TPM parameter is set to "fixed"
        # CONSISTENT: Use TPM_threshold_fixed_div
        if (!is.null(TPM_type) && TPM_type == "fixed") {
          shinyjs::show("TPM_threshold_fixed_div")
        } else {
          shinyjs::hide("TPM_threshold_fixed_div")
        }
      } else {
        # CONSISTENT: Use TPM_threshold_fixed_div
        shinyjs::hide("TPM_threshold_fixed_div")
      }
    })

    # Gene list processing
    gene_list_data <- reactive({
      if (input$gene_list_mode == "custom" && !is.null(input$gene_list_file)) {
        # TODO: Add CSV validation and loading logic
        # For now, return placeholder structure
        list(
          type = "custom",
          file_path = input$gene_list_file$datapath,
          file_name = input$gene_list_file$name
        )
      } else {
        list(
          type = "random"
        )
      }
    })

    # Return analysis choices configuration
    analysis_config <- reactive({
      list(
        gene_list_mode = input$gene_list_mode,
        gene_list_data = gene_list_data(),
        side = input$side,
        # CONSISTENT: Return TPM_threshold_fixed field using input$TPM_threshold_fixed (only default if actually hidden)
        TPM_threshold_fixed = input$TPM_threshold_fixed,
        timestamp = Sys.time()
      )
    })

    return(analysis_config)
  })
}

## To be copied in the UI
# mod_analysis_choices_ui("analysis_choices_1")

## To be copied in the server
# mod_analysis_choices_server("analysis_choices_1")
