#' analysis_choices UI Function
#'
#' @description Creates the Analysis Choices section for perturbation-gene pairs,
#' test parameters, and statistical settings
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList tags div strong selectInput fileInput conditionalPanel numericInput moduleServer reactive observe updateNumericInput showModal modalDialog modalButton observeEvent updateSelectInput
#' @importFrom shinyjs show hide
mod_analysis_choices_ui <- function(id) {
  ns <- NS(id)

  # VARIABLE CONSISTENCY TRACKING:
  # - Expression input field: "Expression_threshold_fixed"
  # - Expression div container: "Expression_threshold_fixed_div"
  # - Parameter control key: "TPM_threshold"
  # - Return field: "Expression_threshold_fixed"

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
        selectInput(ns("gene_list_mode"),
                   "Perturbation-gene pairs to test:",
                   choices = c("Random" = "random", "Custom" = "custom"),
                   selected = "random"),
        conditionalPanel(
          condition = paste0("input['", ns("gene_list_mode"), "'] == 'custom'"),
          style = "margin-bottom: -40px;",
          tags$div(
            class = "file-upload-info",
            style = "border-radius: 3px; padding: 6px; margin: 5px 0;",
            tags$small(
              tags$i(class = "fa fa-info-circle", style = "margin-right: 3px;"),
              "Data frame with columns grna_target and response_id (RDS format)",
              style = "font-size: 11px;"
            )
          ),
          fileInput(ns("gene_list_file"),
                   label = NULL,
                   accept = c(".rds", ".RDS"),
                   placeholder = "No file selected"),

          # Gene list upload status display
          tags$div(
            id = ns("gene_list_status_div"),
            htmlOutput(ns("gene_list_status"))
          )
        ),

        selectInput(ns("side"),
                      add_tooltip("Test sidedness:", "test_side", use_icon = TRUE),
                    choices = c("Left (expression decrease)" = "left",
                               "Right (expression increase)" = "right",
                               "Both (increase or decrease)" = "both"),
                    selected = "left"),

        # Fixed value input for Expression threshold parameter (conditional)
        # CONSISTENT VARIABLE USAGE: Expression_threshold_fixed throughout
        tags$div(
          id = ns("Expression_threshold_fixed_div"),
          style = "margin-top: 10px; display: none;",

          # TAP-seq version
          conditionalPanel(
            condition = "input['sidebar-design_options-assay_type'] == 'tap_seq'",
            numericInput(ns("Expression_threshold_fixed"),
                        add_tooltip("Expression threshold (UMIs/cell):", "umis_per_cell", use_icon = TRUE),
                        value = 1, min = 0.1, max = 5, step = 0.02)
          ),

          # Perturb-seq version (default)
          conditionalPanel(
            condition = "input['sidebar-design_options-assay_type'] == 'perturb_seq'",
            numericInput(ns("Expression_threshold_fixed"),
                        add_tooltip("TPM analysis threshold:", "tpm_threshold", use_icon = TRUE),
                        value = 10, min = 1, max = 200, step = 1)
          )
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
#' @param external_updates Reactive containing parameter updates from sliders (DEPRECATED)
#'
#' @noRd
mod_analysis_choices_server <- function(id, design_config, app_state = NULL){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    # ========================================================================
    # SIDEBAR MODULE - INDEPENDENT FROM SLIDERS
    # ========================================================================
    # No parameter manager integration - sidebar operates independently


    # VARIABLE CONSISTENCY TRACKING:
    # - Parameter control key: config$parameter_controls$TPM_threshold$type
    # - Show/hide target: "Expression_threshold_fixed_div"
    # - Input reference: input$Expression_threshold_fixed
    # - Return field: Expression_threshold_fixed = input$Expression_threshold_fixed

    # Conditional display logic for fixed value input
    observe({
      config <- design_config()

      if (!is.null(config) && !is.null(config$parameter_controls)) {
        # CONSISTENT: Use TPM_threshold parameter control key
        TPM_type <- config$parameter_controls$TPM_threshold$type

        # Show Expression threshold fixed input only when TPM parameter is set to "fixed"
        # CONSISTENT: Use Expression_threshold_fixed_div
        if (!is.null(TPM_type) && TPM_type == "fixed") {
          shinyjs::show("Expression_threshold_fixed_div")
        } else {
          shinyjs::hide("Expression_threshold_fixed_div")
        }
      } else {
        # CONSISTENT: Use Expression_threshold_fixed_div
        shinyjs::hide("Expression_threshold_fixed_div")
      }
    })

    # Reset Expression_threshold_fixed to correct default when assay type changes
    # TAP-seq: 1 (UMIs/cell), Perturb-seq: 10 (TPM threshold)
    observeEvent(design_config()$assay_type, {
      assay_type <- design_config()$assay_type

      if (!is.null(assay_type) && assay_type == "tap_seq") {
        # TAP-seq: UMIs/cell at saturation
        updateNumericInput(session, "Expression_threshold_fixed", value = 1)
      } else if (!is.null(assay_type) && assay_type == "perturb_seq") {
        # Perturb-seq: TPM analysis threshold
        updateNumericInput(session, "Expression_threshold_fixed", value = 10)
      }
    }, ignoreNULL = FALSE, ignoreInit = TRUE)

    # Gene list processing
    gene_list_data <- reactive({
      if (input$gene_list_mode == "custom" && !is.null(input$gene_list_file)) {
        # Validate and load RDS file using our validation function
        validation_result <- validate_gene_list_rds(input$gene_list_file$datapath)

        if (validation_result$is_valid) {
          # Return successful validation with data and summary
          list(
            type = "custom",
            file_path = input$gene_list_file$datapath,
            file_name = input$gene_list_file$name,
            data = validation_result$data,
            is_valid = TRUE,
            errors = NULL,
            summary = validation_result$summary
          )
        } else {
          # Return validation errors
          list(
            type = "custom",
            file_path = input$gene_list_file$datapath,
            file_name = input$gene_list_file$name,
            data = NULL,
            is_valid = FALSE,
            errors = validation_result$errors,
            summary = NULL
          )
        }
      } else {
        list(
          type = "random",
          is_valid = TRUE,
          errors = NULL
        )
      }
    })

    # Gene list upload status output
    output$gene_list_status <- renderUI({
      if (input$gene_list_mode == "custom" && !is.null(input$gene_list_file)) {
        gene_data <- gene_list_data()

        if (gene_data$is_valid) {
          # Compact success message with key statistics
          summary <- gene_data$summary
          HTML(paste0(
            '<div class="file-upload-success" style="margin-bottom: 38px;">',
            '<i class="fa fa-check-circle" style="margin-right: 5px;"></i>',
            '<strong>Gene list loaded:</strong> ',
            summary$total_pairs, ' pairs, ',
            summary$unique_genes, ' genes, ',
            summary$unique_targets, ' targets from <em>', gene_data$file_name, '</em>',
            '</div>'
          ))
        } else {
          # Error message
          error_list <- paste(gene_data$errors, collapse = "<br>\\u2022 ")
          HTML(paste0(
            '<div class="upload-status status-error" style="background-color: rgba(200, 90, 90, 0.1); border: 1px solid #C85A5A; color: #8B3A3A; padding: 8px; border-radius: 4px; margin: 5px 0; margin-bottom: 38px;">',
            '<i class="fa fa-exclamation-triangle" style="margin-right: 5px;"></i>',
            '<strong>Validation failed:</strong><br>\\u2022 ',
            error_list,
            '</div>'
          ))
        }
      } else {
        NULL
      }
    })

    # Return analysis choices configuration
    analysis_config <- reactive({
      list(
        gene_list_mode = input$gene_list_mode,
        gene_list_data = gene_list_data(),
        side = input$side,
        # CONSISTENT: Return Expression_threshold_fixed field using input$Expression_threshold_fixed (only default if actually hidden)
        Expression_threshold_fixed = input$Expression_threshold_fixed,
        timestamp = Sys.time()
      )
    })

    # INPUT FREEZING: Disable all inputs in Phase 2, keep section titles functional
    observeEvent(app_state$phase, {
      if (!is.null(app_state)) {
        inputs_disabled <- (app_state$phase == 2)

        # Core analysis inputs that should be disabled in Phase 2
        shinyjs::toggleState("gene_list_mode", condition = !inputs_disabled)
        shinyjs::toggleState("gene_list_file", condition = !inputs_disabled)
        shinyjs::toggleState("side", condition = !inputs_disabled)
        shinyjs::toggleState("Expression_threshold_fixed", condition = !inputs_disabled)

        # Note: Section headers remain functional for collapse/expand
      }
    }, ignoreInit = TRUE, ignoreNULL = TRUE)

    return(analysis_config)
  })
}

## To be copied in the UI
# mod_analysis_choices_ui("analysis_choices_1")

## To be copied in the server
# mod_analysis_choices_server("analysis_choices_1")
