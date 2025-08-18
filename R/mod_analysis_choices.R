#' analysis_choices UI Function
#'
#' @description Creates the Analysis Choices section for perturbation-gene pairs,
#' test parameters, and statistical settings
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList tags div strong selectInput fileInput conditionalPanel numericInput
mod_analysis_choices_ui <- function(id) {
  ns <- NS(id)
  
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
        selectInput(ns("control_group"), "Control group:", 
                    choices = c("Complement cells" = "complement", 
                               "Non-targeting cells" = "nt_cells"), 
                    selected = "complement"),
        numericInput(ns("fdr_target"), "FDR target level:", 0.05, 0.001, 0.1, 0.001),
        
        # Fixed value input for analysis parameter
        tags$div(
          style = "margin-top: 15px; padding-top: 15px; border-top: 1px solid #E3E6EA;",
          numericInput(ns("tpm_fixed"), "TPM analysis threshold:", 
                      value = 10, min = 0, max = 100, step = 1)
        )
      )
    )
  )
}
    
#' analysis_choices Server Functions
#'
#' @description Server logic for analysis choice parameters and gene list processing
#'
#' @noRd 
mod_analysis_choices_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
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
        control_group = input$control_group,
        fdr_target = input$fdr_target,
        # Fixed value input
        tmp_fixed = input$tmp_fixed,
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
