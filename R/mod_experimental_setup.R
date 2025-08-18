#' experimental_setup UI Function
#'
#' @description Creates the Experimental Setup section for biological system
#' selection and reference data uploads
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList tags div strong selectInput fileInput conditionalPanel numericInput moduleServer reactive observe
#' @importFrom shinyjs show hide
mod_experimental_setup_ui <- function(id) {
  ns <- NS(id)
  
  # Experimental setup (collapsible) - ADAPTED FROM ORIGINAL
  tagList(
    tags$div(
      style = "border-radius: 4px; margin-bottom: 5px;",
      tags$div(
        id = ns("exp-header"),
        style = "padding: 10px 15px; cursor: pointer; border-radius: 4px 4px 0 0;",
        onclick = paste0("toggleSection('", ns("experimental-content"), "', '", ns("exp-chevron"), "')"),
        tags$i(id = ns("exp-chevron"), class = "fa fa-chevron-right", style = "margin-right: 8px;"),
        tags$strong("Experimental choices")
      ),
      tags$div(
        id = ns("experimental-content"),
        style = "padding: 15px;",
        selectInput(ns("biological_system"), "Biological system:", c("K562", "Other")),
        selectInput(ns("pilot_data_choice"), "Reference expression data:",
                   choices = c("Built-in" = "default", "Custom" = "custom"),
                   selected = "default"),
        conditionalPanel(
          condition = paste0("input['", ns("pilot_data_choice"), "'] != 'default'"),
          tags$div(
            class = "file-upload-info",
            style = "border-radius: 3px; padding: 6px; margin: 5px 0;",
            tags$small(
              tags$i(class = "fa fa-info-circle", style = "margin-right: 3px;"),
              tags$strong("Format: "), "Combined RDS file with baseline expression and library parameters",
              style = "font-size: 11px;"
            )
          ),
          fileInput(ns("pilot_data_file"), 
                   label = NULL,
                   accept = c(".rds", ".RDS"),
                   placeholder = "Choose reference expression data RDS file...")
        ),
        
        # Fixed value inputs for experimental parameters (conditional)
        tags$div(
          id = ns("experimental_fixed_params"),
          style = "margin-top: 20px; padding-top: 15px; border-top: 1px solid #E3E6EA; display: none;",
          
          # Cells per target fixed value (conditional)
          tags$div(
            id = ns("cells_fixed_div"),
            style = "display: none; margin-bottom: 15px;",
            numericInput(ns("cells_fixed"), "Cells per target:", 
                        value = 1000, min = 50, max = 5000, step = 50)
          ),
          
          # Reads per cell fixed value (conditional)
          tags$div(
            id = ns("reads_fixed_div"),
            style = "display: none; margin-bottom: 15px;",
            numericInput(ns("reads_fixed"), "Reads per cell:", 
                        value = 5000, min = 500, max = 20000, step = 500)
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
#'
#' @noRd 
mod_experimental_setup_server <- function(id, design_config){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    # Conditional display logic for fixed value inputs
    observe({
      config <- design_config()
      
      if (!is.null(config) && !is.null(config$parameter_controls)) {
        cells_type <- config$parameter_controls$cells_per_target$type
        reads_type <- config$parameter_controls$reads_per_cell$type
        
        # Show cells fixed input only when cells parameter is set to "fixed"
        if (!is.null(cells_type) && cells_type == "fixed") {
          shinyjs::show("cells_fixed_div")
          shinyjs::show("experimental_fixed_params")
        } else {
          shinyjs::hide("cells_fixed_div")
        }
        
        # Show reads fixed input only when reads parameter is set to "fixed"
        if (!is.null(reads_type) && reads_type == "fixed") {
          shinyjs::show("reads_fixed_div")
          shinyjs::show("experimental_fixed_params")
        } else {
          shinyjs::hide("reads_fixed_div")
        }
        
        # Hide the entire section if neither parameter is fixed
        if ((is.null(cells_type) || cells_type != "fixed") && 
            (is.null(reads_type) || reads_type != "fixed")) {
          shinyjs::hide("experimental_fixed_params")
        }
      } else {
        shinyjs::hide("experimental_fixed_params")
        shinyjs::hide("cells_fixed_div")
        shinyjs::hide("reads_fixed_div")
      }
    })
    
    # File upload validation and processing would go here
    pilot_data <- reactive({
      if (input$pilot_data_choice == "custom" && !is.null(input$pilot_data_file)) {
        # TODO: Add file validation and loading logic
        # For now, return placeholder structure
        list(
          type = "custom",
          file_path = input$pilot_data_file$datapath,
          file_name = input$pilot_data_file$name
        )
      } else {
        list(
          type = "default",
          biological_system = input$biological_system
        )
      }
    })
    
    # Return experimental setup configuration
    experimental_config <- reactive({
      list(
        biological_system = input$biological_system,
        pilot_data_choice = input$pilot_data_choice,
        pilot_data = pilot_data(),
        # Fixed value inputs
        cells_fixed = input$cells_fixed,
        reads_fixed = input$reads_fixed,
        timestamp = Sys.time()
      )
    })
    
    return(experimental_config)
  })
}
    
## To be copied in the UI
# mod_experimental_setup_ui("experimental_setup_1")
    
## To be copied in the server
# mod_experimental_setup_server("experimental_setup_1")
