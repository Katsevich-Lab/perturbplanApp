#' advanced_choices UI Function
#'
#' @description Creates the Advanced Settings section for technical parameters
#' that advanced users may want to modify. Contains gRNA variability and mapping efficiency.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList tags div strong numericInput moduleServer reactive
mod_advanced_choices_ui <- function(id) {
  ns <- NS(id)
  
  # Advanced settings (collapsible, initially collapsed)
  tagList(
    tags$div(
      style = "border-radius: 4px; margin-bottom: 5px;",
      tags$div(
        id = ns("advanced-header"),
        style = "padding: 10px 15px; cursor: pointer; border-radius: 4px 4px 0 0;",
        onclick = paste0("toggleSection('", ns("advanced-content"), "', '", ns("advanced-chevron"), "')"),
        tags$i(id = ns("advanced-chevron"), class = "fa fa-chevron-right", style = "margin-right: 8px;"),
        tags$strong("Advanced settings"),
        tags$i(class = "fa fa-exclamation-triangle", style = "margin-left: 8px; color: #f39c12;")
      ),
      tags$div(
        id = ns("advanced-content"),
        style = "padding: 15px;",
        
        # Warning message for advanced users
        tags$div(
          class = "alert-warning",
          style = "background-color: #fff3cd; border: 1px solid #ffeaa7; padding: 8px; border-radius: 4px; margin-bottom: 15px;",
          tags$small(
            tags$i(class = "fa fa-info-circle", style = "margin-right: 5px;"),
            "These parameters have sensible defaults. Change only if you understand their impact.",
            style = "color: #856404;"
          )
        ),
        
        # gRNA variability (moved from effect sizes)
        numericInput(ns("gRNA_variability"), 
                    "gRNA variability:", 
                    value = 0.15, 
                    min = 0.1, 
                    max = 5, 
                    step = 0.05),
        
        # Mapping efficiency (moved from experimental setup)
        numericInput(ns("mapping_efficiency"), 
                    "Mapping efficiency:", 
                    value = 0.72, 
                    min = 0.1, 
                    max = 1.0, 
                    step = 0.01)
      )
    )
  )
}
    
#' advanced_choices Server Functions
#'
#' @description Server logic for advanced settings parameters
#'
#' @param id Module namespace ID
#' 
#' @return Reactive list containing advanced parameter configuration
#' 
#' @noRd 
mod_advanced_choices_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    # Return advanced choices configuration
    advanced_choices_config <- reactive({
      list(
        gRNA_variability = input$gRNA_variability,
        mapping_efficiency = input$mapping_efficiency,
        timestamp = Sys.time()
      )
    })
    
    return(advanced_choices_config)
  })
}
    
## To be copied in the UI
# mod_advanced_choices_ui("advanced_choices_1")
    
## To be copied in the server
# mod_advanced_choices_server("advanced_choices_1")