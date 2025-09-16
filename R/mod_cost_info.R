#' cost_info UI Function
#'
#' @description Creates the Cost Information section with cost per cell and cost per million reads inputs.
#' Appears conditionally when Design Options involve power + cost optimization or cost minimization.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList tags div strong numericInput
mod_cost_info_ui <- function(id) {
  ns <- NS(id)
  
  # Cost Information (collapsible) - CONDITIONAL SECTION
  tagList(
    tags$div(
    id = ns("cost-info-container"),
    style = "border-radius: 4px; margin-bottom: 5px; display: none;",
    tags$div(
      id = ns("cost-header"),
      style = "padding: 10px 15px; cursor: pointer; border-radius: 4px 4px 0 0;",
      onclick = paste0("toggleSection('", ns("cost-content"), "', '", ns("cost-chevron"), "')"),
      tags$i(id = ns("cost-chevron"), class = "fa fa-chevron-right", style = "margin-right: 8px;"),
      tags$strong("Cost Information")
    ),
    tags$div(
      id = ns("cost-content"),
      style = "padding: 15px;",
      
      # Two column layout for cost inputs
      tags$div(
        style = "display: flex; gap: 5px;",
        
        # Cost per cell column
        tags$div(
          style = "flex: 0 0 auto; max-width: 120px;",
          tags$label("Cost/cell:", style = "font-weight: bold; margin-bottom: 5px; display: block;"),
          tags$div(
            class = "cost-input-container",
            tags$span("$", class = "dollar-sign"),
            tags$div(
              numericInput(ns("cost_per_cell"),
                          label = NULL,
                          value = 0.01,
                          min = 0,
                          step = 0.001),
              tags$style(paste0("#", ns("cost_per_cell"), " { width: 80px !important; max-width: 80px !important; }"))
            )
          )
        ),
        
        # Cost per million reads column
        tags$div(
          style = "flex: 0 0 auto; max-width: 150px;",
          tags$label("Cost/million reads:", style = "font-weight: bold; margin-bottom: 5px; display: block;"),
          tags$div(
            class = "cost-input-container",
            tags$span("$", class = "dollar-sign"),
            tags$div(
              numericInput(ns("cost_per_million_reads"),
                          label = NULL,
                          value = 1.00,
                          min = 0,
                          step = 0.01),
              tags$style(paste0("#", ns("cost_per_million_reads"), " { width: 80px !important; max-width: 80px !important; }"))
            )
          )
        )
      )
    )
    )
  )
}
    
#' cost_info Server Functions
#'
#' @description Server logic for cost information parameters with conditional display
#'
#' @param id Module namespace ID
#' @param design_config Reactive containing design options configuration
#'
#' @noRd 
#'
#' @importFrom shinyjs show hide
mod_cost_info_server <- function(id, design_config){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    # Conditional display logic based on design options
    observe({
      config <- design_config()
      show_cost_info <- FALSE
      
      if (!is.null(config)) {
        opt_type <- config$optimization_type
        target <- config$minimization_target
        
        # Show if power + cost optimization OR cost minimization
        if (!is.null(opt_type) && opt_type == "power_cost") {
          show_cost_info <- TRUE
        } else if (!is.null(target) && target == "cost") {
          show_cost_info <- TRUE
        }
      }
      
      if (show_cost_info) {
        shinyjs::show("cost-info-container")
      } else {
        shinyjs::hide("cost-info-container")
      }
    })
    
    # Return cost information configuration
    cost_config <- reactive({
      list(
        cost_per_cell = input$cost_per_cell,
        cost_per_million_reads = input$cost_per_million_reads,
        timestamp = Sys.time()
      )
    })
    
    return(cost_config)
  })
}
    
## To be copied in the UI
# mod_cost_info_ui("cost_info_1")
    
## To be copied in the server
# mod_cost_info_server("cost_info_1")
