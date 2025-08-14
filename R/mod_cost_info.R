#' Cost Information Module UI
#'
#' @description Creates the Cost Information section with cost per cell and cost per million reads inputs.
#' Appears conditionally when Design Options involve power + cost optimization or cost minimization.
#'
#' @param id Module namespace ID
#'
#' @noRd
#'
#' @importFrom shiny NS tagList tags div strong numericInput
#' @importFrom shinyjs show hide
mod_cost_info_ui <- function(id) {
  ns <- NS(id)
  
  # Cost Information (collapsible) - CONDITIONAL SECTION
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
        style = "display: flex; gap: 20px;",
        
        # Cost per cell column
        tags$div(
          style = "flex: 1;",
          tags$label("Cost per cell:", style = "font-weight: bold; margin-bottom: 5px; display: block;"),
          tags$div(
            class = "cost-input-container",
            tags$span("$", class = "dollar-sign"),
            numericInput(ns("cost_per_cell"), 
                        label = NULL,
                        value = 0.01, 
                        min = 0, 
                        step = 0.001,
                        width = "100%")
          )
        ),
        
        # Cost per million reads column  
        tags$div(
          style = "flex: 1;",
          tags$label("Cost per million reads:", style = "font-weight: bold; margin-bottom: 5px; display: block;"),
          tags$div(
            class = "cost-input-container",
            tags$span("$", class = "dollar-sign"),
            numericInput(ns("cost_per_million_reads"), 
                        label = NULL,
                        value = 1.00, 
                        min = 0, 
                        step = 0.01,
                        width = "100%")
          )
        )
      )
    )
  )
}

#' Cost Information Module Server
#'
#' @description Server logic for cost information parameters with conditional display
#'
#' @param id Module namespace ID
#' @param design_config Reactive containing design options configuration
#'
#' @noRd
mod_cost_info_server <- function(id, design_config) {
  moduleServer(id, function(input, output, session) {
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