#' Design Options Module UI
#'
#' @description Creates the Design Options section with constraint-driven workflow:
#' Step 1: Optimization Framework, Step 2: Minimization Target, Step 3: Parameter Control
#'
#' @param id Module namespace ID
#'
#' @noRd
#'
#' @importFrom shiny NS tagList tags div strong selectInput numericInput conditionalPanel
mod_design_options_ui <- function(id) {
  ns <- NS(id)
  
  # Design Options (collapsible) - NEW CONSTRAINT-DRIVEN SECTION
  tags$div(
    style = "border-radius: 4px; margin-bottom: 5px;",
    tags$div(
      id = ns("design-header"),
      style = "padding: 10px 15px; cursor: pointer; border-radius: 4px 4px 0 0;",
      onclick = paste0("toggleSection('", ns("design-content"), "', '", ns("design-chevron"), "')"),
      tags$i(id = ns("design-chevron"), class = "fa fa-chevron-down", style = "margin-right: 8px;"),
      tags$strong("Design Options")
    ),
    tags$div(
      id = ns("design-content"),
      style = "padding: 15px;",
      
      # Step 1: Optimization Framework
      tags$div(
        id = ns("step1"),
        tags$h5("Step 1: Optimization Framework", style = "color: #4A6B82; margin-bottom: 10px;"),
        tags$p("Include cost considerations in optimization?", style = "font-size: 12px; margin-bottom: 8px;"),
        selectInput(ns("optimization_type"), NULL,
                   choices = list(
                     "Power-only optimization" = "power_only",
                     "Power + cost optimization" = "power_cost"
                   ),
                   selected = "power_only"),
        style = "margin-bottom: 15px; padding-bottom: 10px; border-bottom: 1px solid #E3E6EA;"
      ),
      
      # Step 2: Minimization Target
      tags$div(
        id = ns("step2"),
        style = "display: none; margin-bottom: 15px; padding-bottom: 10px; border-bottom: 1px solid #E3E6EA;",
        tags$h5("Step 2: Optimization Target", style = "color: #4A6B82; margin-bottom: 10px;"),
        tags$p("What should we minimize?", style = "font-size: 12px; margin-bottom: 8px;"),
        selectInput(ns("minimization_target"), NULL,
                   choices = list(
                     "Minimize total cells per target" = "cells",
                     "Minimize reads per cell" = "reads", 
                     "Minimize total cost (cells x reads)" = "cost",
                     "Minimize TPM analysis threshold" = "tpm_threshold",
                     "Minimize minimum fold change" = "fold_change"
                   ),
                   selected = NULL),
        # Dynamic note about cost availability
        tags$div(id = ns("cost_note"), 
                style = "color: #6C757D; font-style: italic; font-size: 11px; margin-top: 5px; display: none;",
                "Note: Cost minimization not available with Power + cost optimization")
      ),
      
      # Step 3: Parameter Control
      tags$div(
        id = ns("step3"),
        style = "display: none;",
        tags$h5("Step 3: Parameter Control", style = "color: #4A6B82; margin-bottom: 10px;"),
        tags$p("Specify how each parameter should be handled:", style = "font-size: 12px; margin-bottom: 8px;"),
        
        # Cells per target
        tags$div(
          style = "margin-bottom: 8px;",
          tags$strong("Cells per target:", style = "font-size: 13px;"),
          selectInput(ns("cells_control"), NULL,
                     choices = list("Varying" = "varying", "Fixed" = "fixed", "Minimizing" = "minimizing"),
                     selected = "varying"),
          conditionalPanel(
            condition = paste0("input['", ns("cells_control"), "'] == 'fixed'"),
            numericInput(ns("cells_fixed"), "Fixed value:", value = 1000, min = 50, max = 5000, step = 50)
          )
        ),
        
        # Reads per cell  
        tags$div(
          style = "margin-bottom: 8px;",
          tags$strong("Reads per cell:", style = "font-size: 13px;"),
          selectInput(ns("reads_control"), NULL,
                     choices = list("Varying" = "varying", "Fixed" = "fixed", "Minimizing" = "minimizing"),
                     selected = "varying"),
          conditionalPanel(
            condition = paste0("input['", ns("reads_control"), "'] == 'fixed'"),
            numericInput(ns("reads_fixed"), "Fixed value:", value = 5000, min = 500, max = 20000, step = 500)
          )
        ),
        
        # TPM threshold
        tags$div(
          style = "margin-bottom: 8px;",
          tags$strong("TPM threshold:", style = "font-size: 13px;"),
          selectInput(ns("tpm_control"), NULL,
                     choices = list("Varying" = "varying", "Fixed" = "fixed", "Minimizing" = "minimizing"),
                     selected = "varying"),
          conditionalPanel(
            condition = paste0("input['", ns("tpm_control"), "'] == 'fixed'"),
            numericInput(ns("tpm_fixed"), "Fixed value:", value = 10, min = 0, max = 100, step = 1)
          )
        ),
        
        # Min fold change
        tags$div(
          style = "margin-bottom: 8px;",
          tags$strong("Min fold change:", style = "font-size: 13px;"),
          selectInput(ns("fc_control"), NULL,
                     choices = list("Varying" = "varying", "Fixed" = "fixed", "Minimizing" = "minimizing"),
                     selected = "varying"),
          conditionalPanel(
            condition = paste0("input['", ns("fc_control"), "'] == 'fixed'"),
            numericInput(ns("fc_fixed"), "Fixed value:", value = 1.5, min = 1.1, max = 10, step = 0.1)
          )
        ),
        
        # Business rule note
        tags$div(style = "color: #6C757D; font-style: italic; font-size: 11px; margin-top: 10px;",
                "Note: Only one parameter can be 'Minimizing' (auto-set from target selection)")
      )
    )
  )
}

#' Design Options Module Server
#'
#' @description Server logic for constraint-driven design options with progressive disclosure
#' and business rule enforcement
#'
#' @param id Module namespace ID
#'
#' @noRd
mod_design_options_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Progressive disclosure: Show steps sequentially
    observe({
      # Step 2 appears when optimization type is selected
      if (!is.null(input$optimization_type)) {
        shinyjs::show("step2")
      } else {
        shinyjs::hide("step2")
        shinyjs::hide("step3")
      }
    })
    
    observe({
      # Step 3 appears when minimization target is selected
      if (!is.null(input$minimization_target) && input$minimization_target != "") {
        shinyjs::show("step3")
      } else {
        shinyjs::hide("step3")
      }
    })
    
    # Business Logic: Update cost availability based on optimization type
    observe({
      if (!is.null(input$optimization_type) && input$optimization_type == "power_cost") {
        # Show cost restriction note
        shinyjs::show("cost_note")
        
        # Update choices to disable cost option
        updateSelectInput(session, "minimization_target",
                         choices = list(
                           "Minimize total cells per target" = "cells",
                           "Minimize reads per cell" = "reads", 
                           "Minimize TPM analysis threshold" = "tmp_threshold",
                           "Minimize minimum fold change" = "fold_change"
                         ))
      } else {
        shinyjs::hide("cost_note")
        
        # Include cost option
        updateSelectInput(session, "minimization_target",
                         choices = list(
                           "Minimize total cells per target" = "cells",
                           "Minimize reads per cell" = "reads", 
                           "Minimize total cost (cells x reads)" = "cost",
                           "Minimize TPM analysis threshold" = "tpm_threshold",
                           "Minimize minimum fold change" = "fold_change"
                         ))
      }
    })
    
    # Business Logic: Auto-set parameter to "Minimizing" when selected as target
    observe({
      target <- input$minimization_target
      
      if (!is.null(target) && target != "") {
        # Reset all parameters to varying first
        updateSelectInput(session, "cells_control", selected = "varying")
        updateSelectInput(session, "reads_control", selected = "varying")
        updateSelectInput(session, "tpm_control", selected = "varying")
        updateSelectInput(session, "fc_control", selected = "varying")
        
        # Set the selected target to minimizing
        if (target == "cells") {
          updateSelectInput(session, "cells_control", selected = "minimizing")
        } else if (target == "reads") {
          updateSelectInput(session, "reads_control", selected = "minimizing")
        } else if (target == "tpm_threshold") {
          updateSelectInput(session, "tpm_control", selected = "minimizing")
        } else if (target == "fold_change") {
          updateSelectInput(session, "fc_control", selected = "minimizing")
        }
      }
    })
    
    # Return structured design configuration
    design_config <- reactive({
      list(
        # Design Options
        optimization_type = input$optimization_type,
        minimization_target = input$minimization_target,
        parameter_controls = list(
          cells_per_target = list(
            type = input$cells_control %||% "varying",
            fixed_value = if(!is.null(input$cells_fixed)) input$cells_fixed else NULL
          ),
          reads_per_cell = list(
            type = input$reads_control %||% "varying",
            fixed_value = if(!is.null(input$reads_fixed)) input$reads_fixed else NULL
          ),
          tpm_threshold = list(
            type = input$tmp_control %||% "varying",
            fixed_value = if(!is.null(input$tmp_fixed)) input$tmp_fixed else NULL
          ),
          min_fold_change = list(
            type = input$fc_control %||% "varying",
            fixed_value = if(!is.null(input$fc_fixed)) input$fc_fixed else NULL
          )
        ),
        timestamp = Sys.time()
      )
    })
    
    return(design_config)
  })
}