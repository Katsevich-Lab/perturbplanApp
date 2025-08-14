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
                     "Select optimization type..." = "",
                     "Power-only optimization" = "power_only",
                     "Power + cost optimization" = "power_cost"
                   ),
                   selected = ""),
        style = "margin-bottom: 15px; padding-bottom: 10px; border-bottom: 1px solid #E3E6EA;"
      ),
      
      # Step 2: Minimization Target (initially hidden)
      tags$div(
        id = ns("step2"),
        style = "display: none; margin-bottom: 15px; padding-bottom: 10px; border-bottom: 1px solid #E3E6EA;",
        tags$h5("Step 2: Optimization Target", style = "color: #4A6B82; margin-bottom: 10px;"),
        tags$p("What should we minimize?", style = "font-size: 12px; margin-bottom: 8px;"),
        selectInput(ns("minimization_target"), NULL,
                   choices = list(
                     "Select what to minimize..." = "",
                     "Minimize total cells per target" = "cells",
                     "Minimize reads per cell" = "reads", 
                     "Minimize total cost (cells x reads)" = "cost",
                     "Minimize TPM analysis threshold" = "tpm_threshold",
                     "Minimize minimum fold change" = "fold_change"
                   ),
                   selected = ""),
        # Dynamic note about cost availability
        tags$div(id = ns("cost_note"), 
                style = "color: #6C757D; font-style: italic; font-size: 11px; margin-top: 5px; display: none;",
                "Note: Cost minimization not available with Power + cost optimization")
      ),
      
      # Step 3: Parameter Control (initially hidden)
      tags$div(
        id = ns("step3"),
        style = "display: none;",
        tags$h5("Step 3: Parameter Control", style = "color: #4A6B82; margin-bottom: 10px;"),
        tags$p("Specify how each parameter should be handled:", style = "font-size: 12px; margin-bottom: 8px;"),
        
        # Show the selected minimization target parameter as minimizing
        tags$div(
          id = ns("minimizing_parameter"),
          style = "display: none; margin-bottom: 12px; padding: 8px; background-color: #E8F4FD; border-radius: 4px; border-left: 4px solid #2E5D8A;",
          tags$strong(id = ns("minimizing_param_name"), "Parameter:", style = "font-size: 13px; color: #2E5D8A;"),
          tags$div("Status: Minimizing", style = "color: #2E5D8A; font-weight: bold; margin-top: 3px; font-size: 12px;")
        ),
        
        # Cells per target
        tags$div(
          id = ns("cells_control_div"),
          style = "margin-bottom: 8px;",
          tags$strong("Cells per target:", style = "font-size: 13px;"),
          selectInput(ns("cells_control"), NULL,
                     choices = list("Varying" = "varying", "Fixed" = "fixed"),
                     selected = "varying"),
          conditionalPanel(
            condition = paste0("input['", ns("cells_control"), "'] == 'fixed'"),
            numericInput(ns("cells_fixed"), "Fixed value:", value = 1000, min = 50, max = 5000, step = 50)
          )
        ),
        
        # Reads per cell  
        tags$div(
          id = ns("reads_control_div"),
          style = "margin-bottom: 8px;",
          tags$strong("Reads per cell:", style = "font-size: 13px;"),
          selectInput(ns("reads_control"), NULL,
                     choices = list("Varying" = "varying", "Fixed" = "fixed"),
                     selected = "varying"),
          conditionalPanel(
            condition = paste0("input['", ns("reads_control"), "'] == 'fixed'"),
            numericInput(ns("reads_fixed"), "Fixed value:", value = 5000, min = 500, max = 20000, step = 500)
          )
        ),
        
        # TPM threshold
        tags$div(
          id = ns("tpm_control_div"),
          style = "margin-bottom: 8px;",
          tags$strong("TPM threshold:", style = "font-size: 13px;"),
          selectInput(ns("tpm_control"), NULL,
                     choices = list("Varying" = "varying", "Fixed" = "fixed"),
                     selected = "varying"),
          conditionalPanel(
            condition = paste0("input['", ns("tpm_control"), "'] == 'fixed'"),
            numericInput(ns("tpm_fixed"), "Fixed value:", value = 10, min = 0, max = 100, step = 1)
          )
        ),
        
        # Min fold change
        tags$div(
          id = ns("fc_control_div"),
          style = "margin-bottom: 8px;",
          tags$strong("Min fold change:", style = "font-size: 13px;"),
          selectInput(ns("fc_control"), NULL,
                     choices = list("Varying" = "varying", "Fixed" = "fixed"),
                     selected = "varying"),
          conditionalPanel(
            condition = paste0("input['", ns("fc_control"), "'] == 'fixed'"),
            numericInput(ns("fc_fixed"), "Fixed value:", value = 1.5, min = 1.1, max = 10, step = 0.1)
          )
        ),
        
        # Business rule note
        tags$div(style = "color: #6C757D; font-style: italic; font-size: 11px; margin-top: 10px;",
                "Note: The minimizing parameter is automatically set based on your target selection above.")
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
      if (!is.null(input$optimization_type) && input$optimization_type != "") {
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
                           "Select what to minimize..." = "",
                           "Minimize total cells per target" = "cells",
                           "Minimize reads per cell" = "reads", 
                           "Minimize TPM analysis threshold" = "tpm_threshold",
                           "Minimize minimum fold change" = "fold_change"
                         ))
      } else {
        shinyjs::hide("cost_note")
        
        # Include cost option
        updateSelectInput(session, "minimization_target",
                         choices = list(
                           "Select what to minimize..." = "",
                           "Minimize total cells per target" = "cells",
                           "Minimize reads per cell" = "reads", 
                           "Minimize total cost (cells x reads)" = "cost",
                           "Minimize TPM analysis threshold" = "tpm_threshold",
                           "Minimize minimum fold change" = "fold_change"
                         ))
      }
    })
    
    # Business Logic: Show minimizing parameter and control other parameters
    observe({
      target <- input$minimization_target
      
      if (!is.null(target) && target != "") {
        # Show the minimizing parameter display
        shinyjs::show("minimizing_parameter")
        
        # Hide all parameter control divs first
        shinyjs::hide("cells_control_div")
        shinyjs::hide("reads_control_div")
        shinyjs::hide("tpm_control_div")
        shinyjs::hide("fc_control_div")
        
        # Show the minimizing parameter name and show other parameter controls
        if (target == "cells") {
          shinyjs::html("minimizing_param_name", "Cells per target:")
          shinyjs::show("reads_control_div")
          shinyjs::show("tpm_control_div")
          shinyjs::show("fc_control_div")
        } else if (target == "reads") {
          shinyjs::html("minimizing_param_name", "Reads per cell:")
          shinyjs::show("cells_control_div")
          shinyjs::show("tpm_control_div")
          shinyjs::show("fc_control_div")
        } else if (target == "cost") {
          shinyjs::html("minimizing_param_name", "Total cost:")
          shinyjs::show("tpm_control_div")
          shinyjs::show("fc_control_div")
          # Note: For cost minimization, both cells and reads are varying (handled in return config)
        } else if (target == "tpm_threshold") {
          shinyjs::html("minimizing_param_name", "TPM threshold:")
          shinyjs::show("cells_control_div")
          shinyjs::show("reads_control_div")
          shinyjs::show("fc_control_div")
        } else if (target == "fold_change") {
          shinyjs::html("minimizing_param_name", "Min fold change:")
          shinyjs::show("cells_control_div")
          shinyjs::show("reads_control_div")
          shinyjs::show("tpm_control_div")
        }
      } else {
        # Hide minimizing parameter display
        shinyjs::hide("minimizing_parameter")
      }
    })
    
    # Return structured design configuration
    design_config <- reactive({
      
      # Safe access to input values with NULL checking
      target <- input$minimization_target %||% ""
      
      list(
        # Design Options
        optimization_type = input$optimization_type,
        minimization_target = input$minimization_target,
        parameter_controls = list(
          cells_per_target = list(
            type = if(!is.null(target) && target == "cells") "minimizing" 
                   else if(!is.null(target) && target == "cost") "varying"
                   else (input$cells_control %||% "varying"),
            fixed_value = if(!is.null(input$cells_fixed)) input$cells_fixed else NULL
          ),
          reads_per_cell = list(
            type = if(!is.null(target) && target == "reads") "minimizing" 
                   else if(!is.null(target) && target == "cost") "varying"
                   else (input$reads_control %||% "varying"),
            fixed_value = if(!is.null(input$reads_fixed)) input$reads_fixed else NULL
          ),
          tpm_threshold = list(
            type = if(!is.null(target) && target == "tpm_threshold") "minimizing" else (input$tpm_control %||% "varying"),
            fixed_value = if(!is.null(input$tpm_fixed)) input$tpm_fixed else NULL
          ),
          min_fold_change = list(
            type = if(!is.null(target) && target == "fold_change") "minimizing" else (input$fc_control %||% "varying"),
            fixed_value = if(!is.null(input$fc_fixed)) input$fc_fixed else NULL
          )
        ),
        timestamp = Sys.time()
      )
    })
    
    return(design_config)
  })
}