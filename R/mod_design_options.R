#' design_options UI Function
#'
#' @description Creates the Design Options section with constraint-driven workflow:
#' Step 1: Optimization Framework, Step 2: Minimization Target, Step 3: Parameter Control
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList tags div strong selectInput numericInput conditionalPanel moduleServer observe reactive updateSelectInput
#' @importFrom shinyjs show hide html disable enable
mod_design_options_ui <- function(id) {
  ns <- NS(id)
  
  # Design Options (collapsible) - NEW CONSTRAINT-DRIVEN SECTION
  tagList(
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
          tags$h5("Step 1: Optimization Framework", style = "color: #4A6B82; margin-bottom: 10px; font-weight: bold;"),
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
          tags$h5("Step 2: Minimization Target", style = "color: #4A6B82; margin-bottom: 10px; font-weight: bold;"),
          selectInput(ns("minimization_target"), NULL,
                     choices = list(
                       "Select what to minimize..." = "",
                       "Minimize total cells per target" = "cells",
                       "Minimize reads per cell" = "reads", 
                       "Minimize total cost (cells x reads)" = "cost",
                       "Minimize TPM analysis threshold" = "tpm_threshold",
                       "Minimize minimum fold change" = "fold_change"
                     ),
                     selected = "")
        ),
        
        # Step 3: Parameter Control (initially hidden)
        tags$div(
          id = ns("step3"),
          style = "display: none;",
          tags$h5("Step 3: Power-determining Parameters Setup", style = "color: #4A6B82; margin-bottom: 10px; font-weight: bold;"),
          
          # Parameter controls - all parameters treated equally
          tags$div(
            id = ns("cells_control_div"),
            selectInput(ns("cells_control"), "Cells per target:",
                       choices = list("Varying" = "varying", "Fixed" = "fixed", "Minimizing" = "minimizing"),
                       selected = "varying"),
            conditionalPanel(
              condition = paste0("input['", ns("cells_control"), "'] == 'fixed'"),
              numericInput(ns("cells_fixed"), "Fixed value:", value = 1000, min = 50, max = 5000, step = 50)
            )
          ),
          
          tags$div(
            id = ns("reads_control_div"),
            selectInput(ns("reads_control"), "Reads per cell:",
                       choices = list("Varying" = "varying", "Fixed" = "fixed", "Minimizing" = "minimizing"),
                       selected = "varying"),
            conditionalPanel(
              condition = paste0("input['", ns("reads_control"), "'] == 'fixed'"),
              numericInput(ns("reads_fixed"), "Fixed value:", value = 5000, min = 500, max = 20000, step = 500)
            )
          ),
          
          tags$div(
            id = ns("tpm_control_div"),
            selectInput(ns("tpm_control"), "TPM threshold:",
                       choices = list("Varying" = "varying", "Fixed" = "fixed", "Minimizing" = "minimizing"),
                       selected = "varying"),
            conditionalPanel(
              condition = paste0("input['", ns("tpm_control"), "'] == 'fixed'"),
              numericInput(ns("tpm_fixed"), "Fixed value:", value = 10, min = 0, max = 100, step = 1)
            )
          ),
          
          tags$div(
            id = ns("fc_control_div"),
            selectInput(ns("fc_control"), "Minimum fold change:",
                       choices = list("Varying" = "varying", "Fixed" = "fixed", "Minimizing" = "minimizing"),
                       selected = "varying"),
            conditionalPanel(
              condition = paste0("input['", ns("fc_control"), "'] == 'fixed'"),
              numericInput(ns("fc_fixed"), "Fixed value:", value = 1.5, min = 1.1, max = 10, step = 0.1)
            )
          )
        )
      )
    )
  )
}
    
#' design_options Server Functions
#'
#' @noRd 
mod_design_options_server <- function(id){
  moduleServer(id, function(input, output, session){
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
        # Update choices to disable cost option
        # Power+cost: Only TPM and FC can be minimized
        updateSelectInput(session, "minimization_target",
                         choices = list(
                           "Select what to minimize..." = "",
                           "Minimize TPM analysis threshold" = "tpm_threshold",
                           "Minimize minimum fold change" = "fold_change"
                         ))
        
        # Update cells/reads dropdowns to remove "Minimizing" option for Power+Cost
        updateSelectInput(session, "cells_control",
                         choices = list("Varying" = "varying", "Fixed" = "fixed"))
        updateSelectInput(session, "reads_control", 
                         choices = list("Varying" = "varying", "Fixed" = "fixed"))
      } else {
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
        
        # Restore full choices for cells/reads dropdowns when not Power+Cost
        updateSelectInput(session, "cells_control",
                         choices = list("Varying" = "varying", "Fixed" = "fixed", "Minimizing" = "minimizing"))
        updateSelectInput(session, "reads_control", 
                         choices = list("Varying" = "varying", "Fixed" = "fixed", "Minimizing" = "minimizing"))
      }
    })
    
    # Business Logic: Show minimizing parameter and control other parameters
    observe({
      target <- input$minimization_target
      
      if (!is.null(target) && target != "") {
        # Set the selected parameter to "Minimizing"
        if (target == "cells") {
          updateSelectInput(session, "cells_control", selected = "minimizing")
          updateSelectInput(session, "reads_control", selected = "varying")
          updateSelectInput(session, "tpm_control", selected = "varying")
          updateSelectInput(session, "fc_control", selected = "varying")
        } else if (target == "reads") {
          updateSelectInput(session, "cells_control", selected = "varying")
          updateSelectInput(session, "reads_control", selected = "minimizing")
          updateSelectInput(session, "tpm_control", selected = "varying")
          updateSelectInput(session, "fc_control", selected = "varying")
        } else if (target == "cost") {
          # For cost minimization: cells/reads = varying, tpm/fc = fixed
          updateSelectInput(session, "cells_control", selected = "varying")
          updateSelectInput(session, "reads_control", selected = "varying")
          updateSelectInput(session, "tpm_control", selected = "fixed")
          updateSelectInput(session, "fc_control", selected = "fixed")
        } else if (target == "tpm_threshold") {
          updateSelectInput(session, "cells_control", selected = "varying")
          updateSelectInput(session, "reads_control", selected = "varying")
          updateSelectInput(session, "tpm_control", selected = "minimizing")
          updateSelectInput(session, "fc_control", selected = "varying")
        } else if (target == "fold_change") {
          updateSelectInput(session, "cells_control", selected = "varying")
          updateSelectInput(session, "reads_control", selected = "varying")
          updateSelectInput(session, "tpm_control", selected = "varying")
          updateSelectInput(session, "fc_control", selected = "minimizing")
        }
      }
    })
    
    # PRD Business Rule: Power-only + non-cost minimization = all other params must be Fixed
    observe({
      opt_type <- input$optimization_type
      target <- input$minimization_target
      
      # Only apply rule if both inputs are available
      if (!is.null(opt_type) && !is.null(target) && opt_type != "" && target != "") {
        
        # PRD Rule: Power-only optimization + non-cost minimization target
        if (opt_type == "power_only" && target %in% c("cells", "reads", "tpm_threshold", "fold_change")) {
          
          # Force all non-minimizing parameters to be "Fixed"
          if (target != "cells") {
            updateSelectInput(session, "cells_control", selected = "fixed")
          }
          if (target != "reads") {
            updateSelectInput(session, "reads_control", selected = "fixed")
          }
          if (target != "tpm_threshold") {
            updateSelectInput(session, "tpm_control", selected = "fixed")
          }
          if (target != "fold_change") {
            updateSelectInput(session, "fc_control", selected = "fixed")
          }
          
          # Disable all dropdowns (non-clickable)
          shinyjs::disable("cells_control")
          shinyjs::disable("reads_control")
          shinyjs::disable("tpm_control")
          shinyjs::disable("fc_control")
        } else if (target == "cost") {
          # Cost minimization = all parameters disabled (cells/reads varying, tpm/fc fixed)
          shinyjs::disable("cells_control")
          shinyjs::disable("reads_control")
          shinyjs::disable("tpm_control")
          shinyjs::disable("fc_control")
        } else if (opt_type == "power_cost" && target == "tpm_threshold") {
          # Power+cost + TPM minimization: TPM=minimizing+disabled, FC=fixed+disabled, cells/reads=varying/fixed with constraint
          updateSelectInput(session, "tpm_control", selected = "minimizing")
          updateSelectInput(session, "fc_control", selected = "fixed")
          shinyjs::disable("tpm_control")
          shinyjs::disable("fc_control")
          # Cells/reads: varying/fixed but not both fixed (handled by cells/reads constraint logic)
          shinyjs::enable("cells_control")
          shinyjs::enable("reads_control")
        } else if (opt_type == "power_cost" && target == "fold_change") {
          # Power+cost + FC minimization: FC=minimizing+disabled, TPM=fixed+disabled, cells/reads=varying/fixed with constraint
          updateSelectInput(session, "fc_control", selected = "minimizing")
          updateSelectInput(session, "tpm_control", selected = "fixed")
          shinyjs::disable("fc_control")
          shinyjs::disable("tpm_control")
          # Cells/reads: varying/fixed but not both fixed (handled by cells/reads constraint logic)
          shinyjs::enable("cells_control")
          shinyjs::enable("reads_control")
        } else {
          # Re-enable all dropdowns for other scenarios
          shinyjs::enable("cells_control")
          shinyjs::enable("reads_control")
          shinyjs::enable("tpm_control")
          shinyjs::enable("fc_control")
        }
      }
    })
    
    # Cells/reads constraint logic for Power+Cost optimization
    observe({
      opt_type <- input$optimization_type
      target <- input$minimization_target
      
      # Only apply constraint for Power+Cost optimization with TPM or FC minimization
      if (!is.null(opt_type) && !is.null(target) && 
          opt_type == "power_cost" && target %in% c("tpm_threshold", "fold_change")) {
        
        # When cells is set to fixed, auto-set reads to varying and disable it
        if (!is.null(input$cells_control) && input$cells_control == "fixed") {
          updateSelectInput(session, "reads_control", selected = "varying")
          shinyjs::disable("reads_control")
        }
        
        # When reads is set to fixed, auto-set cells to varying and disable it
        if (!is.null(input$reads_control) && input$reads_control == "fixed") {
          updateSelectInput(session, "cells_control", selected = "varying")
          shinyjs::disable("cells_control")
        }
        
        # When cells or reads is set back to varying, enable the other
        if (!is.null(input$cells_control) && input$cells_control == "varying") {
          shinyjs::enable("reads_control")
        }
        if (!is.null(input$reads_control) && input$reads_control == "varying") {
          shinyjs::enable("cells_control")
        }
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
          tmp_threshold = list(
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
    
## To be copied in the UI
# mod_design_options_ui("design_options_1")
    
## To be copied in the server
# mod_design_options_server("design_options_1")
