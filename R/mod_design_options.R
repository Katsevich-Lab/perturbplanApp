#' Design Options UI Function
#'
#' @description Core module for constraint-driven experimental design.
#' Provides 3-section interface for optimization framework, minimization targets,
#' and parameter control matrix.
#'
#' @param id Module namespace ID
#'
#' @noRd
#'
#' @importFrom shiny NS tagList h3 h4 h5 p div radioButtons checkboxGroupInput
#' @importFrom shiny fluidRow column wellPanel textInput conditionalPanel
#' @importFrom shiny updateRadioButtons updateCheckboxGroupInput moduleServer
#' @importFrom shiny observe req reactive reactiveValues
mod_design_options_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h3("Design Options", class = "module-header"),
    
    # Section 1: Optimization Framework
    wellPanel(
      h4("1. Optimization Framework"),
      p("Include cost considerations in optimization?"),
      radioButtons(
        inputId = ns("optimization_type"),
        label = NULL,
        choices = list(
          "Power-only optimization: Optimize power without cost constraints" = "power_only",
          "Power + cost optimization: Optimize power while considering cost constraints" = "power_cost"
        ),
        selected = "power_only"
      )
    ),
    
    # Section 2: Minimization Target
    wellPanel(
      h4("2. Optimization Target"),
      p("What should we minimize?"),
      checkboxGroupInput(
        inputId = ns("minimization_target"),
        label = NULL,
        choices = list(
          "Minimize total cells per target" = "cells",
          "Minimize reads per cell" = "reads", 
          "Minimize total cost (computed from cells × reads)" = "cost",
          "Minimize TPM analysis threshold (maximize gene coverage)" = "tpm_threshold",
          "Minimize minimum fold change (maximize sensitivity)" = "fold_change"
        ),
        selected = NULL
      ),
      # Dynamic note about cost availability
      div(id = ns("cost_note"), 
          style = "color: #666; font-style: italic; margin-top: 10px;",
          "Note: 'Total cost' optimization target not available when 'Power + cost' is selected (cost already considered as constraint)")
    ),
    
    # Section 3: Parameter Control Matrix
    wellPanel(
      h4("3. Parameter Control"),
      p("For each parameter, specify how it should be handled in the optimization:"),
      
      # Simplified parameter control for now - will enhance in next iteration
      div(class = "parameter-control-matrix",
        h5("Cells per target:"),
        radioButtons(ns("cells_control"), NULL, 
                    choices = list("Varying" = "varying", "Fixed" = "fixed", "Minimizing" = "minimizing"), 
                    selected = "varying", inline = TRUE),
        conditionalPanel(
          condition = paste0("input['", ns("cells_control"), "'] == 'fixed'"),
          textInput(ns("cells_fixed_1"), "Fixed value 1:", placeholder = "e.g., 1000")
        ),
        
        h5("Reads per cell:"),
        radioButtons(ns("reads_control"), NULL, 
                    choices = list("Varying" = "varying", "Fixed" = "fixed", "Minimizing" = "minimizing"), 
                    selected = "varying", inline = TRUE),
        conditionalPanel(
          condition = paste0("input['", ns("reads_control"), "'] == 'fixed'"),
          textInput(ns("reads_fixed_1"), "Fixed value 1:", placeholder = "e.g., 5000")
        ),
        
        h5("TPM threshold:"),
        radioButtons(ns("tpm_control"), NULL, 
                    choices = list("Varying" = "varying", "Fixed" = "fixed", "Minimizing" = "minimizing"), 
                    selected = "varying", inline = TRUE),
        conditionalPanel(
          condition = paste0("input['", ns("tmp_control"), "'] == 'fixed'"),
          textInput(ns("tpm_fixed_1"), "Fixed value 1:", placeholder = "e.g., 10")
        ),
        
        h5("Min fold change:"),
        radioButtons(ns("fc_control"), NULL, 
                    choices = list("Varying" = "varying", "Fixed" = "fixed", "Minimizing" = "minimizing"), 
                    selected = "varying", inline = TRUE),
        conditionalPanel(
          condition = paste0("input['", ns("fc_control"), "'] == 'fixed'"),
          textInput(ns("fc_fixed_1"), "Fixed value 1:", placeholder = "e.g., 1.5")
        )
      ),
      
      # Business rule note
      div(style = "color: #666; font-style: italic; margin-top: 15px;",
          "Note: Only one parameter can be 'Minimizing' (auto-set from target selection). All other parameters are grayed out when one is 'Minimizing'.")
    ),
    
    # Validation summary
    div(id = ns("validation_summary"), 
        class = "alert alert-info",
        style = "margin-top: 20px;",
        "Select optimization objective and minimization target to begin...")
  )
}

#' Design Options Server Function
#'
#' @description Server logic for design options module.
#' Handles business rules, parameter control logic, and returns design configuration.
#'
#' @param id Module namespace ID
#'
#' @return Reactive list containing design configuration
#'
#' @noRd
mod_design_options_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Reactive values for tracking state
    values <- reactiveValues(
      current_minimizing = NULL,
      validation_errors = character()
    )
    
    # Business Logic: Auto-set parameter to "Minimizing" when selected as target
    observe({
      target <- input$minimization_target
      
      if (length(target) == 1) {
        # Single target selected - auto-set corresponding parameter
        values$current_minimizing <- target
        
        # Update parameter control based on target
        if (target == "cells") {
          updateRadioButtons(session, "cells_control", selected = "minimizing")
        } else if (target == "reads") {
          updateRadioButtons(session, "reads_control", selected = "minimizing")
        } else if (target == "tpm_threshold") {
          updateRadioButtons(session, "tmp_control", selected = "minimizing")
        } else if (target == "fold_change") {
          updateRadioButtons(session, "fc_control", selected = "minimizing")
        }
        
      } else if (length(target) == 0) {
        # No target selected - reset all to varying
        values$current_minimizing <- NULL
        updateRadioButtons(session, "cells_control", selected = "varying")
        updateRadioButtons(session, "reads_control", selected = "varying")
        updateRadioButtons(session, "tmp_control", selected = "varying")
        updateRadioButtons(session, "fc_control", selected = "varying")
      } else {
        # Multiple targets selected - not allowed, keep only first
        updateCheckboxGroupInput(session, "minimization_target", selected = target[1])
      }
    })
    
    # Validation logic
    observe({
      errors <- character()
      
      # Check if at least one target is selected
      if (is.null(input$minimization_target) || length(input$minimization_target) == 0) {
        errors <- c(errors, "Please select one minimization target")
      }
      
      # Check for multiple targets
      if (length(input$minimization_target) > 1) {
        errors <- c(errors, "Only one minimization target allowed")
      }
      
      # Check cost target restriction
      if (!is.null(input$optimization_type) && input$optimization_type == "power_cost" && 
          !is.null(input$minimization_target) && "cost" %in% input$minimization_target) {
        errors <- c(errors, "Cost minimization not available with Power + cost optimization")
      }
      
      values$validation_errors <- errors
    })
    
    # Update validation summary display
    observe({
      if (length(values$validation_errors) == 0 && 
          !is.null(input$minimization_target) && length(input$minimization_target) == 1) {
        # Valid configuration
        target_name <- switch(input$minimization_target,
                             "cells" = "cells per target",
                             "reads" = "reads per cell", 
                             "cost" = "total cost",
                             "tmp_threshold" = "TPM threshold",
                             "fold_change" = "fold change")
        
        message <- paste0("✓ Configuration valid: Minimizing ", target_name, 
                         " with ", input$optimization_type, " optimization")
      } else if (length(values$validation_errors) > 0) {
        # Has errors
        message <- paste("⚠ Issues:", paste(values$validation_errors, collapse = "; "))
      } else {
        # Incomplete
        message <- "Select optimization objective and minimization target to begin..."
      }
      
      # Update the div content (note: this is a simplified approach)
      session$sendCustomMessage("updateValidation", list(id = ns("validation_summary"), message = message))
    })
    
    # Return design configuration reactive
    design_config <- reactive({
      req(input$optimization_type, input$minimization_target)
      
      # Return structured configuration
      list(
        optimization_type = input$optimization_type,
        minimization_target = input$minimization_target,
        parameter_controls = list(
          cells_per_target = list(
            type = input$cells_control %||% "varying",
            fixed_values = if(!is.null(input$cells_fixed_1) && input$cells_fixed_1 != "") input$cells_fixed_1 else NULL
          ),
          reads_per_cell = list(
            type = input$reads_control %||% "varying",
            fixed_values = if(!is.null(input$reads_fixed_1) && input$reads_fixed_1 != "") input$reads_fixed_1 else NULL
          ),
          tmp_threshold = list(
            type = input$tpm_control %||% "varying",
            fixed_values = if(!is.null(input$tpm_fixed_1) && input$tpm_fixed_1 != "") input$tmp_fixed_1 else NULL
          ),
          min_fold_change = list(
            type = input$fc_control %||% "varying",
            fixed_values = if(!is.null(input$fc_fixed_1) && input$fc_fixed_1 != "") input$fc_fixed_1 else NULL
          )
        ),
        validation_status = list(
          is_valid = length(values$validation_errors) == 0,
          errors = values$validation_errors
        ),
        timestamp = Sys.time()
      )
    })
    
    # Return the reactive configuration
    return(design_config)
  })
}
    
## To be copied in the UI
# mod_design_options_ui("design_options_1")
    
## To be copied in the server
# mod_design_options_server("design_options_1")
