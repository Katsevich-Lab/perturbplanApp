#' Sidebar UI Module
#'
#' @description Creates the left sidebar with constraint-driven parameter inputs
#' using the exact same styling as the original perturbplan app
#'
#' @param id Module namespace ID
#'
#' @noRd
#'
#' @importFrom shiny NS tagList tags div strong selectInput numericInput
#' @importFrom shiny conditionalPanel fileInput htmlOutput
#' @importFrom shinydashboard dashboardSidebar
mod_sidebar_ui <- function(id) {
  ns <- NS(id)
  
  dashboardSidebar(
    # Parameter panels - make scrollable with collapsible sections
    tags$div(
      style = "padding: 10px; max-height: 90vh; overflow-y: auto;",
      
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
                         "Minimize total cost (cells × reads)" = "cost",
                         "Minimize TPM analysis threshold" = "tmp_threshold",
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
              selectInput(ns("tmp_control"), NULL,
                         choices = list("Varying" = "varying", "Fixed" = "fixed", "Minimizing" = "minimizing"),
                         selected = "varying"),
              conditionalPanel(
                condition = paste0("input['", ns("tmp_control"), "'] == 'fixed'"),
                numericInput(ns("tmp_fixed"), "Fixed value:", value = 10, min = 0, max = 100, step = 1)
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
      ),
      
      # Experimental setup (collapsible) - ADAPTED FROM ORIGINAL
      tags$div(
        style = "border-radius: 4px; margin-bottom: 5px;",
        tags$div(
          id = ns("exp-header"),
          style = "padding: 10px 15px; cursor: pointer; border-radius: 4px 4px 0 0;",
          onclick = paste0("toggleSection('", ns("experimental-content"), "', '", ns("exp-chevron"), "')"),
          tags$i(id = ns("exp-chevron"), class = "fa fa-chevron-right", style = "margin-right: 8px;"),
          tags$strong("Experimental setup")
        ),
        tags$div(
          id = ns("experimental-content"),
          style = "padding: 15px;",
          selectInput(ns("biological_system"), "Biological system:", c("K562", "Other")),
          selectInput(ns("experimental_platform"), "Experimental platform:", c("10x Chromium v3", "Other")),
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
          )
        )
      ),
      
      # Analysis choices (collapsible) - ADAPTED FROM ORIGINAL (MINUS TPM/FC CONTROLS)
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
          numericInput(ns("fdr_target"), "FDR target level:", 0.05, 0.001, 0.1, 0.001)
        )
      ),
      
      # Assumed effect sizes (collapsible) - FROM ORIGINAL
      tags$div(
        style = "border-radius: 4px; margin-bottom: 5px;",
        tags$div(
          id = ns("effects-header"),
          style = "padding: 10px 15px; cursor: pointer; border-radius: 4px 4px 0 0;",
          onclick = paste0("toggleSection('", ns("effects-content"), "', '", ns("effects-chevron"), "')"),
          tags$i(id = ns("effects-chevron"), class = "fa fa-chevron-right", style = "margin-right: 8px;"),
          tags$strong("Assumed effect sizes")
        ),
        tags$div(
          id = ns("effects-content"),
          style = "padding: 15px;",
          numericInput(ns("fc_mean"), "Minimum fold change:", 0.85, 1.1, 10, 0.05),
          numericInput(ns("fc_sd"), "gRNA variability:", 0.15, 0.1, 5, 0.05),
          numericInput(ns("prop_non_null"), "Proportion of non-null pairs:", 0.1, 0, 1, 0.01)
        )
      ),
      
      # Horizontal separator line
      tags$hr(class = "sidebar-separator"),
      
      # Plan button
      tags$div(
        style = "text-align: center; padding: 0 20px;",
        actionButton(ns("plan_btn"), "Plan", class = "btn-success", style = "width: 200px; max-width: 90%;")
      )
    )
  )
}

#' Sidebar Server Module
#'
#' @description Server logic for sidebar parameter inputs and progressive disclosure
#'
#' @param id Module namespace ID
#'
#' @noRd
mod_sidebar_server <- function(id) {
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
                           "Minimize total cost (cells × reads)" = "cost",
                           "Minimize TPM analysis threshold" = "tmp_threshold",
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
        updateSelectInput(session, "tmp_control", selected = "varying")
        updateSelectInput(session, "fc_control", selected = "varying")
        
        # Set the selected target to minimizing
        if (target == "cells") {
          updateSelectInput(session, "cells_control", selected = "minimizing")
        } else if (target == "reads") {
          updateSelectInput(session, "reads_control", selected = "minimizing")
        } else if (target == "tmp_threshold") {
          updateSelectInput(session, "tmp_control", selected = "minimizing")
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
          tmp_threshold = list(
            type = input$tmp_control %||% "varying",
            fixed_value = if(!is.null(input$tmp_fixed)) input$tmp_fixed else NULL
          ),
          min_fold_change = list(
            type = input$fc_control %||% "varying",
            fixed_value = if(!is.null(input$fc_fixed)) input$fc_fixed else NULL
          )
        ),
        
        # Experimental Setup
        biological_system = input$biological_system,
        experimental_platform = input$experimental_platform,
        pilot_data_choice = input$pilot_data_choice,
        
        # Analysis Choices
        gene_list_mode = input$gene_list_mode,
        side = input$side,
        control_group = input$control_group,
        fdr_target = input$fdr_target,
        
        # Effect Sizes
        fc_mean = input$fc_mean,
        fc_sd = input$fc_sd,
        prop_non_null = input$prop_non_null,
        
        timestamp = Sys.time()
      )
    })
    
    return(design_config)
  })
}