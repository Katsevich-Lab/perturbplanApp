#' design_options UI Function
#'
#' @description Creates the Design Options section with constraint-driven workflow:
#' Step 1: Optimization Framework, Step 2: Minimization Target, Step 3: Parameter Control
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList tags div strong selectInput numericInput conditionalPanel moduleServer observe reactive updateSelectInput updateNumericInput renderUI uiOutput
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
        tags$strong("Design problem")
      ),
      tags$div(
        id = ns("design-content"),
        style = "padding: 15px;",
        
        # Step 1: Optimization Constraints
        tags$div(
          id = ns("step1"),
          tags$h5("Step 1: Optimization Constraints", style = "color: #4A6B82; margin-bottom: 10px; font-weight: bold;"),
          selectInput(ns("optimization_type"), NULL,
                     choices = list(
                       "Select constraint type..." = "",
                       "Constrain power only" = "power_only",
                       "Constrain power and cost" = "power_cost"
                     ),
                     selected = ""),
          style = "margin-bottom: 15px; padding-bottom: 10px; border-bottom: 1px solid #E3E6EA;"
        ),
        
        # Power and Cost Requirements (initially hidden)
        tags$div(
          id = ns("power_cost_inputs"),
          style = "display: none; margin-bottom: 15px; padding-bottom: 10px; border-bottom: 1px solid #E3E6EA;",
          
          # Target Power Input (always shown when constraint type is selected)
          tags$div(
            tags$h6("Target Power:", style = "color: #4A6B82; margin-bottom: 5px; font-weight: bold;"),
            numericInput(ns("target_power"), NULL, 
                        value = 0.8, min = 0.1, max = 0.99, step = 0.05)
          ),
          
          # Cost Budget Input (only shown for power + cost constraints)
          tags$div(
            id = ns("cost_budget_div"),
            style = "display: none; margin-top: 10px;",
            tags$h6("Cost Budget ($):", style = "color: #4A6B82; margin-bottom: 5px; font-weight: bold;"),
            numericInput(ns("cost_budget"), NULL, 
                        value = 10000, min = 100, max = 1000000, step = 500),
            
            # Cost parameters (below cost budget)
            tags$div(
              style = "margin-top: 15px;",
              tags$h6("Cost Parameters:", style = "color: #4A6B82; margin-bottom: 10px; font-weight: bold;"),
              
              # Two column layout for cost inputs
              tags$div(
                style = "display: flex; gap: 15px;",
                
                # Cost per cell column
                tags$div(
                  style = "flex: 1;",
                  tags$label("Cost per cell:", style = "font-weight: normal; margin-bottom: 5px; display: block;"),
                  tags$div(
                    class = "cost-input-container",
                    style = "position: relative;",
                    tags$span("$", style = "position: absolute; left: 8px; top: 50%; transform: translateY(-50%); color: #666; z-index: 10;"),
                    numericInput(ns("cost_per_cell"), 
                                label = NULL,
                                value = 0.086, 
                                min = 0, 
                                step = 0.001,
                                width = "100%")
                  )
                ),
                
                # Cost per million reads column  
                tags$div(
                  style = "flex: 1;",
                  tags$label("Cost per million reads:", style = "font-weight: normal; margin-bottom: 5px; display: block;"),
                  tags$div(
                    class = "cost-input-container",
                    style = "position: relative;",
                    tags$span("$", style = "position: absolute; left: 8px; top: 50%; transform: translateY(-50%); color: #666; z-index: 10;"),
                    numericInput(ns("cost_per_million_reads"), 
                                label = NULL,
                                value = 0.374, 
                                min = 0, 
                                step = 0.001,
                                width = "100%")
                  )
                )
              )
            )
          )
        ),
        
        # Step 2: Minimization Target (initially hidden)
        tags$div(
          id = ns("step2"),
          style = "display: none; margin-bottom: 15px; padding-bottom: 10px; border-bottom: 1px solid #E3E6EA;",
          tags$h5("Step 2: Minimization Target", style = "color: #4A6B82; margin-bottom: 10px; font-weight: bold;"),
          selectInput(ns("minimization_target"), NULL,
                     choices = list(
                       "Select what to minimize..." = "",
                       "Cells per target" = "cells_per_target",
                       "Reads per cell" = "reads_per_cell", 
                       "Total cost" = "cost",
                       "TPM analysis threshold" = "TPM_threshold",
                       "Fold change" = "minimum_fold_change"
                     ),
                     selected = ""),
          
          # Cost parameters for cost minimization (only shown when minimizing total cost)
          tags$div(
            id = ns("cost_minimization_params"),
            style = "display: none; margin-top: 15px;",
            tags$h6("Cost Parameters:", style = "color: #4A6B82; margin-bottom: 10px; font-weight: bold;"),
            
            # Two column layout for cost inputs
            tags$div(
              style = "display: flex; gap: 15px;",
              
              # Cost per cell column
              tags$div(
                style = "flex: 1;",
                tags$label("Cost per cell:", style = "font-weight: normal; margin-bottom: 5px; display: block;"),
                tags$div(
                  class = "cost-input-container",
                  style = "position: relative;",
                  tags$span("$", style = "position: absolute; left: 8px; top: 50%; transform: translateY(-50%); color: #666; z-index: 10;"),
                  numericInput(ns("cost_per_cell_min"), 
                              label = NULL,
                              value = 0.086, 
                              min = 0, 
                              step = 0.001,
                              width = "100%")
                )
              ),
              
              # Cost per million reads column  
              tags$div(
                style = "flex: 1;",
                tags$label("Cost per million reads:", style = "font-weight: normal; margin-bottom: 5px; display: block;"),
                tags$div(
                  class = "cost-input-container",
                  style = "position: relative;",
                  tags$span("$", style = "position: absolute; left: 8px; top: 50%; transform: translateY(-50%); color: #666; z-index: 10;"),
                  numericInput(ns("cost_per_million_reads_min"), 
                              label = NULL,
                              value = 0.374, 
                              min = 0, 
                              step = 0.001,
                              width = "100%")
                )
              )
            )
          )
        ),
        
        # Step 3: Parameter Control (initially hidden)
        tags$div(
          id = ns("step3"),
          style = "display: none;",
          # Dynamic parameter controls with conditional title
          uiOutput(ns("dynamic_params"))
        ),
        
        # Design Problem Summary (appears after Step 3 is completed)
        tags$div(
          id = ns("design_summary"),
          style = "display: none; margin-top: 20px; padding: 15px; background-color: #f8f9fa; border-left: 4px solid #4A6B82; border-radius: 4px;",
          tags$div(
            tags$strong("Your Design Problem:", style = "color: #4A6B82; margin-bottom: 8px; display: block;"),
            tags$div(
              id = ns("summary_text"),
              style = "font-size: 14px; line-height: 1.4; color: #333;"
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
      # Power/cost inputs appear when optimization type is selected
      if (!is.null(input$optimization_type) && input$optimization_type != "") {
        shinyjs::show("power_cost_inputs")
        
        # Show cost budget input only for power + cost constraints
        if (input$optimization_type == "power_cost") {
          shinyjs::show("cost_budget_div")
        } else {
          shinyjs::hide("cost_budget_div")
        }
      } else {
        shinyjs::hide("power_cost_inputs")
        shinyjs::hide("step2")
        shinyjs::hide("step3")
      }
    })
    
    # Reset power and cost inputs when constraint type changes
    observe({
      input$optimization_type
      
      # Reset the inputs when constraint type changes
      updateNumericInput(session, "target_power", value = 0.8)
      updateNumericInput(session, "cost_budget", value = 10000)
    })
    
    observe({
      # Step 2 appears when power (and cost if needed) inputs are provided
      power_ready <- !is.null(input$target_power) && is.numeric(input$target_power) && input$target_power > 0
      cost_ready <- TRUE  # Default to ready
      
      # If power+cost is selected, also check cost budget
      if (!is.null(input$optimization_type) && input$optimization_type == "power_cost") {
        cost_ready <- !is.null(input$cost_budget) && is.numeric(input$cost_budget) && input$cost_budget > 0
      }
      
      if (power_ready && cost_ready) {
        shinyjs::show("step2")
      } else {
        shinyjs::hide("step2")
        shinyjs::hide("step3")
      }
    })
    
    observe({
      # Step 3 appears when minimization target is selected AND there are parameters to configure
      if (!is.null(input$minimization_target) && input$minimization_target != "") {
        
        # Check if there are any varying parameters to show
        opt_type <- input$optimization_type
        target <- input$minimization_target
        
        if (!is.null(opt_type) && opt_type != "" && !is.null(target) && target != "") {
          param_configs <- get_param_configs(opt_type, target)
          
          # Count how many parameters need UI controls (not minimizing/optimizing)
          has_controls <- 
            (!param_configs$cells_per_target$type %in% c("minimizing", "optimizing")) ||
            (!param_configs$mapped_reads_per_cell$type %in% c("minimizing", "optimizing")) ||
            (!param_configs$TPM_threshold$type %in% c("minimizing", "optimizing")) ||
            (!param_configs$minimum_fold_change$type %in% c("minimizing", "optimizing"))
          
          if (has_controls) {
            shinyjs::show("step3")
          } else {
            shinyjs::hide("step3")
          }
        } else {
          shinyjs::hide("step3")
        }
        
        # Show cost parameters if minimizing total cost
        if (input$minimization_target == "cost") {
          shinyjs::show("cost_minimization_params")
        } else {
          shinyjs::hide("cost_minimization_params")
        }
      } else {
        shinyjs::hide("step3")
        shinyjs::hide("cost_minimization_params")
      }
    })
    
    # Generate and display design problem summary (after Step 3)
    observe({
      # Trigger on parameter control changes
      input$cells_per_target_control
      input$reads_per_cell_control
      input$TPM_control
      input$fc_control
      
      # Show summary only after all steps are completed and Step 3 is visible
      if (!is.null(input$optimization_type) && input$optimization_type != "" &&
          !is.null(input$minimization_target) && input$minimization_target != "" &&
          !is.null(input$target_power) && input$target_power > 0) {
        
        # Check if Step 3 has been shown (indicating parameter controls are set up)
        step3_visible <- !is.null(input$minimization_target) && input$minimization_target != ""
        
        if (step3_visible) {
        
        # Get resolved parameter configurations for accurate summary
        param_configs <- get_param_configs(input$optimization_type, input$minimization_target)
        
          # Generate summary text based on workflow and current input states
          summary_text <- generate_design_summary(
            opt_type = input$optimization_type,
            target = input$minimization_target,
            power = input$target_power,
            cost_budget = input$cost_budget,
            param_configs = param_configs,
            cells_per_target_control = input$cells_per_target_control,
            reads_per_cell_control = input$reads_per_cell_control,
            TPM_control = input$TPM_control,
            fc_control = input$fc_control
          )
        
          # Update summary text and show the section
          shinyjs::html("summary_text", summary_text)
          shinyjs::show("design_summary")
        } else {
          shinyjs::hide("design_summary")
        }
      } else {
        shinyjs::hide("design_summary")
      }
    })
    
    # Helper function to generate design problem summary
    generate_design_summary <- function(opt_type, target, power, cost_budget, param_configs = NULL, 
                                       cells_per_target_control = NULL, reads_per_cell_control = NULL, 
                                       TPM_control = NULL, fc_control = NULL) {
      # Base text
      if (opt_type == "power_only") {
        if (target == "cost") {
          return(paste0(
            "Find the minimum <strong>total cost</strong> for which power is at least <strong>", 
            power * 100, "%</strong>, while varying cells per target and reads per cell, keeping TPM threshold and fold change fixed."
          ))
        } else {
          target_name <- switch(target,
            "cells_per_target" = "cells per target",
            "reads_per_cell" = "reads per cell", 
            "TPM_threshold" = "TPM threshold",
            "minimum_fold_change" = "fold change"
          )
          return(paste0(
            "Find the minimum <strong>", target_name, "</strong> for which power is at least <strong>", 
            power * 100, 
            "%</strong>, keeping all other parameters fixed."
          ))
        }
      } else if (opt_type == "power_cost") {
        target_name <- switch(target,
          "TPM_threshold" = "TPM threshold",
          "minimum_fold_change" = "fold change"
        )
        
        # Generate specific parameter description based on actual Step 3 input states
        param_desc <- ""
        
        # Use actual control input values if available, otherwise fall back to param_configs
        actual_cells_type <- cells_per_target_control
        actual_reads_type <- reads_per_cell_control
        
        # If inputs not available, use resolved configs
        if (is.null(actual_cells_type) && !is.null(param_configs)) {
          actual_cells_type <- param_configs$cells_per_target$type
        }
        if (is.null(actual_reads_type) && !is.null(param_configs)) {
          actual_reads_type <- param_configs$mapped_reads_per_cell$type
        }
        
        if (!is.null(actual_cells_type) && !is.null(actual_reads_type)) {
          # Build cells/reads description
          cells_reads_desc <- ""
          if (actual_cells_type == "varying" && actual_reads_type == "varying") {
            cells_reads_desc <- "varying cells per target and reads per cell"
          } else if (actual_cells_type == "fixed" && actual_reads_type == "varying") {
            cells_reads_desc <- "keeping cells per target fixed and varying reads per cell"
          } else if (actual_cells_type == "varying" && actual_reads_type == "fixed") {
            cells_reads_desc <- "varying cells per target and keeping reads per cell fixed"
          } else if (actual_cells_type == "fixed" && actual_reads_type == "fixed") {
            cells_reads_desc <- "keeping both cells per target and reads per cell fixed"
          } else {
            cells_reads_desc <- "optimizing cells per target and reads per cell parameters"
          }
          
          # Add TPM/FC information for power+cost workflows
          TPM_fc_desc <- ""
          if (target == "TPM_threshold") {
            TPM_fc_desc <- "keeping fold change fixed"
          } else if (target == "minimum_fold_change") {
            TPM_fc_desc <- "keeping TPM threshold fixed"
          }
          
          # Combine descriptions - always include TPM/FC info for power+cost
          if (TPM_fc_desc != "") {
            param_desc <- paste0("while ", cells_reads_desc, " and ", TPM_fc_desc)
          } else {
            param_desc <- paste0("while ", cells_reads_desc)
          }
        } else {
          param_desc <- "while configuring parameter constraints"
        }
        
        return(paste0(
          "Find the minimum <strong>", target_name, "</strong> for which power is at least <strong>", 
          power * 100, 
          "%</strong> and cost is at most <strong>$", 
          format(cost_budget, big.mark = ",", scientific = FALSE),
          "</strong>, ", param_desc, "."
        ))
      }
      
      return("Please complete all design options to see your optimization objective.")
    }
    
    # Dynamic parameter controls generation
    output$dynamic_params <- renderUI({
      opt_type <- input$optimization_type
      target <- input$minimization_target
      
      if (is.null(opt_type) || is.null(target) || opt_type == "" || target == "") {
        return(NULL)
      }
      
      # Define parameter configurations for each workflow
      param_configs <- get_param_configs(opt_type, target)
      
      # Generate UI controls only for non-minimizing parameters
      param_uis <- list()
      
      # Only show parameters that are not being minimized or optimized automatically
      if (!param_configs$cells_per_target$type %in% c("minimizing", "optimizing")) {
        param_uis <- append(param_uis, list(
          create_param_ui(ns, "cells_per_target", "Cells per target:", param_configs$cells_per_target, 1000, 50, 5000, 50)
        ))
      }
      
      if (!param_configs$mapped_reads_per_cell$type %in% c("minimizing", "optimizing")) {
        param_uis <- append(param_uis, list(
          create_param_ui(ns, "reads_per_cell", "Mapped reads per cell:", param_configs$mapped_reads_per_cell, 5000, 500, 20000, 500)
        ))
      }
      
      if (!param_configs$TPM_threshold$type %in% c("minimizing", "optimizing")) {
        param_uis <- append(param_uis, list(
          create_param_ui(ns, "TPM", "TPM threshold:", param_configs$TPM_threshold, 10, 0, 100, 1)
        ))
      }
      
      if (!param_configs$minimum_fold_change$type %in% c("minimizing", "optimizing")) {
        param_uis <- append(param_uis, list(
          create_param_ui(ns, "fc", "Fold change:", param_configs$minimum_fold_change, 1.5, 1.1, 10, 0.1)
        ))
      }
      
      if (length(param_uis) == 0) {
        # No varying parameters to show - hide Step 3 entirely
        return(NULL)
      }
      
      # Show title only when there are parameters to display
      tagList(
        tags$h5("Step 3: Varying parameters", style = "color: #4A6B82; margin-bottom: 10px; font-weight: bold;"),
        do.call(tagList, param_uis)
      )
    })
    
    # Helper function to get parameter configurations for each workflow
    get_param_configs <- function(opt_type, target) {
      configs <- list(
        cells_per_target = list(type = "varying", enabled = TRUE),
        mapped_reads_per_cell = list(type = "varying", enabled = TRUE),
        TPM_threshold = list(type = "varying", enabled = TRUE),
        minimum_fold_change = list(type = "varying", enabled = TRUE)
      )
      
      if (opt_type == "power_only") {
        if (target %in% c("cells_per_target", "reads_per_cell", "TPM_threshold", "minimum_fold_change")) {
          # Power-only + single parameter minimization: minimize target, fix all others
          configs$cells_per_target$type <- if (target == "cells_per_target") "minimizing" else "fixed"
          configs$mapped_reads_per_cell$type <- if (target == "reads_per_cell") "minimizing" else "fixed"
          configs$TPM_threshold$type <- if (target == "TPM_threshold") "minimizing" else "fixed"
          configs$minimum_fold_change$type <- if (target == "minimum_fold_change") "minimizing" else "fixed"
        } else if (target == "cost") {
          # Cost minimization: cells/reads vary simultaneously (omit both), TPM/fc fixed
          configs$cells_per_target$type <- "optimizing"
          configs$mapped_reads_per_cell$type <- "optimizing"
          configs$TPM_threshold$type <- "fixed"
          configs$minimum_fold_change$type <- "fixed"
        }
      } else if (opt_type == "power_cost") {
        if (target == "TPM_threshold") {
          # Power+cost + TPM minimization: TPM minimizing, FC fixed, cells/reads constrained varying/fixed
          configs$cells_per_target$type <- "varying"
          configs$mapped_reads_per_cell$type <- "varying"
          configs$TPM_threshold$type <- "minimizing"
          configs$minimum_fold_change$type <- "fixed"
        } else if (target == "minimum_fold_change") {
          # Power+cost + FC minimization: FC minimizing, TPM fixed, cells/reads constrained varying/fixed
          configs$cells_per_target$type <- "varying"
          configs$mapped_reads_per_cell$type <- "varying"
          configs$TPM_threshold$type <- "fixed"
          configs$minimum_fold_change$type <- "minimizing"
        }
      }
      
      return(configs)
    }
    
    # Helper function to create parameter UI based on type
    create_param_ui <- function(ns, param_id, label, config, default_val, min_val, max_val, step_val) {
      if (config$type == "varying") {
        # For varying parameters: show dropdown with varying/fixed options
        # Fixed value inputs are now in their logical sidebar sections
        tags$div(
          style = "margin-bottom: 15px;",
          selectInput(ns(paste0(param_id, "_control")), label,
                     choices = list("Varying" = "varying", "Fixed" = "fixed"),
                     selected = "varying")
        )
      } else if (config$type == "fixed") {
        # For fixed parameters: Don't show redundant "(Fixed)" labels 
        # Users can see fixed values in the summary section
        NULL
      }
      # Note: minimizing parameters are completely omitted (return NULL)
    }
    
    # Business Logic: Update cost availability based on optimization type
    observe({
      if (!is.null(input$optimization_type) && input$optimization_type == "power_cost") {
        # Update choices to disable cost option
        # Power+cost: Only TPM and FC can be minimized
        updateSelectInput(session, "minimization_target",
                         choices = list(
                           "Select what to minimize..." = "",
                           "Minimize TPM analysis threshold" = "TPM_threshold",
                           "Minimize fold change" = "minimum_fold_change"
                         ))
        
        # Update cells/reads dropdowns to remove "Minimizing" option for Power+Cost
        updateSelectInput(session, "cells_per_target_control",
                         choices = list("Varying" = "varying", "Fixed" = "fixed"))
        updateSelectInput(session, "reads_per_cell_control", 
                         choices = list("Varying" = "varying", "Fixed" = "fixed"))
      } else {
        # Include cost option
        updateSelectInput(session, "minimization_target",
                         choices = list(
                           "Select what to minimize..." = "",
                           "Cells per target" = "cells_per_target",
                           "Reads per cell" = "reads_per_cell", 
                           "Total cost" = "cost",
                           "TPM analysis threshold" = "TPM_threshold",
                           "Fold change" = "minimum_fold_change"
                         ))
        
        # Restore full choices for cells/reads dropdowns when not Power+Cost
        updateSelectInput(session, "cells_per_target_control",
                         choices = list("Varying" = "varying", "Fixed" = "fixed", "Minimizing" = "minimizing"))
        updateSelectInput(session, "reads_per_cell_control", 
                         choices = list("Varying" = "varying", "Fixed" = "fixed", "Minimizing" = "minimizing"))
      }
    })
    
    # Business Logic: Show minimizing parameter and control other parameters
    observe({
      target <- input$minimization_target
      
      if (!is.null(target) && target != "") {
        # Set the selected parameter to "Minimizing"
        if (target == "cells_per_target") {
          updateSelectInput(session, "cells_per_target_control", selected = "minimizing")
          updateSelectInput(session, "reads_per_cell_control", selected = "varying")
          updateSelectInput(session, "TPM_control", selected = "varying")
          updateSelectInput(session, "fc_control", selected = "varying")
        } else if (target == "reads_per_cell") {
          updateSelectInput(session, "cells_per_target_control", selected = "varying")
          updateSelectInput(session, "reads_per_cell_control", selected = "minimizing")
          updateSelectInput(session, "TPM_control", selected = "varying")
          updateSelectInput(session, "fc_control", selected = "varying")
        } else if (target == "cost") {
          # For cost minimization: cells/reads = varying, TPM/fc = fixed
          updateSelectInput(session, "cells_per_target_control", selected = "varying")
          updateSelectInput(session, "reads_per_cell_control", selected = "varying")
          updateSelectInput(session, "TPM_control", selected = "fixed")
          updateSelectInput(session, "fc_control", selected = "fixed")
        } else if (target == "TPM_threshold") {
          updateSelectInput(session, "cells_per_target_control", selected = "varying")
          updateSelectInput(session, "reads_per_cell_control", selected = "varying")
          updateSelectInput(session, "TPM_control", selected = "minimizing")
          updateSelectInput(session, "fc_control", selected = "varying")
        } else if (target == "minimum_fold_change") {
          updateSelectInput(session, "cells_per_target_control", selected = "varying")
          updateSelectInput(session, "reads_per_cell_control", selected = "varying")
          updateSelectInput(session, "TPM_control", selected = "varying")
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
        if (opt_type == "power_only" && target %in% c("cells_per_target", "reads_per_cell", "TPM_threshold", "minimum_fold_change")) {
          
          # Force all non-minimizing parameters to be "Fixed"
          if (target != "cells_per_target") {
            updateSelectInput(session, "cells_per_target_control", selected = "fixed")
          }
          if (target != "reads_per_cell") {
            updateSelectInput(session, "reads_per_cell_control", selected = "fixed")
          }
          if (target != "TPM_threshold") {
            updateSelectInput(session, "TPM_control", selected = "fixed")
          }
          if (target != "minimum_fold_change") {
            updateSelectInput(session, "fc_control", selected = "fixed")
          }
          
          # Disable all dropdowns (non-clickable)
          shinyjs::disable("cells_per_target_control")
          shinyjs::disable("reads_per_cell_control")
          shinyjs::disable("TPM_control")
          shinyjs::disable("fc_control")
        } else if (target == "cost") {
          # Cost minimization = all parameters disabled (cells/reads varying, TPM/fc fixed)
          shinyjs::disable("cells_per_target_control")
          shinyjs::disable("reads_per_cell_control")
          shinyjs::disable("TPM_control")
          shinyjs::disable("fc_control")
        } else if (opt_type == "power_cost" && target == "TPM_threshold") {
          # Power+cost + TPM minimization: TPM=minimizing+disabled, FC=fixed+disabled, cells/reads=varying/fixed with constraint
          updateSelectInput(session, "TPM_control", selected = "minimizing")
          updateSelectInput(session, "fc_control", selected = "fixed")
          shinyjs::disable("TPM_control")
          shinyjs::disable("fc_control")
          # Cells/reads: varying/fixed but not both fixed (handled by cells/reads constraint logic)
          shinyjs::enable("cells_per_target_control")
          shinyjs::enable("reads_per_cell_control")
        } else if (opt_type == "power_cost" && target == "minimum_fold_change") {
          # Power+cost + FC minimization: FC=minimizing+disabled, TPM=fixed+disabled, cells/reads=varying/fixed with constraint
          updateSelectInput(session, "fc_control", selected = "minimizing")
          updateSelectInput(session, "TPM_control", selected = "fixed")
          shinyjs::disable("fc_control")
          shinyjs::disable("TPM_control")
          # Cells/reads: varying/fixed but not both fixed (handled by cells/reads constraint logic)
          shinyjs::enable("cells_per_target_control")
          shinyjs::enable("reads_per_cell_control")
        } else {
          # Re-enable all dropdowns for other scenarios
          shinyjs::enable("cells_per_target_control")
          shinyjs::enable("reads_per_cell_control")
          shinyjs::enable("TPM_control")
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
          opt_type == "power_cost" && target %in% c("TPM_threshold", "minimum_fold_change")) {
        
        # When cells is set to fixed, auto-set reads to varying and disable it
        if (!is.null(input$cells_per_target_control) && input$cells_per_target_control == "fixed") {
          updateSelectInput(session, "reads_per_cell_control", selected = "varying")
          shinyjs::disable("reads_per_cell_control")
        }
        
        # When reads is set to fixed, auto-set cells to varying and disable it
        if (!is.null(input$reads_per_cell_control) && input$reads_per_cell_control == "fixed") {
          updateSelectInput(session, "cells_per_target_control", selected = "varying")
          shinyjs::disable("cells_per_target_control")
        }
        
        # When cells or reads is set back to varying, enable the other
        if (!is.null(input$cells_per_target_control) && input$cells_per_target_control == "varying") {
          shinyjs::enable("reads_per_cell_control")
        }
        if (!is.null(input$reads_per_cell_control) && input$reads_per_cell_control == "varying") {
          shinyjs::enable("cells_per_target_control")
        }
      }
    })
    
    # Helper function to get resolved parameter controls using business logic and user inputs
    get_resolved_param_controls <- function(opt_type, target, input_vals) {
      # Get the base parameter configs using business logic
      param_configs <- get_param_configs(opt_type, target)
      
      # Override with actual user input from Step 3 controls when available
      # But ONLY allow user input to override "varying" types (user has a choice)
      # Never allow override of "minimizing", "optimizing", or "fixed" types (business logic)
      cells_type <- param_configs$cells_per_target$type
      if (!is.null(input_vals$cells_per_target_control) && 
          param_configs$cells_per_target$type == "varying") {
        cells_type <- input_vals$cells_per_target_control
      }
      
      
      reads_type <- param_configs$mapped_reads_per_cell$type
      if (!is.null(input_vals$reads_per_cell_control) && 
          param_configs$mapped_reads_per_cell$type == "varying") {
        reads_type <- input_vals$reads_per_cell_control
      }
      
      TPM_type <- param_configs$TPM_threshold$type
      # Only allow user override if base config allows varying/fixed choice
      if (!is.null(input_vals$TPM_control) && 
          param_configs$TPM_threshold$type == "varying") {
        TPM_type <- input_vals$TPM_control
      }
      
      fc_type <- param_configs$minimum_fold_change$type
      # Only allow user override if base config allows varying/fixed choice
      if (!is.null(input_vals$fc_control) && 
          param_configs$minimum_fold_change$type == "varying") {
        fc_type <- input_vals$fc_control
      }
      
      list(
        cells_per_target = list(
          type = cells_type,
          fixed_value = if(!is.null(input_vals$cells_fixed)) input_vals$cells_fixed else NULL
        ),
        mapped_reads_per_cell = list(
          type = reads_type,
          fixed_value = if(!is.null(input_vals$reads_fixed)) input_vals$reads_fixed else NULL
        ),
        TPM_threshold = list(
          type = TPM_type,
          fixed_value = if(!is.null(input_vals$TPM_fixed)) input_vals$TPM_fixed else NULL
        ),
        minimum_fold_change = list(
          type = fc_type,
          fixed_value = if(!is.null(input_vals$minimum_fold_change_fixed)) input_vals$minimum_fold_change_fixed else NULL
        )
      )
    }
    
    # Return structured design configuration
    design_config <- reactive({
      
      # Explicitly depend on control inputs to ensure reactivity
      input$cells_per_target_control
      input$reads_per_cell_control
      input$TPM_control
      input$fc_control
      
      # Safe access to input values with NULL checking
      target <- input$minimization_target %||% ""
      
      list(
        # Design Options
        optimization_type = input$optimization_type,
        minimization_target = input$minimization_target,
        
        # Power and Cost Requirements
        target_power = input$target_power,
        cost_budget = if (input$optimization_type == "power_cost") input$cost_budget else NULL,
        cost_per_cell = if (input$optimization_type == "power_cost") input$cost_per_cell 
                        else if (input$minimization_target == "cost") input$cost_per_cell_min 
                        else NULL,
        cost_per_million_reads = if (input$optimization_type == "power_cost") input$cost_per_million_reads 
                                else if (input$minimization_target == "cost") input$cost_per_million_reads_min 
                                else NULL,
        parameter_controls = get_resolved_param_controls(input$optimization_type, target, input),
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
