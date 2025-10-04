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
      class = "collapsible-section",
      tags$div(
        id = ns("design-header"),
        class = "collapsible-header",
        onclick = paste0("toggleSection('", ns("design-content"), "', '", ns("design-chevron"), "')"),
        tags$i(id = ns("design-chevron"), class = "fa fa-chevron-down"),
        tags$strong("Design problem")
      ),
      tags$div(
        id = ns("design-content"),
        class = "collapsible-content",

        # Step 1: Assay Type
        tags$div(
          id = ns("step1"),
          tags$h5("Step 1: Assay Type", class = "step-header-large"),
          selectInput(ns("assay_type"), NULL,
                     choices = list(
                       "Select assay type..." = "",
                       "Perturb-seq" = "perturb_seq",
                       "TAP-seq" = "tap_seq"
                     ),
                     selected = "")
        ),

        # Step 2: Optimization Constraints (initially hidden)
        tags$div(
          id = ns("step2"),
          class = "hidden-section-with-spacing",
          tags$h5("Step 2: Optimization Constraints", class = "step-header-large"),
          selectInput(ns("optimization_type"), NULL,
                     choices = list(
                       "Select constraint type..." = "",
                       "Constrain power only" = "power_only",
                       "Constrain power and cost" = "power_cost"
                     ),
                     selected = "")
        ),

        # Power and Cost Requirements (initially hidden)
        tags$div(
          id = ns("power_cost_inputs"),
          class = "hidden-section-with-spacing",

          # Target Power Input (always shown when constraint type is selected)
          tags$div(
            tags$h6("Target Power:", class = "step-header"),
            numericInput(ns("target_power"), NULL,
                        value = 0.8, min = 0.1, max = 0.99, step = 0.05)
          ),

          # Cost Budget Input (only shown for power + cost constraints)
          tags$div(
            id = ns("cost_budget_div"),
            class = "hidden-section-with-margin",
            tags$h6("Cost Budget ($):", class = "step-header"),
            numericInput(ns("cost_budget"), NULL,
                        value = 10000, min = 100, max = 1000000, step = 500),

            # Cost parameters (below cost budget)
            tags$div(
              class = "cost-params-section",
              tags$h6("Cost Parameters:", class = "step-header"),

              # Cost inputs using helper function
              create_cost_inputs_ui(ns, "cost_per_cell")
            )
          )
        ),

        # Step 3: Minimization Target (initially hidden)
        tags$div(
          id = ns("step3"),
          class = "hidden-section-with-spacing",
          tags$h5("Step 3: Minimization Target", class = "step-header-large"),
          selectInput(ns("minimization_target"), NULL,
                     choices = list(
                       "Select what to minimize..." = "",
                       "Cells per target" = "cells_per_target",
                       "Reads per cell" = "reads_per_cell",
                       "Total cost" = "cost",
                       "Expression threshold" = "TPM_threshold",
                       "Fold change" = "minimum_fold_change"
                     ),
                     selected = ""),

          # Expression threshold reminder (conditional)
          conditionalPanel(
            condition = "input.minimization_target == 'TPM_threshold'",
            ns = ns,
            tags$div(
              class = "parameter-info-note",
              uiOutput(ns("expression_threshold_reminder"))
            )
          ),

          # Cost parameters for cost minimization (only shown when minimizing total cost)
          tags$div(
            id = ns("cost_minimization_params"),
            class = "hidden-section-with-large-margin",
            tags$h6("Cost Parameters:", class = "step-header"),

            # Cost inputs using helper function
            create_cost_inputs_ui(ns, "cost_per_cell_min")
          )
        ),

        # Step 4: Parameter Control (initially hidden)
        tags$div(
          id = ns("step4"),
          class = "hidden-section",
          # Dynamic parameter controls with conditional title
          uiOutput(ns("dynamic_params"))
        ),

        # Design Problem Summary (appears after Step 3 is completed)
        tags$div(
          id = ns("design_summary"),
          class = "design-summary",
          tags$div(
            tags$strong("Your Design Problem:", class = "design-summary-title"),
            tags$div(
              id = ns("summary_text"),
              class = "design-summary-text"
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
mod_design_options_server <- function(id, app_state = NULL){
  moduleServer(id, function(input, output, session){
    ns <- session$ns


    # INPUT FREEZING: Disable all inputs in Phase 2, keep section titles functional
    observeEvent(app_state$phase, {
      if (!is.null(app_state)) {
        inputs_disabled <- (app_state$phase == 2)

        # Core inputs that should be disabled in Phase 2
        shinyjs::toggleState("assay_type", condition = !inputs_disabled)
        shinyjs::toggleState("optimization_type", condition = !inputs_disabled)
        shinyjs::toggleState("target_power", condition = !inputs_disabled)
        shinyjs::toggleState("cost_budget", condition = !inputs_disabled)
        shinyjs::toggleState("cost_per_cell", condition = !inputs_disabled)
        shinyjs::toggleState("cost_per_million_reads", condition = !inputs_disabled)
        shinyjs::toggleState("minimization_target", condition = !inputs_disabled)
        shinyjs::toggleState("cost_per_cell_min", condition = !inputs_disabled)
        shinyjs::toggleState("cost_per_million_reads_min", condition = !inputs_disabled)

        # Dynamic parameter controls (Step 3)
        shinyjs::toggleState("cells_per_target_control", condition = !inputs_disabled)
        shinyjs::toggleState("reads_per_cell_control", condition = !inputs_disabled)
        shinyjs::toggleState("TPM_control", condition = !inputs_disabled)
        shinyjs::toggleState("fc_control", condition = !inputs_disabled)

        # Note: Section headers (#design-header) remain functional for collapse/expand
      }
    }, ignoreInit = TRUE, ignoreNULL = TRUE)

    # PHASE 3.1: Progressive State Reactive
    # Centralized state calculation for unified progressive disclosure controller
    progressive_state <- reactive({
      # Calculate basic validations for new 4-step structure
      step1_complete <- is_assay_type_complete(input$assay_type)
      step2_complete <- is_step2_complete(input$optimization_type)
      power_ready <- is_power_input_ready(input$target_power)
      cost_ready <- is_cost_budget_ready(input$cost_budget, input$optimization_type)
      step3_complete <- is_step3_complete(input$minimization_target)
      step4_has_controls <- workflow_has_varying_parameters(input$optimization_type, input$minimization_target)

      # Return complete state object
      list(
        # Basic validations
        step1_complete = step1_complete,
        step2_complete = step2_complete,
        power_ready = power_ready,
        cost_ready = cost_ready,
        step3_complete = step3_complete,
        step4_has_controls = step4_has_controls,

        # Combined conditions for step visibility
        step2_ready = step1_complete,  # Step 2 shows after Step 1 (assay type)
        step3_ready = step1_complete && step2_complete && power_ready && cost_ready,  # Step 3 shows after Step 2 + inputs
        step4_ready = step3_complete && step4_has_controls,  # Step 4 shows after Step 3 + has controls

        # Special UI conditions
        show_cost_budget = !is.null(input$optimization_type) && input$optimization_type == "power_cost",
        show_cost_minimization = !is.null(input$minimization_target) && input$minimization_target == "cost",

        # Summary readiness check
        # Power-only workflows: show after step3 complete (no step4 needed)
        # Power+cost workflows: show after step4 complete with all controls set
        summary_ready = step3_complete &&
                       (if (step4_has_controls) {
                         are_all_parameter_controls_set(input$optimization_type, input$minimization_target, input)
                       } else {
                         TRUE  # No step4 needed for power-only workflows
                       }),

        # Current workflow context
        optimization_type = input$optimization_type,
        minimization_target = input$minimization_target
      )
    })

    # Unified Progressive Disclosure Controller
    # Centralized state management for all step visibility and UI updates
    observe({
      state <- progressive_state()

      # Step 1 (assay type) → Step 2 (optimization constraints) visibility
      toggle_step2_section(session, state$step2_ready)

      # Step 2 → Power/Cost section visibility
      toggle_power_cost_inputs(session, state$step2_ready, state$optimization_type)

      # Step 2 + inputs ready → Step 3 (minimization target) visibility
      toggle_step3_section(session, state$step3_ready)

      # Step 3 + has controls → Step 4 (parameter control) visibility
      toggle_step4_section(session, state$step4_ready)

      # Cost minimization parameters (independent condition)
      toggle_cost_minimization_params(session, state$show_cost_minimization)

      # All conditions met → Design Summary (only show in Phase 1)
      if (state$summary_ready && !is.null(app_state) && app_state$phase == 1) {
        # Generate summary text using existing function
        summary_text <- generate_design_summary(
          opt_type = state$optimization_type,
          target = state$minimization_target,
          power = input$target_power,
          cost_budget = input$cost_budget,
          param_configs = if (!is.null(state$optimization_type) && !is.null(state$minimization_target)) {
            get_param_configs(state$optimization_type, state$minimization_target)
          } else NULL,
          cells_per_target_control = input$cells_per_target_control,
          reads_per_cell_control = input$reads_per_cell_control,
          TPM_control = input$TPM_control,
          fc_control = input$fc_control,
          assay_type = input$assay_type
        )
        toggle_design_summary(session, TRUE, summary_text)
      } else {
        toggle_design_summary(session, FALSE)
      }
    })

    # Phase-aware design summary controller - hide summary when entering Phase 2
    observeEvent(app_state$phase, {
      if (!is.null(app_state) && app_state$phase == 2) {
        toggle_design_summary(session, FALSE)
      }
    }, ignoreNULL = TRUE, ignoreInit = TRUE)

    # Input reset controller (separate concern)
    observe({
      input$optimization_type  # dependency trigger
      reset_input_values(session)
    })

    # Business Logic: Clear all downstream selections when Step 1 (assay type) changes
    observe({
      input$assay_type  # dependency trigger

      # Clear Step 2 (optimization type)
      updateSelectInput(session, "optimization_type", selected = "")

      # Clear Step 3 (minimization target)
      updateSelectInput(session, "minimization_target", selected = "")

      # Reset power/cost inputs to defaults
      reset_input_values(session)

      # Clear Step 4 parameter controls
      updateSelectInput(session, "cells_per_target_control", selected = "")
      updateSelectInput(session, "reads_per_cell_control", selected = "")
      updateSelectInput(session, "TPM_control", selected = "")
      updateSelectInput(session, "fc_control", selected = "")
    })




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
          create_param_ui(ns, "cells_per_target", "Cells per target:", param_configs$cells_per_target, 1000, 20, 2000, 20)
        ))
      }

      if (!param_configs$reads_per_cell$type %in% c("minimizing", "optimizing")) {
        param_uis <- append(param_uis, list(
          create_param_ui(ns, "reads_per_cell", "Reads per cell:", param_configs$reads_per_cell, 5000, 1000, 150000, 1000)
        ))
      }

      if (!param_configs$TPM_threshold$type %in% c("minimizing", "optimizing")) {
        param_uis <- append(param_uis, list(
          create_param_ui(ns, "TPM", "TPM threshold:", param_configs$TPM_threshold, 10, 0, 100, 1)
        ))
      }

      if (!param_configs$minimum_fold_change$type %in% c("minimizing", "optimizing")) {
        param_uis <- append(param_uis, list(
          create_param_ui(ns, "fc", "Fold change:", param_configs$minimum_fold_change, 1.5, 1.1, 2, 0.1)
        ))
      }

      if (length(param_uis) == 0) {
        # No varying parameters to show - hide Step 3 entirely
        return(NULL)
      }

      # Show title only when there are parameters to display
      tagList(
        tags$h5("Step 4: Varying parameters", class = "step-header-large"),
        do.call(tagList, param_uis)
      )
    })

    # Conditional reminder for Expression threshold
    output$expression_threshold_reminder <- renderUI({
      if (!is.null(input$assay_type) && input$assay_type != "" &&
          !is.null(input$minimization_target) && input$minimization_target == "TPM_threshold") {

        message <- if (input$assay_type == "tap_seq") {
          "Minimum UMI per cell for a gene at the sequencing saturation"
        } else if (input$assay_type == "perturb_seq") {
          "Minimum TPM analysis threshold parameter"
        } else {
          ""
        }

        if (message != "") {
          tags$small(message, style = "color: #888; font-style: italic;")
        }
      }
    })

    # Business Logic: Update cost availability and clear Step 2 when Step 1 changes
    observe({
      if (!is.null(input$optimization_type) && input$optimization_type == "power_cost") {
        # Update choices to disable cost option
        # Power+cost: Only TPM and FC can be minimized
        updateSelectInput(session, "minimization_target",
                         choices = list(
                           "Select what to minimize..." = "",
                           "Minimize Expression threshold" = "TPM_threshold",
                           "Minimize fold change" = "minimum_fold_change"
                         ),
                         selected = "")  # Clear selection when Step 1 changes

        # Update cells/reads dropdowns to remove "Minimizing" option for Power+Cost
        updateSelectInput(session, "cells_per_target_control",
                         choices = list("Varying" = "varying", "Fixed" = "fixed"))
        updateSelectInput(session, "reads_per_cell_control",
                         choices = list("Varying" = "varying", "Fixed" = "fixed"))
      } else if (!is.null(input$optimization_type) && input$optimization_type != "") {
        # Include cost option for power_only
        updateSelectInput(session, "minimization_target",
                         choices = list(
                           "Select what to minimize..." = "",
                           "Cells per target" = "cells_per_target",
                           "Reads per cell" = "reads_per_cell",
                           "Total cost" = "cost",
                           "Expression threshold" = "TPM_threshold",
                           "Fold change" = "minimum_fold_change"
                         ),
                         selected = "")  # Clear selection when Step 1 changes

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
        parameter_controls = if (!is.null(input$optimization_type) && input$optimization_type != "" &&
                                 !is.null(target) && target != "") {
          get_resolved_param_controls(input$optimization_type, target, input)
        } else {
          NULL  # No parameter controls during transitions - this prevents analysis from running
        },
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
