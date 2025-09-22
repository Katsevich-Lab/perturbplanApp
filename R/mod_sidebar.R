#' sidebar UI Function
#'
#' @description Creates the left sidebar using modular components
#' for constraint-driven parameter inputs
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList tags div actionButton observeEvent showNotification uiOutput renderUI modalDialog showModal removeModal
#' @importFrom shinydashboard dashboardSidebar
#' @importFrom shinyjs show hide
mod_sidebar_ui <- function(id) {
  ns <- NS(id)

  dashboardSidebar(
    # Sidebar toggle button (positioned at right edge)
    tags$div(
      id = "simple-toggle",
      "\\u25C0"
    ),

    # Parameter panels - make scrollable with collapsible sections
    tags$div(
      style = "padding: 10px; max-height: 90vh; overflow-y: auto; position: relative;",

      # Design problem
      mod_design_options_ui(ns("design_options")),

      # Experimental choices (now includes perturbation choices)
      mod_experimental_setup_ui(ns("experimental_setup")),

      # Analysis choices
      mod_analysis_choices_ui(ns("analysis_choices")),

      # Effect sizes (always visible - contains non-null proportion)
      mod_effect_sizes_ui(ns("effect_sizes")),

      # Advanced settings
      mod_advanced_choices_ui(ns("advanced_choices")),

      # Horizontal separator line
      tags$hr(class = "sidebar-separator"),

      # Dynamic Plan/Restart button
      tags$div(
        style = "text-align: center; padding: 0 20px;",
        uiOutput(ns("dynamic_plan_button")),

        # Instructional text for Phase 2 (shown below restart button)
        uiOutput(ns("restart_instruction_text"))
      )
    )
  )
}

#' sidebar Server Functions
#'
#' @description Server logic for sidebar using modular components
#' operating independently from parameter manager and sliders
#'
#' @param id Module namespace ID
#'
#' @noRd
mod_sidebar_server <- function(id, app_state = NULL){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    # Initialize module servers - all modules now have app_state for input freezing
    design_config <- mod_design_options_server("design_options", app_state)
    experimental_config <- mod_experimental_setup_server("experimental_setup", design_config, app_state)
    analysis_config <- mod_analysis_choices_server("analysis_choices", design_config, app_state)
    advanced_config <- mod_advanced_choices_server("advanced_choices", app_state, experimental_config)
    effect_sizes_config <- mod_effect_sizes_server("effect_sizes", design_config, app_state)

    # Track optimization mode changes to reset plan button
    previous_mode <- reactiveVal(NULL)
    actual_plan_clicks <- reactiveVal(0)     # Track actual plan clicks (not restart clicks)
    observe({
      current_mode <- design_config()$optimization_type
      if (!is.null(current_mode) && current_mode != "" &&
          !is.null(previous_mode()) && previous_mode() != current_mode) {
        # Mode changed - reset plan counter
        actual_plan_clicks(0)  # Reset to 0 to require new plan
      }
      if (!is.null(current_mode)) {
        previous_mode(current_mode)
      }
    })

    # Static button - prevent renderUI re-creation that causes double analysis
    output$dynamic_plan_button <- renderUI({
      actionButton(ns("plan_btn"), "Plan", class = "btn-success", style = "width: 200px; max-width: 90%;")
    })

    # Instructional text for Phase 2
    output$restart_instruction_text <- renderUI({
      if (!is.null(app_state) && app_state$phase == 2) {
        tags$div(
          style = "margin-top: 10px; color: #ADB5BD; font-size: 14px; font-weight: 500;",
          "Click Restart to update sidebar selections"
        )
      } else {
        NULL
      }
    })

    # Update button text without re-rendering using updateActionButton
    observeEvent(app_state$phase, {
      if (!is.null(app_state)) {
        if(app_state$phase == 1) {
          button_text <- "Plan"
        }else{
          button_text <- "Restart"
        }
        updateActionButton(session, "plan_btn", label = button_text)
      }
    }, ignoreInit = TRUE)

    # Plan/Restart button logic
    observeEvent(input$plan_btn, {
      if (!is.null(app_state)) {
        if (app_state$phase == 1) {
          # Phase 1: Plan behavior - collapse all sections first
          shinyjs::runjs("
            // Collapse all sidebar sections
            var contentElements = document.querySelectorAll('[id$=\"-content\"]');
            var chevronElements = document.querySelectorAll('[id$=\"-chevron\"]');

            contentElements.forEach(function(el) {
              el.style.display = 'none';
            });

            chevronElements.forEach(function(el) {
              el.className = 'fa fa-chevron-right';
            });
          ")

          # Then validate configuration
          current_config <- combined_config()
          validation_result <- validate_design_configuration(current_config$design_options)

          if (!validation_result$valid) {
            # Show specific validation error
            showNotification(
              validation_result$message,
              type = "error",
              duration = 6
            )
            return()  # Don't proceed with analysis
          }

          # Only proceed if validation passes
          showNotification("Analysis starting...", type = "message", duration = 2)
          # Increment actual plan clicks counter
          actual_plan_clicks(actual_plan_clicks() + 1)
        } else if (app_state$phase == 2) {
          # Phase 2: Restart behavior - show confirmation dialog
          # DON'T increment plan clicks or trigger reactive updates
          showModal(modalDialog(
            title = "Restart Analysis",
            "This will clear all results and reset all parameters to default values. Are you sure?",
            footer = tagList(
              actionButton(ns("restart_cancel"), "Cancel", class = "btn btn-secondary"),
              actionButton(ns("restart_confirm"), "Yes, Restart", class = "btn btn-danger")
            ),
            easyClose = FALSE  # Prevent closing by clicking outside
          ))
        }
      }
    }, ignoreInit = TRUE)

    # Confirmation dialog handlers
    observeEvent(input$restart_cancel, {
      removeModal()  # Close dialog, do nothing
    })

    observeEvent(input$restart_confirm, {
      removeModal()  # Close dialog

      # Browser refresh restart - simple and bulletproof
      session$reload()
    })

    # Return configuration from parameter manager (now safe with isolate() patterns)
    combined_config <- reactive({
      # Get design and non-parameter configurations from sidebar modules
      design_opts <- design_config()

      # Get parameter values from parameter manager (single source of truth)
      config <- list(
        # Design options with complete configuration from design module
        design_options = design_opts,

        # Experimental setup from sidebar modules (independent of sliders)
        experimental_setup = experimental_config(),

        # Analysis choices from sidebar modules (independent of sliders)
        analysis_choices = analysis_config(),

        # Effect sizes from sidebar modules (independent of sliders)
        effect_sizes = effect_sizes_config(),

        # Sidebar-only configuration (not parameters)
        advanced_choices = advanced_config(),
        timestamp = Sys.time()
      )

      # OPTION B: Compute workflow_info once here (performance optimization)
      # This eliminates duplicate computation in analysis engine and enables
      # sliders to access workflow_info without circular dependencies
      config$workflow_info <- detect_workflow_scenario(config)

      # Use the actual plan clicks counter (not the raw button clicks)
      # This prevents restart dialog clicks from affecting analysis
      config$plan_clicked <- actual_plan_clicks()

      return(config)
    })

    return(combined_config)
  })
}

## To be copied in the UI
# mod_sidebar_ui("sidebar_1")

## To be copied in the server
# mod_sidebar_server("sidebar_1")
