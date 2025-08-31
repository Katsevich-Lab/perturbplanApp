#' sidebar UI Function
#'
#' @description Creates the left sidebar using modular components
#' for constraint-driven parameter inputs
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList tags div actionButton observeEvent showNotification uiOutput renderUI
#' @importFrom shinydashboard dashboardSidebar
#' @importFrom shinyjs show hide
#' @importFrom digest digest
mod_sidebar_ui <- function(id) {
  ns <- NS(id)
  
  dashboardSidebar(
    # Parameter panels - make scrollable with collapsible sections
    tags$div(
      style = "padding: 10px; max-height: 90vh; overflow-y: auto;",
      
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
      
      # Plan button
      tags$div(
        style = "text-align: center; padding: 0 20px;",
        actionButton(ns("plan_btn"), "Plan", class = "btn-success", style = "width: 200px; max-width: 90%;")
      )
    )
  )
}
    
#' sidebar Server Functions
#'
#' @description Server logic for sidebar using modular components
#' with central parameter manager integration
#'
#' @param id Module namespace ID
#' @param param_manager Parameter manager instance (central hub)
#' @param plan_state ReactiveValues containing plan click state for real-time analysis
#'
#' @noRd 
mod_sidebar_server <- function(id, param_manager, plan_state = NULL){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    # Initialize module servers with parameter manager integration
    design_config <- mod_design_options_server("design_options", param_manager)
    experimental_config <- mod_experimental_setup_server("experimental_setup", design_config, param_manager)
    analysis_config <- mod_analysis_choices_server("analysis_choices", design_config, param_manager)
    advanced_config <- mod_advanced_choices_server("advanced_choices")
    
    # Initialize effect sizes server with parameter manager integration and analysis choices
    effect_sizes_config <- mod_effect_sizes_server("effect_sizes", design_config, param_manager, analysis_config)
    
    # Track optimization mode changes to reset plan button
    previous_mode <- reactiveVal(NULL)
    plan_count_adjustment <- reactiveVal(0)  # Adjustment to subtract from plan button count
    
    observe({
      current_mode <- design_config()$optimization_type
      if (!is.null(current_mode) && current_mode != "" && 
          !is.null(previous_mode()) && previous_mode() != current_mode) {
        # Mode changed - reset plan by adjusting the count
        plan_count_adjustment(input$plan_btn)  # Store current count to subtract
      }
      if (!is.null(current_mode)) {
        previous_mode(current_mode)
      }
    })
    
    # Plan button logic
    # Plan button click handler with real-time analysis state management
    observeEvent(input$plan_btn, {
      if (!is.null(plan_state)) {
        current_config <- combined_config()
        
        # Create signature for current design problem
        if (!is.null(current_config) && !is.null(current_config$design_options)) {
          current_signature <- digest::digest(list(
            optimization_type = current_config$design_options$optimization_type,
            minimization_target = current_config$design_options$minimization_target,
            parameter_controls = if (!is.null(current_config$design_options$parameter_controls)) {
              lapply(current_config$design_options$parameter_controls, function(param) param$type)
            } else NULL
          ), algo = "md5")
          
          # Check if this is first plan click for current design problem
          if (!plan_state$first_plan_clicked || 
              !identical(plan_state$current_design_signature, current_signature)) {
            
            # First plan click for this design problem structure
            plan_state$first_plan_clicked <- TRUE
            plan_state$sliders_visible <- TRUE
            plan_state$current_design_signature <- current_signature
            
            showNotification(
              "Optimization mode activated! Sliders enabled for real-time analysis.", 
              duration = 4, 
              type = "message"
            )
          } else {
            # Subsequent plan clicks - just show analysis starting
            showNotification("Running analysis...", duration = 2, type = "message")
          }
          
          # Enable real-time mode after first click
          plan_state$real_time_enabled <- TRUE
        }
      } else {
        # Fallback for backward compatibility
        showNotification("Running analysis...", duration = 2, type = "message")
      }
    })
    
    # Return configuration from parameter manager (now safe with isolate() patterns)
    combined_config <- reactive({
      # Get design and non-parameter configurations from sidebar modules
      design_opts <- design_config()
      
      # Get parameter values from parameter manager (single source of truth)
      config <- list(
        # Design options with complete configuration from design module
        design_options = design_opts,
        
        # Experimental setup from experimental setup module (includes biological_system, pilot_data, etc.)
        experimental_setup = experimental_config(),
        
        # Analysis choices from analysis choices module (includes side, gene_list_mode, etc.)
        analysis_choices = analysis_config(),
        
        # Effect sizes from effect sizes module (includes prop_non_null)
        effect_sizes = effect_sizes_config(),
        
        # Sidebar-only configuration (not parameters)
        advanced_choices = advanced_config(),
        plan_clicked = input$plan_btn - plan_count_adjustment(),  # Reset plan count on mode change
        timestamp = Sys.time()
      )
      
      # Update parameter controls with current parameter manager values
      if (!is.null(config$design_options) && !is.null(config$design_options$parameter_controls)) {
        config$design_options$parameter_controls$cells_per_target$fixed_value <- param_manager$parameters$cells_per_target
        config$design_options$parameter_controls$sequenced_reads_per_cell$fixed_value <- param_manager$parameters$reads_per_cell
        config$design_options$parameter_controls$TPM_threshold$fixed_value <- param_manager$parameters$TPM_threshold
        config$design_options$parameter_controls$minimum_fold_change$fixed_value <- param_manager$parameters$minimum_fold_change
      }
      
      # Update cost budget with current parameter manager value (critical for slider sync)
      if (!is.null(config$design_options)) {
        config$design_options$cost_budget <- param_manager$parameters$cost_budget
      }
      
      return(config)
    })
    
    return(combined_config)
  })
}
    
## To be copied in the UI
# mod_sidebar_ui("sidebar_1")
    
## To be copied in the server
# mod_sidebar_server("sidebar_1")
