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
#' @param external_updates Reactive containing parameter updates from sliders (DEPRECATED)
#'
#' @noRd 
mod_sidebar_server <- function(id, param_manager){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    # Initialize module servers with parameter manager integration
    design_config <- mod_design_options_server("design_options")
    experimental_config <- mod_experimental_setup_server("experimental_setup", design_config, param_manager)
    analysis_config <- mod_analysis_choices_server("analysis_choices", design_config, param_manager)
    advanced_config <- mod_advanced_choices_server("advanced_choices")
    
    # Initialize effect sizes server with parameter manager integration
    effect_sizes_config <- mod_effect_sizes_server("effect_sizes", design_config, param_manager)
    
    # Plan button logic
    observeEvent(input$plan_btn, {
      # TODO: Trigger analysis with combined configuration
      showNotification("Plan button clicked - analysis will be implemented", type = "message")
    })
    
    # Return configuration from central parameter manager with design options integration
    combined_config <- reactive({
      # Get current values directly from parameter manager's reactiveValues
      design_opts <- design_config()
      
      # Build config structure manually to avoid reactive-within-reactive issues
      config <- list(
        # Design options with complete configuration from design module
        design_options = design_opts,
        
        # Experimental setup from parameter manager
        experimental_setup = list(
          MOI = param_manager$parameters$MOI,
          num_targets = param_manager$parameters$num_targets,
          gRNAs_per_target = param_manager$parameters$gRNAs_per_target,
          non_targeting_gRNAs = param_manager$parameters$non_targeting_gRNAs,
          cells_fixed = param_manager$parameters$cells_per_target,
          mapped_reads_fixed = param_manager$parameters$reads_per_cell
        ),
        
        # Analysis choices
        analysis_choices = list(
          TPM_threshold_fixed = param_manager$parameters$TPM_threshold
        ),
        
        # Effect sizes
        effect_sizes = list(
          minimum_fold_change_fixed = param_manager$parameters$minimum_fold_change
        ),
        
        # Sidebar-only configuration
        advanced_choices = advanced_config(),
        plan_clicked = input$plan_btn,
        timestamp = Sys.time()
      )
      
      # Update parameter controls with current parameter manager values
      if (!is.null(config$design_options) && !is.null(config$design_options$parameter_controls)) {
        config$design_options$parameter_controls$cells_per_target$fixed_value <- param_manager$parameters$cells_per_target
        config$design_options$parameter_controls$mapped_reads_per_cell$fixed_value <- param_manager$parameters$reads_per_cell
        config$design_options$parameter_controls$TPM_threshold$fixed_value <- param_manager$parameters$TPM_threshold
        config$design_options$parameter_controls$minimum_fold_change$fixed_value <- param_manager$parameters$minimum_fold_change
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
