#' sidebar UI Function
#'
#' @description Creates the left sidebar using modular components
#' for constraint-driven parameter inputs
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList tags div actionButton observeEvent uiOutput renderUI debounce
#' @importFrom shinydashboard dashboardSidebar
#' @importFrom shinyjs show hide
#' @importFrom digest digest
# Helper function: Create design problem signature identical to app_server.R
create_design_problem_signature_local <- function(user_config) {
  if (is.null(user_config)) return(NULL)
  
  # Include ALL sidebar parameters that should trigger complete clearing
  all_sidebar_elements <- list(
    # Design options (Steps 1/2/3) - Structure AND shared parameter values
    design_options = if (!is.null(user_config$design_options)) {
      list(
        optimization_type = user_config$design_options$optimization_type,
        minimization_target = user_config$design_options$minimization_target,
        parameter_controls = if (!is.null(user_config$design_options$parameter_controls)) {
          lapply(user_config$design_options$parameter_controls, function(param) param$type)
        } else NULL,
        # Include shared parameter value
        cost_budget = user_config$design_options$cost_budget,
        # Include parameters that should trigger slider reset when changed (must match app_server.R)
        # Handle NULL values to ensure consistent signature comparison
        # Note: target_power now handled by analysis engine observer (Location 1A) for clean transitions
        cost_per_cell = user_config$design_options$cost_per_cell %||% "not_applicable",
        cost_per_million_reads = user_config$design_options$cost_per_million_reads %||% "not_applicable"
      )
    } else NULL,
    
    # Experimental setup - ALL parameters (shared + non-shared)
    experimental_setup = if (!is.null(user_config$experimental_setup)) {
      list(
        # Non-shared parameters
        biological_system = user_config$experimental_setup$biological_system,
        pilot_data_choice = user_config$experimental_setup$pilot_data_choice,
        non_targeting_gRNAs = user_config$experimental_setup$non_targeting_gRNAs,
        
        # Shared parameters
        MOI = user_config$experimental_setup$MOI,
        num_targets = user_config$experimental_setup$num_targets,
        gRNAs_per_target = user_config$experimental_setup$gRNAs_per_target,
        cells_fixed = user_config$experimental_setup$cells_fixed,
        sequenced_reads_fixed = user_config$experimental_setup$sequenced_reads_fixed
      )
    } else NULL,
    
    # Analysis choices - ALL parameters (shared + non-shared)
    analysis_choices = if (!is.null(user_config$analysis_choices)) {
      list(
        # Non-shared parameters
        side = user_config$analysis_choices$side,
        gene_list_mode = user_config$analysis_choices$gene_list_mode,
        
        # Shared parameter
        TPM_threshold_fixed = user_config$analysis_choices$TPM_threshold_fixed
      )
    } else NULL,
    
    # Effect sizes - ALL parameters (shared + non-shared)
    effect_sizes = if (!is.null(user_config$effect_sizes)) {
      list(
        # Non-shared parameter
        prop_non_null = user_config$effect_sizes$prop_non_null,
        
        # Shared parameter
        minimum_fold_change_fixed = user_config$effect_sizes$minimum_fold_change_fixed
      )
    } else NULL,
    
    # Advanced choices - ALL parameters (none are shared with sliders)
    advanced_choices = if (!is.null(user_config$advanced_choices)) {
      list(
        gRNA_variability = user_config$advanced_choices$gRNA_variability,
        mapping_efficiency = user_config$advanced_choices$mapping_efficiency,
        control_group = user_config$advanced_choices$control_group,
        fdr_target = user_config$advanced_choices$fdr_target
      )
    } else NULL
  )
  
  # Create signature hash of ALL sidebar elements
  return(digest::digest(all_sidebar_elements, algo = "md5"))
}

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
    
    # COMMENTED OUT FOR TESTING: Location 1B - Sidebar mode observer
    # This observer may be unnecessary since Location 1A (analysis engine) already handles
    # optimization_type changes with in_mode_transition(TRUE). Commenting out to test if
    # step 1 can work like step 2 without Plan state resets.
    
    # # Use observeEvent with intelligent filtering to prevent excessive firing
    # observeEvent({
    #   config <- design_config()
    #   # Only return the optimization_type, ignore other config changes
    #   if (!is.null(config)) config$optimization_type else NULL
    # }, {
    #   current_mode <- design_config()$optimization_type
    #   
    #   # OPTIMIZATION: Skip processing if no actual change occurred
    #   if (is.null(current_mode) || current_mode == "" || 
    #       identical(current_mode, previous_mode())) {
    #     return()  # Early exit - prevents excessive firing
    #   }
    #   
    #   
    #   # Mode changed - reset plan state WITHOUT creating fake plan triggers
    #   if (!is.null(plan_state)) {
    #     # Reset plan state for new optimization mode
    #     plan_state$reset_plan_state <- TRUE   # Signal analysis engine to reset tracking
    #     # CRITICAL: Clear ALL Plan button tracking to prevent auto-collapse race condition
    #     plan_state$waiting_for_plan_result <- FALSE
    #     cat("[DEBUG] Mode changed - waiting_for_plan_result reset to FALSE\n")
    #     plan_state$has_plan_been_clicked <- FALSE
    #   }
    #   plan_count_adjustment(0)  # Reset adjustment for backward compatibility
    #   
    #   # Update previous mode tracking
    #   previous_mode(current_mode)
    # }, priority = 100, ignoreInit = TRUE)  # High priority + ignore initial firing
    
    # Plan button logic
    # Plan button click handler with real-time analysis state management
    observeEvent(input$plan_btn, {
      if (!is.null(plan_state)) {
        # Prevent multiple clicks on same configuration  
        if (plan_state$has_plan_been_clicked) {
          return()  # Ignore subsequent clicks until config changes
        }
        
        # Track user Plan button click for auto-collapse detection
        plan_state$waiting_for_plan_result <- TRUE
        
        # Set plan clicked flag to trigger analysis
        plan_state$has_plan_been_clicked <- TRUE
        
        current_config <- combined_config()
        
        # Create signature for current design problem
        if (!is.null(current_config)) {
          # Use the SAME comprehensive signature as app_server.R
          current_signature <- create_design_problem_signature_local(current_config)
          
          # Check if this is first plan click for current design problem
          if (!plan_state$sliders_visible || 
              !identical(plan_state$current_design_signature, current_signature)) {
            
            # First plan click for this design problem structure
            plan_state$sliders_visible <- TRUE
            plan_state$current_design_signature <- current_signature
            
          } else {
            # Subsequent plan clicks - just show analysis starting
          }
          
          # Don't enable real-time mode yet - wait for first slider interaction
          # plan_state$real_time_enabled <- TRUE  # MOVED to slider module
        }
      } else {
        # Fallback for backward compatibility
      }
    })
    
    # Return configuration from parameter manager (now safe with isolate() patterns)
    combined_config <- reactive({
      # Get design and non-parameter configurations from sidebar modules
      design_opts <- design_config()
      
      # Get parameter manager state to include source information
      param_manager_state <- param_manager$combined_config()
      
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
        
        # Plan click state for analysis engine reactive dependency
        plan_clicked_flag = plan_state$has_plan_been_clicked,
        timestamp = Sys.time(),
        
        # CRITICAL: Pass through source information from parameter manager
        last_parameter_source = param_manager_state$last_updated_by,
        last_parameter_timestamp = param_manager_state$timestamp
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
    }) %>% debounce(100)
    
    return(combined_config)
  })
}
    
## To be copied in the UI
# mod_sidebar_ui("sidebar_1")
    
## To be copied in the server
# mod_sidebar_server("sidebar_1")
