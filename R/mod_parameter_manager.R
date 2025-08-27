#' parameter_manager UI Function
#'
#' @description Central parameter management module - no UI needed
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList moduleServer reactive reactiveValues observe
mod_parameter_manager_ui <- function(id) {
  ns <- NS(id)
  tagList(
    # No UI - this is a backend-only module
  )
}

#' parameter_manager Server Functions
#'
#' @description Central hub for all parameter values. Both sidebar and sliders
#' feed INTO this module, and this module updates all UI components.
#'
#' @noRd
mod_parameter_manager_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # ========================================================================
    # CENTRAL PARAMETER STATE - Single Source of Truth
    # ========================================================================
    
    parameters <- reactiveValues(
      # Experimental parameters
      MOI = 10,
      num_targets = 100,
      gRNAs_per_target = 4,
      non_targeting_gRNAs = 10,
      
      # Power-determining parameters
      cells_per_target = 1000,
      reads_per_cell = 5000,
      TPM_threshold = 10,
      minimum_fold_change = 0.8,
      
      # Meta information
      last_updated_by = "system",
      last_updated_at = Sys.time()
    )
    
    # ========================================================================
    # INPUT COLLECTION - Multiple Sources → One Destination
    # ========================================================================
    
    # Central function for parameter updates from any source
    update_parameter <- function(param_name, value, source = "unknown") {
      if (!is.null(value) && !is.na(value) && 
          !identical(parameters[[param_name]], value)) {
        
        parameters[[param_name]] <- value
        parameters$last_updated_by <- source
        parameters$last_updated_at <- Sys.time()
        
        # Optional: Log for debugging
        # cat(sprintf("[%s] %s updated %s to %s\n", 
        #            Sys.time(), source, param_name, value))
      }
    }
    
    # ========================================================================
    # UI UPDATE COORDINATION - DISABLED to prevent infinite loops
    # ========================================================================
    
    # UI updates will be handled by individual reactive observers in each module
    # that watch the parameter manager's reactive values directly
    
    # Placeholder for potential future targeted updates
    register_ui_updater <- function(component_name, updater_function) {
      # Currently disabled to prevent circular dependencies
      return(NULL)
    }
    
    # ========================================================================
    # COMPATIBILITY LAYER - For Existing Analysis Engine
    # ========================================================================
    
    # Generate config in format expected by existing analysis engine
    combined_config <- reactive({
      list(
        # Design options with parameter controls
        design_options = list(
          parameter_controls = list(
            cells_per_target = list(fixed_value = parameters$cells_per_target),
            mapped_reads_per_cell = list(fixed_value = parameters$reads_per_cell),
            TPM_threshold = list(fixed_value = parameters$TPM_threshold),
            minimum_fold_change = list(fixed_value = parameters$minimum_fold_change)
          )
        ),
        
        # Experimental setup
        experimental_setup = list(
          MOI = parameters$MOI,
          num_targets = parameters$num_targets,
          gRNAs_per_target = parameters$gRNAs_per_target,
          non_targeting_gRNAs = parameters$non_targeting_gRNAs,
          cells_fixed = parameters$cells_per_target,
          mapped_reads_fixed = parameters$reads_per_cell
        ),
        
        # Analysis choices
        analysis_choices = list(
          TPM_threshold_fixed = parameters$TPM_threshold
        ),
        
        # Effect sizes
        effect_sizes = list(
          minimum_fold_change_fixed = parameters$minimum_fold_change
        ),
        
        # Meta
        timestamp = parameters$last_updated_at,
        last_updated_by = parameters$last_updated_by
      )
    })
    
    # ========================================================================
    # PUBLIC API
    # ========================================================================
    
    return(list(
      # Core functions
      update_parameter = update_parameter,
      register_ui_updater = register_ui_updater,
      
      # Data access
      parameters = parameters,
      combined_config = combined_config,
      
      # Direct parameter access (reactive)
      get_parameter = function(param_name) {
        reactive({ parameters[[param_name]] })
      }
    ))
  })
}
    
## To be copied in the UI
# mod_parameter_manager_ui("parameter_manager_1")
    
## To be copied in the server
# mod_parameter_manager_server("parameter_manager_1")
