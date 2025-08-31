#' Analysis Engine Module UI Function
#'
#' @description Backend-only module that generates analysis data.
#' No UI components needed - this is a pure server-side module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_analysis_engine_ui <- function(id) {
  ns <- NS(id)
  tagList(
    # No UI - this is a backend analysis module
  )
}

#' Analysis Engine Server Functions
#'
#' @description Generates real analysis results using perturbplan package functions.
#' Handles all workflow scenarios with comprehensive error handling and caching.
#'
#' @param id Module namespace ID
#' @param workflow_config Reactive containing complete user configuration
#' @param param_manager Parameter manager instance for real-time analysis triggers
#'
#' @return Reactive list containing analysis results data
#' @noRd
#'
#' @importFrom shiny moduleServer reactive req bindCache showNotification
#' @importFrom magrittr %>%
mod_analysis_engine_server <- function(id, workflow_config, param_manager = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ========================================================================
    # MAIN ANALYSIS REACTIVE - PERTURBPLAN INTEGRATION
    # ========================================================================

    # Track previous configuration to detect sidebar changes
    previous_config_hash <- reactiveVal(NULL)
    previous_config_object <- reactiveVal(NULL)
    last_plan_count <- reactiveVal(0)
    
    # PHASE 1 FIX: Track last real-time trigger to prevent duplicate analysis
    last_trigger_count <- reactiveVal(0)
    
    # Track the last processed plan count AND timestamp to detect duplicates
    last_processed_plan <- reactiveVal(list(count = 0, time = NULL))

    # Cache for expensive computation results
    cached_results <- reactiveVal(NULL)
    
    # Track design option changes to clear cache and refresh state
    previous_design_options <- reactiveVal(NULL)
    in_mode_transition <- reactiveVal(FALSE)
    
    # Clear cache when any design options change (optimization type, minimization target, parameter controls)
    observe({
      config <- workflow_config()
      if (!is.null(config) && !is.null(config$design_options)) {
        # Track all key design options that should trigger plot refresh
        current_design <- list(
          optimization_type = config$design_options$optimization_type,
          minimization_target = config$design_options$minimization_target,
          parameter_control_types = if (!is.null(config$design_options$parameter_controls)) {
            lapply(config$design_options$parameter_controls, function(control) {
              list(type = control$type)  # Only track the type, not fixed_value
            })
          } else {
            NULL
          }
        )
        
        # If any design options changed, clear cached results and stay in transition until user acts
        if (!is.null(previous_design_options()) && !identical(previous_design_options(), current_design)) {
          in_mode_transition(TRUE)       # Mark as in transition - will stay TRUE until explicit user action
          cached_results(NULL)           # Clear cached results
          previous_config_hash(NULL)     # Reset configuration tracking
          previous_config_object(NULL)   # Reset configuration object
          # DON'T reset last_plan_count - this prevents old plan clicks from being detected as new triggers
          # last_plan_count(0)           # This was causing the bug!
          # Note: We don't clear parameter controls here - that would cause UI issues
          # Note: We don't set in_mode_transition(FALSE) here - user must explicitly trigger analysis
        }
        
        previous_design_options(current_design)
      }
    })

    analysis_results <- reactive({
      req(workflow_config())
      
      config <- workflow_config()
      
      
      # PHASE 1 FIX (ENHANCED): Establish dependencies on both triggers but process only one
      # Create dependencies (these must be outside conditionals to work properly)
      current_plan_count <- config$plan_clicked
      current_trigger_count <- if (!is.null(param_manager) && !is.null(param_manager$analysis_trigger)) {
        param_manager$analysis_trigger()
      } else {
        0
      }
      
      
      # Check if this is a valid NEW trigger that should exit transition mode
      has_new_plan_click <- (current_plan_count > last_plan_count())
      has_new_real_time_trigger <- (current_trigger_count > last_trigger_count())
      has_valid_trigger <- has_new_plan_click || has_new_real_time_trigger
      
      
      # EARLY EXIT: If in transition mode AND no valid trigger, return NULL
      if (in_mode_transition() && !has_valid_trigger) {
        return(NULL)
      }
      
      # Track config hash to detect spurious invalidations
      current_config_hash <- create_config_hash(config)
      

      # Early validation: Skip analysis if essential configuration is missing OR incompatible
      # During UI transitions, don't run analysis - just return NULL to show "Ready for Analysis"
      # THIS MUST HAPPEN BEFORE BOTH PLAN CHECK AND CACHING LOGIC
      design_config <- config$design_options
      
      # Check for missing essential fields
      if (is.null(design_config$optimization_type) || design_config$optimization_type == "" ||
          is.null(design_config$minimization_target) || design_config$minimization_target == "" ||
          is.null(design_config$parameter_controls)) {
        return(NULL)  # Don't show errors during transitions - let UI show "Ready for Analysis"
      }
      
      # Check for incompatible optimization type + minimization target combinations (transition states)
      opt_type <- design_config$optimization_type
      target <- design_config$minimization_target
      
      # Power+cost mode can only minimize TPM_threshold or minimum_fold_change
      if (opt_type == "power_cost" && !target %in% c("TPM_threshold", "minimum_fold_change")) {
        return(NULL)  # Incompatible combination during transition - show "Ready for Analysis"
      }
      
      # PHASE 1 FIX (ENHANCED): Determine trigger source (only one can be active at a time)
      is_plan_click <- FALSE
      is_real_time_trigger <- FALSE
      is_spurious_invalidation <- FALSE
      
      # Check for Plan button click (takes priority)
      if (current_plan_count > last_plan_count()) {
        
        # Check if we just processed this plan click (within 1 second)
        last_processed <- last_processed_plan()
        if (last_processed$count == current_plan_count && 
            !is.null(last_processed$time) &&
            difftime(Sys.time(), last_processed$time, units = "secs") < 1) {
          return(cached_results())
        }
        
        is_plan_click <- TRUE
        last_plan_count(current_plan_count)
        # Reset trigger counter to prevent immediate real-time trigger
        last_trigger_count(current_trigger_count)
        # Update config hash for this plan click
        previous_config_hash(current_config_hash)
        # Mark this plan as being processed
        last_processed_plan(list(count = current_plan_count, time = Sys.time()))
      }
      # Check for real-time trigger (only if NOT a plan click)
      else if (current_trigger_count > last_trigger_count()) {
        is_real_time_trigger <- TRUE
        last_trigger_count(current_trigger_count)
        # Update config hash for this real-time trigger
        previous_config_hash(current_config_hash)
      }
      # Check if this is just a spurious config invalidation
      else if (!is.null(previous_config_hash()) && 
               current_config_hash == previous_config_hash()) {
        # Config content hasn't actually changed - this is a spurious invalidation
        is_spurious_invalidation <- TRUE
      } else {
      }
      
      # Skip analysis if neither trigger is active
      if (!is_plan_click && !is_real_time_trigger) {
        # Always return cached results if available when no explicit trigger
        if (!is.null(cached_results())) {
          if (is_spurious_invalidation) {
          } else {
          }
          return(cached_results())
        }
        
        # Check if we have initial plan but no triggers yet
        if (current_plan_count == 0) {
          return(NULL)  # No plan clicked yet
        }
        
        return(NULL)  # No cached results and no triggers
      }

      # Clear cache appropriately based on trigger type
      if (is_plan_click) {
        # Plan click: always clear cache and update tracking
        previous_config_object(config)
        cached_results(NULL)  # Clear cache for new plan
        in_mode_transition(FALSE)  # Exit transition mode - user explicitly triggered analysis
      } else if (is_real_time_trigger) {
        # Real-time trigger: clear cache for fresh analysis
        cached_results(NULL)
        previous_config_object(config)             # Update stored config
        in_mode_transition(FALSE)  # Ensure we're not in transition mode
      }
      
      # Check if we're still in transition mode after handling plan clicks and live params
      if (in_mode_transition()) {
        return(NULL)  # Don't run analysis during design transitions
      }

      # Validate configuration (only after essential fields are present)
      validation <- validate_workflow_config(config)
      if (!validation$is_valid) {
        return(list(
          error = paste("Configuration Error:", paste(validation$errors, collapse = ", ")),
          metadata = list(
            analysis_mode = get_analysis_mode(),
            timestamp = Sys.time()
          )
        ))
      }

      # ============================================================================
      # CENTRALIZED PARAMETER TRANSLATION: UI → Backend
      # ============================================================================
      # Standardize UI parameter names to backend-compatible names before analysis
      # This happens AFTER UI processing but BEFORE backend analysis functions
      if (!is.null(config$design_options$minimization_target)) {
        config$design_options$minimization_target <- switch(
          config$design_options$minimization_target,
          "cells" = "cells_per_target",           # "Cells per target" → backend
          "reads" = "reads_per_cell",             # "Reads per cell" → backend
          "fold_change" = "minimum_fold_change",  # "Fold change" → backend
          config$design_options$minimization_target  # No change for TPM_threshold, cost
        )
      }
      
      # Detect workflow scenario (with translated parameter names)
      workflow_info <- detect_workflow_scenario(config)
      
      # Skip analysis if workflow detection failed (prevents invalid configurations)
      # Return NULL during transitions to show "Ready for Analysis" instead of errors
      if (!is.null(workflow_info$workflow_id) && workflow_info$workflow_id == "unknown") {
        return(NULL)  # Let UI show "Ready for Analysis" instead of error messages
      }

      # PERTURBPLAN ANALYSIS: Call perturbplan package functions
      # Wrap in comprehensive error handling to prevent app crashes
      
      # PHASE 1 FIX: Use the determined trigger type for error messages
      results <- tryCatch({
        generate_real_analysis(config, workflow_info)
      }, error = function(e) {
        # Return error object instead of crashing
        # Provide more context based on trigger type
        error_prefix <- if (is_real_time_trigger) {
          "Real-time Analysis Error:"
        } else {
          "Analysis Error:"
        }
        
        list(
          error = paste(error_prefix, e$message),
          metadata = list(
            analysis_mode = get_analysis_mode(),
            workflow_type = workflow_info$workflow_id %||% "unknown",
            is_real_time = is_real_time_trigger,
            timestamp = Sys.time(),
            error_details = as.character(e)
          )
        )
      })

      # Cache the results to prevent duplicate computation
      cached_results(results)
      
      return(results)
    })

    return(analysis_results)
  })
}



# ============================================================================
# PERTURBPLAN INTEGRATION - PRODUCTION ANALYSIS ENGINE
# ============================================================================

#' Generate real analysis results using perturbplan package
#'
#' @description Production analysis engine with full perturbplan integration.
#' Handles all 11 workflow scenarios with real mathematical optimization.
#'
#' @param config User configuration from sidebar modules
#' @param workflow_info Detected workflow information
#'
#' @return List containing real analysis results (same structure as placeholder)
#' @noRd
generate_real_analysis <- function(config, workflow_info) {
  # Extract pilot data for perturbplan function
  pilot_data <- extract_pilot_data(config$experimental_setup)

  # Handle case where pilot data is not available
  if (is.null(pilot_data)) {
    return(list(
      error = "Pilot data required for real analysis. Please upload reference expression data or use built-in data.",
      metadata = list(
        analysis_mode = "Real Analysis (Data Missing)",
        timestamp = Sys.time()
      )
    ))
  }

  # Check if this is cost minimization workflow (Workflow 5)
  if (workflow_info$workflow_id == "power_cost_minimization") {
    # Use specialized cost minimization analysis
    tryCatch({
      results <- perform_cost_minimization_analysis(config, workflow_info, pilot_data)

      # Return results directly (already in plotting format)
      return(results)

    }, error = function(e) {
      stop("Cost minimization analysis failed: ", e$message)
    })
  }

  # Check if this is TPM or FC minimization workflow (Workflows 10-11)
  if (workflow_info$workflow_id %in% c("power_cost_TPM_cells_reads", "power_cost_fc_cells_reads")) {
    # Use unified constrained minimization analysis
    tryCatch({
      results <- perform_constrained_minimization_analysis(config, workflow_info, pilot_data)
      
      # Return results directly (already in plotting format)
      return(results)

    }, error = function(e) {
      stop("Constrained minimization analysis failed: ", e$message)
    })
  }

  # For all other workflows: Use standard cost_power_computation
  # Map UI configuration to perturbplan::cost_power_computation parameters
  perturbplan_params <- map_config_to_perturbplan_params(config, workflow_info, pilot_data)

  # Call perturbplan::cost_power_computation
  tryCatch({
    results <- do.call(perturbplan::cost_power_computation, perturbplan_params)

    # Standardize perturbplan output column names to sequenced_reads_per_cell
    if ("raw_reads_per_cell" %in% names(results)) {
      results$sequenced_reads_per_cell <- results$raw_reads_per_cell
      results$raw_reads_per_cell <- NULL
    } else if ("reads_per_cell" %in% names(results)) {
      results$sequenced_reads_per_cell <- results$reads_per_cell
      results$reads_per_cell <- NULL
    }

    # Convert perturbplan results to our standardized format
    standardized_results <- standardize_perturbplan_results(results, config, workflow_info)

    # NEW: Transform to plotting-compatible format
    if (!is.null(standardized_results$error)) {
      return(standardized_results)  # Return error as-is
    }

    plotting_results <- transform_perturbplan_to_plotting_format(
      standardized_results, config, workflow_info
    )

    return(plotting_results)

  }, error = function(e) {
    # Return error object to display to user instead of falling back

    return(list(
      error = e$message,
      metadata = list(
        analysis_mode = get_analysis_mode(),
        workflow_type = workflow_info$workflow_id %||% "unknown",
        timestamp = Sys.time(),
        error_details = as.character(e)
      )
    ))
  })
}


# ============================================================================
# UTILITY FUNCTIONS
# ============================================================================

#' Create configuration hash for change detection
#'
#' @description Creates a hash of the sidebar configuration to detect changes.
#' Excludes plan_clicked and timestamp to focus on actual parameter changes.
#'
#' @param config User configuration from sidebar modules
#' @return Character string representing configuration hash
#' @noRd
create_config_hash <- function(config) {
  # Extract relevant configuration excluding plan_clicked and timestamp
  config_for_hash <- list(
    design_options = config$design_options,
    perturbation_choices = config$perturbation_choices,
    experimental_setup = config$experimental_setup,
    analysis_choices = config$analysis_choices,
    effect_sizes = config$effect_sizes,
    advanced_choices = config$advanced_choices
  )

  # Create hash using digest (assuming digest package is available)
  # If digest not available, use simple serialization
  tryCatch({
    digest::digest(config_for_hash, algo = "md5")
  }, error = function(e) {
    # Fallback: use serialization and simple hash
    paste(collapse = "", as.character(serialize(config_for_hash, NULL)))
  })
}

#' Detect if only live parameters changed between configurations
#'
#' @description Compares two configuration objects to determine if only
#' "live" parameters (prop_non_null, fdr_target) changed, which should
#' trigger immediate re-analysis without requiring Plan button click.
#'
#' @param current_config Current configuration object
#' @param previous_config Previous configuration object
#' @return Boolean indicating if only live parameters changed
#' @noRd
detect_live_parameter_change <- function(current_config, previous_config) {
  # If no previous config, this isn't a live parameter change
  if (is.null(previous_config)) {
    return(FALSE)
  }
  
  # Check if live parameters changed
  prop_non_null_changed <- !identical(
    current_config$effect_sizes$prop_non_null, 
    previous_config$effect_sizes$prop_non_null
  )
  
  fdr_target_changed <- !identical(
    current_config$advanced_choices$fdr_target, 
    previous_config$advanced_choices$fdr_target
  )
  
  mapping_efficiency_changed <- !identical(
    current_config$advanced_choices$mapping_efficiency, 
    previous_config$advanced_choices$mapping_efficiency
  )
  
  # If no live parameters changed, return FALSE
  if (!prop_non_null_changed && !fdr_target_changed && !mapping_efficiency_changed) {
    return(FALSE)
  }
  
  # If live parameters changed, check if ONLY live parameters changed
  # Create copies without live parameters to compare everything else
  current_copy <- current_config
  previous_copy <- previous_config
  
  # Remove live parameters from both configs
  if (!is.null(current_copy$effect_sizes)) {
    current_copy$effect_sizes$prop_non_null <- NULL
  }
  if (!is.null(current_copy$advanced_choices)) {
    current_copy$advanced_choices$fdr_target <- NULL
    current_copy$advanced_choices$mapping_efficiency <- NULL
  }
  if (!is.null(previous_copy$effect_sizes)) {
    previous_copy$effect_sizes$prop_non_null <- NULL
  }
  if (!is.null(previous_copy$advanced_choices)) {
    previous_copy$advanced_choices$fdr_target <- NULL
    previous_copy$advanced_choices$mapping_efficiency <- NULL
  }
  
  # Compare hashes of configs without live parameters
  current_hash_no_live <- create_config_hash(current_copy)
  previous_hash_no_live <- create_config_hash(previous_copy)
  
  # If configs are identical without live parameters, then only live parameters changed
  return(current_hash_no_live == previous_hash_no_live)
}

