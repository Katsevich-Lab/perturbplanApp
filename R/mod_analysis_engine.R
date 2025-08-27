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
#'
#' @return Reactive list containing analysis results data
#' @noRd
#'
#' @importFrom shiny moduleServer reactive req bindCache
#' @importFrom magrittr %>%
mod_analysis_engine_server <- function(id, workflow_config) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ========================================================================
    # MAIN ANALYSIS REACTIVE - PERTURBPLAN INTEGRATION
    # ========================================================================

    # Track previous configuration to detect sidebar changes
    previous_config <- reactiveVal(NULL)
    last_plan_count <- reactiveVal(0)

    # Cache for expensive computation results
    cached_results <- reactiveVal(NULL)
    
    # Track optimization mode changes to clear cache and refresh state
    previous_optimization_mode <- reactiveVal(NULL)
    in_mode_transition <- reactiveVal(FALSE)
    
    # Clear cache when optimization mode changes
    observe({
      config <- workflow_config()
      if (!is.null(config) && !is.null(config$design_options$optimization_type)) {
        current_mode <- config$design_options$optimization_type
        
        # If mode changed, clear cached results to show "Ready for Analysis"
        if (!is.null(previous_optimization_mode()) && previous_optimization_mode() != current_mode) {
          in_mode_transition(TRUE)       # Mark as in transition
          cached_results(NULL)           # Clear cached results
          previous_config(NULL)          # Reset configuration tracking
          last_plan_count(0)             # Reset plan tracking
          # Note: We don't clear parameter controls here - that would cause UI issues
          # Instead, the early validation (lines 85-88) will return NULL during transitions
        } else {
          in_mode_transition(FALSE)      # Not in transition
        }
        
        previous_optimization_mode(current_mode)
      }
    })

    analysis_results <- reactive({
      req(workflow_config())
      
      # CRITICAL: Stop reactive execution entirely during mode transitions
      req(!in_mode_transition())

      config <- workflow_config()

      # Early validation: Skip analysis if essential configuration is missing OR incompatible
      # During UI transitions, don't run analysis - just return NULL to show "Ready for Analysis"
      # THIS MUST HAPPEN BEFORE BOTH PLAN CHECK AND CACHING LOGIC
      design_config <- config$design_options
      
      # Check for missing essential fields
      cat("=== ANALYSIS ENGINE DEBUG ===\n")
      cat("optimization_type:", design_config$optimization_type, "\n")
      cat("minimization_target:", design_config$minimization_target, "\n") 
      cat("parameter_controls is NULL:", is.null(design_config$parameter_controls), "\n")
      cat("plan_clicked:", config$plan_clicked, "\n")
      
      if (is.null(design_config$optimization_type) || design_config$optimization_type == "" ||
          is.null(design_config$minimization_target) || design_config$minimization_target == "" ||
          is.null(design_config$parameter_controls)) {
        cat("Returning NULL - incomplete configuration\n")
        cat("==============================\n")
        return(NULL)  # Don't show errors during transitions - let UI show "Ready for Analysis"
      }
      
      cat("Configuration complete - continuing to analysis\n")
      cat("===============================================\n")
      
      # Check for incompatible optimization type + minimization target combinations (transition states)
      opt_type <- design_config$optimization_type
      target <- design_config$minimization_target
      
      # Power+cost mode can only minimize TPM_threshold or minimum_fold_change
      if (opt_type == "power_cost" && !target %in% c("TPM_threshold", "minimum_fold_change")) {
        return(NULL)  # Incompatible combination during transition - show "Ready for Analysis"
      }

      # Skip analysis if plan not clicked (only after configuration is validated as compatible)
      if (is.null(config$plan_clicked) || config$plan_clicked == 0) {
        return(NULL)
      }

      # Protection mechanism: Clear results if sidebar inputs changed since last plan
      current_config_hash <- create_config_hash(config)
      previous_hash <- previous_config()
      current_plan_count <- config$plan_clicked

      # If this is a new plan click, update tracking and clear cache
      if (current_plan_count > last_plan_count()) {
        previous_config(current_config_hash)
        last_plan_count(current_plan_count)
        cached_results(NULL)  # Clear cache for new plan
      } else {
        # Check if sidebar inputs changed since last plan
        if (!is.null(previous_hash) && current_config_hash != previous_hash) {
          # Sidebar changed but no new plan click - keep showing old results
          if (!is.null(cached_results())) {
            return(cached_results())  # Show old results instead of clearing
          }
          # If no cached results, continue to analysis (shouldn't happen normally)
        }

        # Same config as before - return cached results if available
        if (!is.null(cached_results())) {
          return(cached_results())
        }
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
      results <- tryCatch({
        generate_real_analysis(config, workflow_info)
      }, error = function(e) {
        # Return error object instead of crashing
        list(
          error = paste("Analysis Error:", e$message),
          metadata = list(
            analysis_mode = get_analysis_mode(),
            workflow_type = workflow_info$workflow_id %||% "unknown",
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
    cat("=== REAL ANALYSIS ERROR ===\n")
    cat("Error:", e$message, "\n")
    cat("Returning error to user\n")
    cat("==========================\n")

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

