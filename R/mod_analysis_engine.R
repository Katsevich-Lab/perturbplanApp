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
#' @importFrom shiny moduleServer reactive req bindCache debounce
#' @importFrom magrittr %>%
mod_analysis_engine_server <- function(id, workflow_config) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ========================================================================
    # DEBOUNCED CONFIG INPUT
    # ========================================================================

    # Debounce the workflow_config by 50ms to prevent excessive analysis calls
    workflow_config_debounced <- debounce(workflow_config, 10)

    # ========================================================================
    # MAIN ANALYSIS REACTIVE - PERTURBPLAN INTEGRATION
    # ========================================================================

    analysis_results <- reactive({
      # extract the debounced workflow_config
      config <- workflow_config_debounced()

      # Early validation: Skip analysis if configuration is missing (validation handled at Plan button level)
      # During UI transitions, don't run analysis - just return NULL to show "Ready for Analysis"
      design_config <- config$design_options

      # Minimal check for missing config during transitions
      if (is.null(design_config)) {
        return(NULL)  # Don't show errors during transitions - let UI show "Ready for Analysis"
      }

      # Phase-based analysis triggering
      # Phase 1: Require plan button click
      if (is.null(config$plan_clicked) || config$plan_clicked == 0) {
        return(NULL)
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

      # Use pre-computed workflow_info from sidebar (performance optimization)
      workflow_info <- config$workflow_info

      # PERTURBPLAN ANALYSIS: Call perturbplan package functions
      # Wrap in comprehensive error handling to prevent app crashes
      results <- tryCatch({
        generate_real_analysis(config, workflow_info)
      }, error = function(e) {
        # Return error object instead of crashing
        list(error = paste("Analysis Error:", e$message))
      })

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
  # Extract pilot data for perturbplan function (with gene list support)
  pilot_data <- extract_pilot_data(config$experimental_setup, config$analysis_choices)

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

  # TAP-seq parameter transformation: Convert UMIs/cell to TPM threshold
  assay_type <- config$design_options$assay_type
  if (!is.null(assay_type) && assay_type == "tap_seq") {
    # For TAP-seq: Convert UMIs/cell threshold to TPM scale
    # TPM_threshold = UMIs_per_cell / total_UMI_per_cell * 1e6
    if (!is.null(config$analysis_choices$TPM_threshold_fixed) &&
        !is.null(pilot_data$library_parameters$UMI_per_cell)) {

      umis_per_cell_threshold <- config$analysis_choices$TPM_threshold_fixed
      total_umi_per_cell <- pilot_data$library_parameters$UMI_per_cell

      # Transform to TPM scale
      config$analysis_choices$TPM_threshold_fixed <- (umis_per_cell_threshold / total_umi_per_cell) * 1e6
    }
  }

  # Clean 3-function architecture: Each analysis type has unified signature and return format

  # Workflow 5: Cost minimization
  if (workflow_info$workflow_id == "power_cost_minimization") {
    return(perform_cost_minimization_analysis(config, pilot_data))
  }

  # Workflows 10-11: Constrained minimization (TPM/FC)
  if (workflow_info$workflow_id %in% c("power_cost_TPM_cells_reads", "power_cost_fc_cells_reads")) {
    return(perform_constrained_minimization_analysis(config, pilot_data))
  }

  # Workflows 1-4, 6-9: Standard analysis
  return(perform_standard_analysis(config, pilot_data))
}
