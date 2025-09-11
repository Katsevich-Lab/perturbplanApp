#' parameter_source_manager UI Function
#'
#' @description Backend-only module for parameter coordination.
#' No UI components needed - this is a pure server-side module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_parameter_source_manager_ui <- function(id) {
  ns <- NS(id)
  tagList(
    # No UI - this is a backend parameter coordination module
  )
}

#' parameter_source_manager Server Functions
#'
#' @description Central parameter coordination hub that manages sidebar-base + slider-override logic.
#' Implements the dual-workflow system where sidebar is master configuration and sliders provide overrides.
#'
#' @param id Module namespace ID
#' @param sidebar_config Reactive containing complete sidebar configuration (master source)
#' @param slider_config Reactive containing slider parameter overrides (subset when active)
#'
#' @return Reactive containing unified parameter configuration for analysis_engine
#' @noRd
#'
#' @importFrom shiny moduleServer reactive req
mod_parameter_source_manager_server <- function(id, sidebar_config, slider_config){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    # ========================================================================
    # PARAMETER SOURCE COORDINATION
    # ========================================================================
    # Central logic for sidebar-base + slider-override parameter management

    unified_config <- reactive({
      cat("=== UNIFIED_CONFIG REACTIVE TRIGGERED ===\n")
      cat("Time:", format(Sys.time(), "%H:%M:%OS3"), "\n")

      # Get base configuration from sidebar (always required)
      base_config <- sidebar_config()
      req(base_config)
      
      # DEBUG: What's changing in the sidebar config?
      cat("Sidebar plan_clicked:", base_config$plan_clicked %||% "NULL", "\n")
      cat("Sidebar optimization_type:", base_config$design_options$optimization_type %||% "NULL", "\n")

      # Get slider overrides (may be NULL if sliders not initialized)
      slider_overrides <- slider_config()

      cat("slider_overrides is NULL:", is.null(slider_overrides), "\n")
      if (!is.null(slider_overrides)) {
        cat("slider_active flag:", isTRUE(slider_overrides$slider_active), "\n")
      }

      # Phase 1: Sidebar-only mode (sliders not initialized)
      if (is.null(slider_overrides) || !isTRUE(slider_overrides$slider_active)) {
        cat("PHASE 1: Returning sidebar config only\n")
        cat("=== END UNIFIED_CONFIG (SIDEBAR ONLY) ===\n\n")
        return(base_config)
      }

      # Phase 2: Sidebar + Slider override mode
      cat("PHASE 2: Merging sidebar + slider overrides\n")
      cat("Sample override - Cells per target:", slider_overrides$experimental_setup$cells_fixed, "\n")
      cat("Sample override - TPM:", slider_overrides$analysis_choices$TPM_threshold_fixed, "\n")

      # DEBUG: Compare sidebar vs slider values to see if they're actually different
      cat("VALUE COMPARISON:\n")
      cat("  Sidebar cells:", base_config$experimental_setup$cells_fixed, 
          "vs Slider cells:", slider_overrides$experimental_setup$cells_fixed, "\n")
      cat("  Sidebar TPM:", base_config$analysis_choices$TPM_threshold_fixed,
          "vs Slider TPM:", slider_overrides$analysis_choices$TPM_threshold_fixed, "\n")
      cat("  Values are different:", 
          base_config$experimental_setup$cells_fixed != slider_overrides$experimental_setup$cells_fixed ||
          base_config$analysis_choices$TPM_threshold_fixed != slider_overrides$analysis_choices$TPM_threshold_fixed, "\n")

      # Create merged configuration with slider overrides
      merged_config <- base_config  # Start with sidebar as base

      # Apply slider overrides to experimental_setup
      if (!is.null(slider_overrides$experimental_setup)) {
        slider_exp <- slider_overrides$experimental_setup
        merged_config$experimental_setup$MOI <- slider_exp$MOI %||% merged_config$experimental_setup$MOI
        merged_config$experimental_setup$num_targets <- slider_exp$num_targets %||% merged_config$experimental_setup$num_targets
        merged_config$experimental_setup$gRNAs_per_target <- slider_exp$gRNAs_per_target %||% merged_config$experimental_setup$gRNAs_per_target
        merged_config$experimental_setup$cells_fixed <- slider_exp$cells_fixed %||% merged_config$experimental_setup$cells_fixed
        merged_config$experimental_setup$mapped_reads_fixed <- slider_exp$mapped_reads_fixed %||% merged_config$experimental_setup$mapped_reads_fixed
      }

      # Apply slider overrides to analysis_choices
      if (!is.null(slider_overrides$analysis_choices)) {
        slider_analysis <- slider_overrides$analysis_choices
        merged_config$analysis_choices$TPM_threshold_fixed <- slider_analysis$TPM_threshold_fixed %||% merged_config$analysis_choices$TPM_threshold_fixed
      }

      # Apply slider overrides to effect_sizes
      if (!is.null(slider_overrides$effect_sizes)) {
        slider_effects <- slider_overrides$effect_sizes
        merged_config$effect_sizes$minimum_fold_change_fixed <- slider_effects$minimum_fold_change_fixed %||% merged_config$effect_sizes$minimum_fold_change_fixed
      }

      cat("=== END UNIFIED_CONFIG (MERGED) ===\n\n")
      return(merged_config)
    })

    return(unified_config)
  })
}

## To be copied in the UI
# mod_parameter_source_manager_ui("parameter_source_manager_1")

## To be copied in the server
# mod_parameter_source_manager_server("parameter_source_manager_1")
