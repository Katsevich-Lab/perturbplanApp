#' Variable Naming Standards
#' 
#' @description Canonical naming conventions for all parameters across the
#' entire application architecture. This enforces consistency from UI inputs
#' through backend processing to results display.
#'
#' @name naming_standards
#' @keywords internal
NULL

#' Canonical Parameter Names by Layer
#'
#' @description Complete specification of parameter names at each layer of
#' the application architecture, preserving essential conversion logic.
#'
#' @format List with naming conventions for each parameter across all layers
#' @export
CANONICAL_PARAMETER_NAMES <- list(
  
  # ==== READS PARAMETER (Complex 5-layer transformation) ====
  reads = list(
    # UI Input Layer - what users enter/see in input forms
    ui_input_id = "mapped_reads_per_cell",
    ui_display_label = "Mapped reads per cell",
    ui_help_text = "Number of mapped reads per cell in the experiment",
    
    # Config Layer - parameter passing between modules  
    config_key = "mapped_reads_fixed", 
    config_path = "experimental_setup$mapped_reads_fixed",
    
    # API Interface Layer - perturbplan function parameters
    perturbplan_input_param = "reads_per_cell",  # What perturbplan expects
    perturbplan_output_column = "raw_reads_per_cell",  # What perturbplan returns
    
    # Internal Processing Layer - post-perturbplan standardization
    internal_data_column = "sequenced_reads_per_cell",  # Internal standard
    results_display_label = "Optimal sequenced reads per cell",  # Solution panel
    plot_aesthetic = "sequenced_reads_per_cell"  # ggplot column name
  ),
  
  # ==== TPM PARAMETER (Simple standardization) ====
  tpm = list(
    # UI Input Layer
    ui_input_id = "TPM_threshold",  # IMPORTANT: TPM not tmp
    ui_display_label = "TPM threshold", 
    ui_help_text = "Minimum TPM threshold for gene filtering",
    
    # Config Layer
    config_key = "TPM_threshold_fixed",
    config_path = "analysis_choices$TPM_threshold_fixed",
    
    # Backend Processing Layer
    function_param = "TPM_threshold",
    data_column = "TPM_threshold",
    
    # API Layer (perturbplan)
    perturbplan_param = "TPM_threshold",  # Matches our internal name
    
    # Display Layer
    results_display_label = "Optimal TPM threshold",
    plot_aesthetic = "TPM_threshold"
  ),
  
  # ==== CELLS PARAMETER (Simple standardization) ====
  cells = list(
    # UI Input Layer  
    ui_input_id = "cells_per_target",
    ui_display_label = "Cells per target",
    ui_help_text = "Number of cells per target gene",
    
    # Config Layer
    config_key = "cells_fixed", 
    config_path = "experimental_setup$cells_fixed",
    
    # Backend Processing Layer
    function_param = "cells_per_target",
    data_column = "cells_per_target", 
    
    # API Layer (perturbplan)
    perturbplan_param = "cells_per_target",  # Matches our internal name
    
    # Display Layer
    results_display_label = "Optimal cells per target",
    plot_aesthetic = "cells_per_target"
  ),
  
  # ==== FOLD CHANGE PARAMETER (Simple standardization) ====
  fold_change = list(
    # UI Input Layer
    ui_input_id = "minimum_fold_change", 
    ui_display_label = "Minimum fold change",
    ui_help_text = "Minimum detectable fold change for power analysis",
    
    # Config Layer  
    config_key = "minimum_fold_change_fixed",
    config_path = "effect_sizes$minimum_fold_change_fixed",
    
    # Backend Processing Layer
    function_param = "minimum_fold_change",
    data_column = "minimum_fold_change",
    
    # API Layer (perturbplan)
    perturbplan_param = "minimum_fold_change",  # Matches our internal name
    
    # Display Layer
    results_display_label = "Optimal minimum fold change", 
    plot_aesthetic = "minimum_fold_change"
  )
)

#' Parameter Control Matrix Display Names
#'
#' @description Standardized display names for the parameter control matrix
#' component, ensuring consistent user-facing terminology.
#'
#' @format Named list mapping internal parameter names to display names
#' @export  
PARAMETER_DISPLAY_NAMES <- list(
  "cells_per_target" = "Cells per target",
  "mapped_reads_per_cell" = "Mapped reads per cell", 
  "TPM_threshold" = "TPM threshold",
  "minimum_fold_change" = "Minimum fold change"
)

#' Essential Data Conversion Patterns  
#'
#' @description Conversion patterns that MUST be preserved during naming
#' unification. These handle the perturbplan API interface correctly.
#'
#' @format List of conversion patterns with before/after column names
#' @export
ESSENTIAL_CONVERSIONS <- list(
  
  # perturbplan reads output → internal standard (PRESERVE THIS!)
  reads_standardization = list(
    description = "Convert perturbplan reads output to internal standard",
    pattern = '
# PRESERVE: Essential conversion pattern for reads
if ("raw_reads_per_cell" %in% names(data)) {
  data$sequenced_reads_per_cell <- data$raw_reads_per_cell
  data$raw_reads_per_cell <- NULL
} else if ("reads_per_cell" %in% names(data)) {
  data$sequenced_reads_per_cell <- data$reads_per_cell  
  data$reads_per_cell <- NULL
}',
    input_columns = c("raw_reads_per_cell", "reads_per_cell"),
    output_column = "sequenced_reads_per_cell",
    locations = c(
      "fct_cost_minimization.R",
      "fct_unified_minimization.R", 
      "mod_analysis_engine.R",
      "mod_plotting_engine.R"
    )
  )
)

#' Validate Parameter Name Usage
#'
#' @description Helper function to validate that parameter names follow
#' the canonical naming standards throughout the codebase.
#'
#' @param parameter_type One of: "reads", "tmp", "cells", "fold_change"
#' @param layer One of: "ui_input", "config", "function_param", "perturbplan", "display"
#' @param name The name to validate
#' @return Logical indicating if the name matches canonical standards
#' @export
validate_parameter_name <- function(parameter_type, layer, name) {
  if (!parameter_type %in% names(CANONICAL_PARAMETER_NAMES)) {
    return(FALSE)
  }
  
  param_spec <- CANONICAL_PARAMETER_NAMES[[parameter_type]]
  layer_key <- switch(layer,
    "ui_input" = "ui_input_id",
    "config" = "config_key", 
    "function_param" = "function_param",
    "perturbplan" = "perturbplan_param",
    "display" = "results_display_label",
    NULL
  )
  
  if (is.null(layer_key) || !layer_key %in% names(param_spec)) {
    return(FALSE)
  }
  
  return(name == param_spec[[layer_key]])
}