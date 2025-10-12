#' Tooltip Content Library
#'
#' @description Centralized repository of tooltip texts for UI elements.
#' Content extracted from vignette first sentences for consistency with documentation.
#'
#' @name tooltip-content
NULL

#' Get Tooltip Text for UI Element
#'
#' @description Retrieves tooltip text for a given UI element ID.
#' Tooltips are extracted from the first sentence of each parameter's
#' vignette documentation to ensure consistency.
#'
#' @param element_id Character. The ID of the UI element
#'
#' @return Character. The tooltip text, or empty string if not found
#'
#' @noRd
get_tooltip_text <- function(element_id) {
  tooltips <- list(
    # ==========================================================================
    # Design Problem Section
    # ==========================================================================

    # Target Power
    target_power = "The desired probability of detecting a true effect if it exists.",

    # Cost Budget (Power + Cost workflows only)
    cost_budget = "Maximum budget constraint for your experiment (in dollars).",

    # Cost Parameters
    cost_per_cell = "The cost to capture and prepare a single cell for sequencing (in dollars/cell).",
    cost_per_million_reads = "The cost to generate one million sequencing reads (in $/million reads).",

    # ==========================================================================
    # Experimental Choices Section
    # ==========================================================================

    # Reference Expression Data
    reference_expression_data = "Expression data used to calculate statistical power.",

    # Multiplicity of Infection (MOI)
    moi = "The average number of sgRNA-carrying lentiviral particles that infect a single cell.",

    # Number of Targets
    num_targets = "The total number of genomic elements you plan to perturb in your screen.",

    # gRNAs per Target
    grnas_per_target = "The number of different guide RNAs designed to perturb each target.",

    # Non-targeting gRNAs
    non_targeting_grnas = "The number of control guide RNAs that do not target any genomic element.",

    # Cells per Target
    cells_per_target = "The averaged number of cells receiving gRNAs targeting the same element.",

    # Sequenced Reads per Cell
    sequenced_reads_per_cell = "The averaged number of reads per cell when sequencing.",

    # ==========================================================================
    # Analysis Choices Section
    # ==========================================================================

    # Perturbation-Gene Pairs to Analyze
    perturbation_gene_pairs = "Method to construct the analysis set of perturbation-gene pairs for power calculation.",

    # Test Side
    test_side = "Sideness of the hypothesis test when inferring the perturbation effect.",

    # TPM Analysis Threshold
    tpm_threshold = "Transcripts per million, scale for expression threshold when Perturb-seq experiment is planned.",

    # UMIs/cell at Saturation
    umis_per_cell = "UMIs per cell when sequencing is saturated, scale for expression threshold when TAP-seq experiment is planned.",

    # ==========================================================================
    # Effect Sizes Section
    # ==========================================================================

    # Fold Change
    fold_change = "The minimum effect size of interest, expressed as a multiplicative fold change in gene expression.",

    # Proportion of Non-null Pairs
    non_null_proportion = "The fraction of perturbation-gene pairs expected to have signal greater than the minimum effect size of interest.",

    # ==========================================================================
    # Advanced Settings Section
    # ==========================================================================

    # gRNA Variability
    grna_variability = "Effect size variability of multiple gRNAs targeting the same genomic element.",

    # Mapping Efficiency
    mapping_efficiency = "The ratio of sequencing reads that map confidently to the genes of interest.",

    # Control Group
    control_group = "The strategy used to construct control cells for baseline expression and statistical comparisons.",

    # FDR Target Level
    fdr_target = "Pre-specified level for false discovery rate control."
  )

  # Return tooltip text or empty string if not found
  tooltips[[element_id]] %||% ""
}

#' Get All Available Tooltip IDs
#'
#' @description Returns a character vector of all available tooltip IDs
#' for validation and testing purposes.
#'
#' @return Character vector of tooltip IDs
#'
#' @noRd
get_available_tooltip_ids <- function() {
  c(
    # Design Problem
    "target_power", "cost_budget", "cost_per_cell", "cost_per_million_reads",

    # Experimental Choices
    "reference_expression_data", "moi", "num_targets", "grnas_per_target",
    "non_targeting_grnas", "cells_per_target", "sequenced_reads_per_cell",

    # Analysis Choices
    "perturbation_gene_pairs", "test_side", "tpm_threshold", "umis_per_cell",

    # Effect Sizes
    "fold_change", "non_null_proportion",

    # Advanced Settings
    "grna_variability", "mapping_efficiency", "control_group", "fdr_target"
  )
}

#' Validate Tooltip ID
#'
#' @description Checks if a tooltip ID exists in the tooltip library
#'
#' @param element_id Character. The ID to validate
#'
#' @return Logical. TRUE if ID exists, FALSE otherwise
#'
#' @noRd
validate_tooltip_id <- function(element_id) {
  element_id %in% get_available_tooltip_ids()
}
