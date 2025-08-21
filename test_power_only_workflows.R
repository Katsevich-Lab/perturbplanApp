# Comprehensive test script for power-only workflows
# Tests all 4 minimizing variables: cells_per_target, reads_per_cell, TPM_threshold, minimum_fold_change

# Load required libraries
library(perturbplan)

# Source the integration functions directly
source("R/fct_perturbplan_integration.R")
source("R/fct_pilot_data_validation.R")

cat("=== Testing All Power-Only Workflows ===\n\n")

# Test function for each workflow
test_power_only_workflow <- function(minimizing_param, workflow_name, fixed_fc = 1.2) {
  cat(sprintf("--- Testing: %s ---\n", workflow_name))
  
  # Create test configuration
  test_config <- list(
    design_options = list(
      optimization_type = "power_only",
      minimization_target = minimizing_param,
      target_power = 0.8,
      parameter_controls = list(
        cells_per_target = list(type = if(minimizing_param == "cells_per_target") "minimizing" else "fixed"),
        reads_per_cell = list(type = if(minimizing_param == "reads_per_cell") "minimizing" else "fixed"),
        TPM_threshold = list(type = if(minimizing_param == "TPM_threshold") "minimizing" else "fixed"),
        min_fold_change = list(type = if(minimizing_param == "min_fold_change") "minimizing" else "fixed")
      )
    ),
    experimental_setup = list(
      biological_system = "K562",
      pilot_data = list(type = "default", biological_system = "K562"),
      MOI = 10,
      num_targets = 100,
      gRNAs_per_target = 4,
      non_targeting_gRNAs = 10,
      # Fixed values for when not minimizing
      cells_fixed = if(minimizing_param != "cells_per_target") 1000 else NULL,
      reads_fixed = if(minimizing_param != "reads_per_cell") 5000 else NULL
    ),
    analysis_choices = list(
      control_group = "complement",
      side = "left",
      fdr_target = 0.05,
      TPM_threshold = if(minimizing_param != "TPM_threshold") 10 else NULL  # Fixed TPM threshold when not minimizing
    ),
    effect_sizes = list(
      fc_sd = 0.15,
      prop_non_null = 0.1,
      minimum_fold_change = fixed_fc  # Fixed fold change value
    )
  )
  
  # Simulate workflow detection
  workflow_info <- list(
    scenario = paste0("power_only_minimize_", minimizing_param),
    plot_type = "power_curve",
    minimizing_parameter = minimizing_param,
    optimization_type = "power_only"
  )
  
  tryCatch({
    # Test pilot data extraction
    pilot_data_result <- extract_pilot_data(test_config$experimental_setup)
    if (is.null(pilot_data_result)) {
      cat("  ✗ Failed to extract pilot data\n")
      return(FALSE)
    }
    
    # Test parameter mapping
    perturbplan_params <- map_config_to_perturbplan_params(test_config, workflow_info)
    cat(sprintf("  ✓ Parameter mapping: minimizing_variable = %s\n", perturbplan_params$minimizing_variable))
    
    # Test cost_power_computation
    results <- perturbplan::cost_power_computation(
      minimizing_variable = perturbplan_params$minimizing_variable,
      fixed_variable = perturbplan_params$fixed_variable,
      MOI = perturbplan_params$MOI,
      num_targets = perturbplan_params$num_targets,
      non_targeting_gRNAs = perturbplan_params$non_targeting_gRNAs,
      gRNAs_per_target = perturbplan_params$gRNAs_per_target,
      gRNA_variability = perturbplan_params$gRNA_variability,
      prop_non_null = perturbplan_params$prop_non_null,
      control_group = perturbplan_params$control_group,
      side = perturbplan_params$side,
      multiple_testing_alpha = perturbplan_params$multiple_testing_alpha,
      power_target = perturbplan_params$power_target,
      cost_constraint = perturbplan_params$cost_constraint,
      baseline_expression_stats = perturbplan_params$baseline_expression_stats,
      library_parameters = perturbplan_params$library_parameters
    )
    
    cat(sprintf("  ✓ Analysis completed: %d designs tested\n", nrow(results)))
    
    # Show key results
    if ("overall_power" %in% colnames(results)) {
      power_range <- range(results$overall_power, na.rm = TRUE)
      optimal_idx <- which.max(results$overall_power)
      optimal_design <- results[optimal_idx, ]
      
      cat(sprintf("  ✓ Power range: %.3f to %.3f\n", power_range[1], power_range[2]))
      cat(sprintf("  ✓ Optimal design: %s = %.1f, power = %.3f\n", 
                  minimizing_param, optimal_design[[minimizing_param]], optimal_design$overall_power))
      
      # Show cost information
      if ("total_cost" %in% colnames(results)) {
        cost_range <- range(results$total_cost, na.rm = TRUE)
        cat(sprintf("  ✓ Cost range: $%.0f to $%.0f\n", cost_range[1], cost_range[2]))
        cat(sprintf("  ✓ Optimal cost: $%.0f\n", optimal_design$total_cost))
      }
    }
    
    # Test standardization
    standardized_results <- standardize_perturbplan_results(results, test_config, workflow_info)
    cat(sprintf("  ✓ Results standardized: %s\n", standardized_results$metadata$analysis_mode))
    cat(sprintf("  ✓ Meets power target: %s\n", standardized_results$validation$meets_power_target))
    
    cat("  ✓ Workflow test PASSED\n\n")
    return(TRUE)
    
  }, error = function(e) {
    cat(sprintf("  ✗ Error: %s\n\n", e$message))
    return(FALSE)
  })
}

# Test all 4 power-only workflows
workflows <- list(
  list(param = "cells_per_target", name = "Power-Only + Minimize Cells per Target", fc = 1.2),
  list(param = "reads_per_cell", name = "Power-Only + Minimize Reads per Cell", fc = 1.2),
  list(param = "TPM_threshold", name = "Power-Only + Minimize TPM Threshold", fc = 1.2),
  list(param = "min_fold_change", name = "Power-Only + Minimize Fold Change", fc = NULL)
)

# Store results
results_summary <- data.frame(
  workflow = character(0),
  success = logical(0),
  stringsAsFactors = FALSE
)

# Run all tests
for (workflow in workflows) {
  # For fold change minimization, we don't need a fixed fold change
  if (workflow$param == "min_fold_change") {
    success <- test_power_only_workflow(workflow$param, workflow$name)
  } else {
    success <- test_power_only_workflow(workflow$param, workflow$name, workflow$fc)
  }
  
  results_summary <- rbind(results_summary, data.frame(
    workflow = workflow$name,
    success = success
  ))
}

# Summary
cat("=== WORKFLOW TEST SUMMARY ===\n")
for (i in 1:nrow(results_summary)) {
  status <- if (results_summary$success[i]) "✓ PASS" else "✗ FAIL"
  cat(sprintf("%s: %s\n", results_summary$workflow[i], status))
}

passed <- sum(results_summary$success)
total <- nrow(results_summary)
cat(sprintf("\nOverall: %d/%d workflows passed\n", passed, total))

if (passed == total) {
  cat("🎉 All power-only workflows are working correctly!\n")
} else {
  cat("⚠️  Some workflows need attention.\n")
}