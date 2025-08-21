# Phase 1 Test Script: Data Transformation Layer
# Tests the transformation of perturbplan results to plotting format

library(perturbplan)
source("R/fct_perturbplan_integration.R")
source("R/fct_pilot_data_validation.R")

cat("=== PHASE 1 TEST: Data Transformation Layer ===\n\n")

# Test helper functions first
test_helper_functions <- function() {
  cat("--- Testing Helper Functions ---\n")
  
  # Test get_parameter_column_name()
  test_cases <- list(
    list(param = "cells_per_target", expected = "cells_per_target"),
    list(param = "reads_per_cell", expected = "raw_reads_per_cell"),
    list(param = "TPM_threshold", expected = "TPM_threshold"),
    list(param = "minimum_fold_change", expected = "minimum_fold_change")
  )
  
  for (case in test_cases) {
    result <- get_parameter_column_name(case$param)
    if (result == case$expected) {
      cat(sprintf("  ✓ %s -> %s\n", case$param, result))
    } else {
      cat(sprintf("  ✗ %s -> %s (expected %s)\n", case$param, result, case$expected))
    }
  }
  
  # Test workflow title/description creation
  title <- create_workflow_title("cells_per_target")
  desc <- create_workflow_description("cells_per_target", 0.8)
  
  cat(sprintf("  ✓ Title: %s\n", title))
  cat(sprintf("  ✓ Description: %s\n", desc))
  
  cat("\n")
}

# Test transformation for each workflow
test_transformation_workflow <- function(minimizing_param, workflow_name) {
  cat(sprintf("--- Testing Transformation: %s ---\n", workflow_name))
  
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
        min_fold_change = list(type = if(minimizing_param == "minimum_fold_change") "minimizing" else "fixed")
      )
    ),
    experimental_setup = list(
      biological_system = "K562",
      pilot_data = list(type = "default", biological_system = "K562"),
      MOI = 10,
      num_targets = 100,
      gRNAs_per_target = 4,
      non_targeting_gRNAs = 10,
      cells_fixed = if(minimizing_param != "cells_per_target") 1000 else NULL,
      reads_fixed = if(minimizing_param != "reads_per_cell") 30000 else NULL
    ),
    analysis_choices = list(
      control_group = "complement",
      side = "left",
      fdr_target = 0.05,
      TPM_threshold = 10
    ),
    effect_sizes = list(
      fc_sd = 0.15,
      prop_non_null = 0.1,
      minimum_fold_change = 0.8
    )
  )
  
  workflow_info <- list(
    scenario = paste0("power_only_minimize_", minimizing_param),
    minimizing_parameter = minimizing_param,
    optimization_type = "power_only"
  )
  
  tryCatch({
    # Step 1: Generate real perturbplan data
    cat("  [1/4] Running perturbplan analysis...")
    
    # Call perturbplan directly for testing
    perturbplan_params <- map_config_to_perturbplan_params(test_config, workflow_info)
    
    raw_results <- perturbplan::cost_power_computation(
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
      library_parameters = perturbplan_params$library_parameters,
      grid_size = 100
    )
    
    # Create standardized results format for testing
    standardized_results <- standardize_perturbplan_results(raw_results, test_config, workflow_info)
    
    if (!is.null(standardized_results$error)) {
      cat(" FAILED\n")
      cat(sprintf("    Error: %s\n", standardized_results$error))
      return(FALSE)
    }
    cat(" OK\n")
    
    # Step 2: Test data transformation
    cat("  [2/4] Testing data transformation...")
    plotting_results <- transform_perturbplan_to_plotting_format(standardized_results, test_config, workflow_info)
    
    if (!is.null(plotting_results$error)) {
      cat(" FAILED\n")
      cat(sprintf("    Error: %s\n", plotting_results$error))
      return(FALSE)
    }
    cat(" OK\n")
    
    # Step 3: Validate structure
    cat("  [3/4] Validating result structure...")
    
    # Check required fields
    required_fields <- c("power_data", "optimal_design", "workflow_info", "user_config", "metadata")
    missing_fields <- setdiff(required_fields, names(plotting_results))
    
    if (length(missing_fields) > 0) {
      cat(" FAILED\n")
      cat(sprintf("    Missing fields: %s\n", paste(missing_fields, collapse = ", ")))
      return(FALSE)
    }
    
    # Check power_data structure
    power_data <- plotting_results$power_data
    required_cols <- c("parameter_value", "power", "meets_threshold")
    missing_cols <- setdiff(required_cols, names(power_data))
    
    if (length(missing_cols) > 0) {
      cat(" FAILED\n")
      cat(sprintf("    Missing power_data columns: %s\n", paste(missing_cols, collapse = ", ")))
      return(FALSE)
    }
    
    cat(" OK\n")
    
    # Step 4: Validate data content
    cat("  [4/4] Validating data content...")
    
    # Check data types
    if (!is.numeric(power_data$parameter_value)) {
      cat(" FAILED\n")
      cat("    parameter_value is not numeric\n")
      return(FALSE)
    }
    
    if (!is.numeric(power_data$power)) {
      cat(" FAILED\n")
      cat("    power is not numeric\n")
      return(FALSE)
    }
    
    if (!is.logical(power_data$meets_threshold)) {
      cat(" FAILED\n")
      cat("    meets_threshold is not logical\n")
      return(FALSE)
    }
    
    # Check optimal design
    optimal_design <- plotting_results$optimal_design
    if (!is.numeric(optimal_design$parameter_value) || !is.numeric(optimal_design$achieved_power)) {
      cat(" FAILED\n")
      cat("    optimal_design has invalid data types\n")
      return(FALSE)
    }
    
    cat(" OK\n")
    
    # Print summary
    n_designs <- nrow(power_data)
    n_meeting_target <- sum(power_data$meets_threshold)
    power_range <- range(power_data$power, na.rm = TRUE)
    param_range <- range(power_data$parameter_value, na.rm = TRUE)
    
    cat(sprintf("  ✓ Designs evaluated: %d\n", n_designs))
    cat(sprintf("  ✓ Meeting target (≥80%%): %d (%.1f%%)\n", n_meeting_target, 100 * n_meeting_target / n_designs))
    cat(sprintf("  ✓ Power range: %.3f - %.3f\n", power_range[1], power_range[2]))
    cat(sprintf("  ✓ Parameter range: %.2f - %.2f\n", param_range[1], param_range[2]))
    cat(sprintf("  ✓ Optimal: %s=%.2f, power=%.3f\n", 
               minimizing_param, optimal_design$parameter_value, optimal_design$achieved_power))
    cat(sprintf("  ✓ Plot title: %s\n", plotting_results$workflow_info$title))
    
    cat("  → TRANSFORMATION TEST PASSED\n\n")
    return(TRUE)
    
  }, error = function(e) {
    cat(" FAILED\n")
    cat(sprintf("    Unexpected error: %s\n\n", e$message))
    return(FALSE)
  })
}

# Test error handling
test_error_handling <- function() {
  cat("--- Testing Error Handling ---\n")
  
  # Test with error in standardized results
  error_results <- list(error = "Mock error for testing")
  config <- list()
  workflow_info <- list(minimizing_parameter = "cells_per_target")
  
  result <- transform_perturbplan_to_plotting_format(error_results, config, workflow_info)
  
  if (!is.null(result$error) && result$error == "Mock error for testing") {
    cat("  ✓ Error handling: Passes through errors correctly\n")
  } else {
    cat("  ✗ Error handling: Failed to pass through errors\n")
  }
  
  # Test with empty data
  empty_results <- list(data = data.frame())
  result <- transform_perturbplan_to_plotting_format(empty_results, config, workflow_info)
  
  if (!is.null(result$error) && grepl("No data available", result$error)) {
    cat("  ✓ Empty data handling: Correctly detects empty data\n")
  } else {
    cat("  ✗ Empty data handling: Failed to detect empty data\n")
  }
  
  cat("\n")
}

# Run all tests
cat("Starting Phase 1 tests...\n\n")

# Test helper functions
test_helper_functions()

# Test error handling
test_error_handling()

# Test all 4 workflows
workflows <- list(
  list(param = "cells_per_target", name = "Minimize Cells per Target"),
  list(param = "reads_per_cell", name = "Minimize Reads per Cell"),
  list(param = "TPM_threshold", name = "Minimize TPM Threshold"), 
  list(param = "minimum_fold_change", name = "Minimize Minimum Fold Change")
)

results_summary <- data.frame(
  workflow = character(0),
  success = logical(0),
  stringsAsFactors = FALSE
)

for (workflow in workflows) {
  success <- test_transformation_workflow(workflow$param, workflow$name)
  results_summary <- rbind(results_summary, data.frame(
    workflow = workflow$name,
    success = success
  ))
}

# Final summary
cat("=== PHASE 1 TEST SUMMARY ===\n")
for (i in 1:nrow(results_summary)) {
  status <- if (results_summary$success[i]) "✓ PASS" else "✗ FAIL"
  cat(sprintf("%s: %s\n", results_summary$workflow[i], status))
}

passed <- sum(results_summary$success)
total <- nrow(results_summary)
cat(sprintf("\nPhase 1 Result: %d/%d workflows passed\n", passed, total))

if (passed == total) {
  cat("🎉 Phase 1 COMPLETE: Data transformation layer working perfectly!\n")
  cat("✅ Ready to proceed to Phase 2: Analysis Engine Integration\n")
} else {
  cat("⚠️  Phase 1 needs attention before proceeding\n")
}

cat("\n=== TEST COMPLETE ===\n")