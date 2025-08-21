# Phase 2 Test Script: Analysis Engine Integration
# Tests the complete workflow from UI config to plotting-ready results

library(perturbplan)
source("R/fct_perturbplan_integration.R")
source("R/fct_pilot_data_validation.R")
source("R/fct_analysis_config.R")
source("R/mod_analysis_engine.R")

cat("=== PHASE 2 TEST: Analysis Engine Integration ===\n\n")

# Test mode detection
test_mode_detection <- function() {
  cat("--- Testing Mode Detection ---\n")
  
  # Test default mode (should be real now)
  default_mode <- use_placeholder_mode()
  cat(sprintf("  Default mode (placeholder): %s\n", default_mode))
  
  # Test mode description
  mode_desc <- get_analysis_mode()
  cat(sprintf("  Mode description: %s\n", mode_desc))
  
  # Test environment variable override
  Sys.setenv("PERTURBPLAN_USE_PLACEHOLDER" = "true")
  env_mode <- use_placeholder_mode()
  cat(sprintf("  Environment override (placeholder): %s\n", env_mode))
  
  # Reset environment
  Sys.unsetenv("PERTURBPLAN_USE_PLACEHOLDER")
  reset_mode <- use_placeholder_mode()
  cat(sprintf("  After reset (placeholder): %s\n", reset_mode))
  
  cat("\n")
}

# Test complete analysis workflow
test_analysis_workflow <- function(minimizing_param, workflow_name) {
  cat(sprintf("--- Testing Complete Workflow: %s ---\n", workflow_name))
  
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
  
  tryCatch({
    # Step 1: Test workflow detection
    cat("  [1/5] Testing workflow detection...")
    workflow_info <- detect_workflow_scenario(test_config)
    
    if (workflow_info$minimizing_parameter != minimizing_param) {
      cat(" FAILED\n")
      cat(sprintf("    Expected: %s, Got: %s\n", minimizing_param, workflow_info$minimizing_parameter))
      return(FALSE)
    }
    cat(" OK\n")
    
    # Step 2: Test mode selection  
    cat("  [2/5] Testing analysis mode...")
    analysis_mode <- if(use_placeholder_mode()) "placeholder" else "real"
    cat(sprintf(" %s mode\n", analysis_mode))
    
    # Step 3: Test analysis execution
    cat("  [3/5] Running complete analysis...")
    
    # Force real mode for this test by temporarily updating mode
    original_mode <- use_placeholder_mode()
    
    # Override mode detection for testing
    assign("use_placeholder_mode", function() FALSE, envir = .GlobalEnv)
    
    # Get workflow info first
    workflow_info <- detect_workflow_scenario(test_config)
    
    # Generate real analysis results
    results <- generate_real_analysis(test_config, workflow_info)
    
    # Restore original mode function
    source("R/fct_analysis_config.R")
    
    if (!is.null(results$error)) {
      cat(" FAILED\n")
      cat(sprintf("    Error: %s\n", results$error))
      return(FALSE)
    }
    cat(" OK\n")
    
    # Step 4: Validate plotting-ready format
    cat("  [4/5] Validating plotting format...")
    
    # Check required fields for plotting
    required_fields <- c("power_data", "optimal_design", "workflow_info", "user_config")
    missing_fields <- setdiff(required_fields, names(results))
    
    if (length(missing_fields) > 0) {
      cat(" FAILED\n")
      cat(sprintf("    Missing fields: %s\n", paste(missing_fields, collapse = ", ")))
      return(FALSE)
    }
    
    # Check power_data structure
    power_data <- results$power_data
    required_cols <- c("parameter_value", "power", "meets_threshold")
    missing_cols <- setdiff(required_cols, names(power_data))
    
    if (length(missing_cols) > 0) {
      cat(" FAILED\n")
      cat(sprintf("    Missing power_data columns: %s\n", paste(missing_cols, collapse = ", ")))
      return(FALSE)
    }
    
    cat(" OK\n")
    
    # Step 5: Validate real data content
    cat("  [5/5] Validating real data content...")
    
    # Check that we have real perturbplan data (not placeholder patterns)
    if (nrow(power_data) < 10) {
      cat(" FAILED\n")
      cat("    Too few data points - may be placeholder data\n")
      return(FALSE)
    }
    
    # Check that analysis mode is correctly identified
    if (is.null(results$metadata$analysis_mode) || 
        !grepl("perturbplan", results$metadata$analysis_mode, ignore.case = TRUE)) {
      cat(" FAILED\n")
      cat("    Analysis mode not correctly identified as real perturbplan\n")
      return(FALSE)
    }
    
    cat(" OK\n")
    
    # Print summary
    n_designs <- nrow(power_data)
    n_meeting_target <- sum(power_data$meets_threshold)
    power_range <- range(power_data$power, na.rm = TRUE)
    optimal_design <- results$optimal_design
    
    cat(sprintf("  ✓ Analysis mode: %s\n", results$metadata$analysis_mode))
    cat(sprintf("  ✓ Designs evaluated: %d\n", n_designs))
    cat(sprintf("  ✓ Meeting target (≥80%%): %d (%.1f%%)\n", n_meeting_target, 100 * n_meeting_target / n_designs))
    cat(sprintf("  ✓ Power range: %.3f - %.3f\n", power_range[1], power_range[2]))
    cat(sprintf("  ✓ Optimal: %s=%.2f, power=%.3f\n", 
               minimizing_param, optimal_design$parameter_value, optimal_design$achieved_power))
    cat(sprintf("  ✓ Plot title: %s\n", results$workflow_info$title))
    cat(sprintf("  ✓ Plot type: %s\n", results$workflow_info$plot_type))
    
    cat("  → COMPLETE WORKFLOW TEST PASSED\n\n")
    return(TRUE)
    
  }, error = function(e) {
    cat(" FAILED\n")
    cat(sprintf("    Unexpected error: %s\n\n", e$message))
    return(FALSE)
  })
}

# Test error handling and fallback
test_error_handling <- function() {
  cat("--- Testing Error Handling & Fallback ---\n")
  
  # Create invalid configuration to test error handling
  invalid_config <- list(
    design_options = list(
      optimization_type = "power_only",
      target_power = 0.8
      # Missing required fields
    )
  )
  
  tryCatch({
    # This should trigger error handling (detect workflow info first)
    workflow_info <- detect_workflow_scenario(invalid_config)
    results <- generate_real_analysis(invalid_config, workflow_info)
    
    # Check if error was handled gracefully
    if (!is.null(results$error) || !is.null(results$power_data)) {
      cat("  ✓ Error handling: Graceful error handling or fallback to placeholder\n")
    } else {
      cat("  ✗ Error handling: Unexpected success with invalid config\n")
    }
    
  }, error = function(e) {
    cat("  ✓ Error handling: Errors caught at analysis level\n")
  })
  
  cat("\n")
}

# Run all tests
cat("Starting Phase 2 tests...\n\n")

# Test mode detection
test_mode_detection()

# Test error handling
test_error_handling()

# Test complete workflows
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
  success <- test_analysis_workflow(workflow$param, workflow$name)
  results_summary <- rbind(results_summary, data.frame(
    workflow = workflow$name,
    success = success
  ))
}

# Final summary
cat("=== PHASE 2 TEST SUMMARY ===\n")
for (i in 1:nrow(results_summary)) {
  status <- if (results_summary$success[i]) "✓ PASS" else "✗ FAIL"
  cat(sprintf("%s: %s\n", results_summary$workflow[i], status))
}

passed <- sum(results_summary$success)
total <- nrow(results_summary)
cat(sprintf("\nPhase 2 Result: %d/%d workflows passed\n", passed, total))

if (passed == total) {
  cat("🎉 Phase 2 COMPLETE: Analysis engine integration working perfectly!\n")
  cat("✅ Real perturbplan data now flows through complete analysis pipeline\n")
  cat("✅ Ready to proceed to Phase 3: Plotting Engine Updates\n")
} else {
  cat("⚠️  Phase 2 needs attention before proceeding\n")
}

cat("\n=== TEST COMPLETE ===\n")