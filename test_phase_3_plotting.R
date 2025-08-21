# Phase 3 Test Script: Plotting Engine Updates
# Tests the plotting engine with real perturbplan data

library(perturbplan)
library(ggplot2)
library(plotly)
library(scales)
library(magrittr)
library(rlang)
library(stringr)
source("R/fct_perturbplan_integration.R")
source("R/fct_pilot_data_validation.R")
source("R/fct_analysis_config.R")
source("R/mod_analysis_engine.R")
source("R/mod_plotting_engine.R")

cat("=== PHASE 3 TEST: Plotting Engine Updates ===\n\n")

# Test plotting for each workflow
test_plotting_workflow <- function(minimizing_param, workflow_name) {
  cat(sprintf("--- Testing Plotting: %s ---\n", workflow_name))
  
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
    # Step 1: Generate analysis results (from Phase 2)
    cat("  [1/5] Running analysis...")
    workflow_info <- detect_workflow_scenario(test_config)
    results <- generate_real_analysis(test_config, workflow_info)
    
    if (!is.null(results$error)) {
      cat(" FAILED\n")
      cat(sprintf("    Analysis error: %s\n", results$error))
      return(FALSE)
    }
    cat(" OK\n")
    
    # Step 2: Test plot generation
    cat("  [2/5] Generating plots...")
    
    # Debug: Check critical values
    cat(sprintf("\n    Debug - minimizing_parameter: '%s'\n", results$workflow_info$minimizing_parameter))
    cat(sprintf("    Debug - plot_type: '%s'\n", results$workflow_info$plot_type))
    cat(sprintf("    Debug - power_data columns: %s\n", paste(names(results$power_data), collapse = ", ")))
    
    # Create a fake module server environment for testing
    plot_objects <- list()
    
    # Simulate the reactive analysis_results
    analysis_results_reactive <- function() results
    
    # Test format_parameter_name function first
    cat(sprintf("    Debug - testing format_parameter_name('%s'): ", results$workflow_info$minimizing_parameter))
    tryCatch({
      param_label <- format_parameter_name(results$workflow_info$minimizing_parameter)
      cat(sprintf("'%s'\n", param_label))
    }, error = function(e) {
      cat(sprintf("ERROR: %s\n", e$message))
      return(FALSE)
    })
    
    # Debug: Check all critical values that might be empty
    cat(sprintf("    Debug - target_power: '%s'\n", results$user_config$design_options$target_power))
    cat(sprintf("    Debug - workflow title: '%s'\n", results$workflow_info$title))
    cat(sprintf("    Debug - power_data rows: %d\n", nrow(results$power_data)))
    cat(sprintf("    Debug - optimal_design structure: %s\n", paste(names(results$optimal_design), collapse = ", ")))
    
    # Test the ggplot creation manually first
    cat("    Debug - Testing manual ggplot creation...")
    tryCatch({
      power_data <- results$power_data
      target_power <- results$user_config$design_options$target_power
      varying_param <- results$workflow_info$minimizing_parameter
      param_label <- format_parameter_name(varying_param)
      plot_title <- results$workflow_info$title
      
      # Create basic ggplot
      p <- ggplot(power_data, aes(x = parameter_value, y = power)) +
        geom_line() +
        geom_point() +
        geom_hline(yintercept = target_power, linetype = "dashed") +
        labs(title = plot_title, x = param_label, y = "Power") +
        theme_bw()
      
      cat(" OK\n")
      cat("    Debug - Manual ggplot creation successful\n")
      
    }, error = function(e) {
      cat(sprintf(" FAILED - Manual ggplot error: %s\n", e$message))
      return(FALSE)
    })
    
    # Call the plotting engine logic directly
    if (results$workflow_info$plot_type == "single_parameter_curve") {
      plots <- create_single_parameter_plots(results)
    } else if (results$workflow_info$plot_type == "cost_tradeoff_curves") {
      plots <- create_cost_tradeoff_plots(results)
    } else {
      cat(" FAILED\n")
      cat(sprintf("    Unknown plot type: %s\n", results$workflow_info$plot_type))
      return(FALSE)
    }
    
    cat(" OK\n")
    
    # Step 3: Validate plot structure
    cat("  [3/5] Validating plot structure...")
    
    required_plot_fields <- c("main_plot", "interactive_plot", "summary_stats", "plot_data")
    missing_plot_fields <- setdiff(required_plot_fields, names(plots))
    
    if (length(missing_plot_fields) > 0) {
      cat(" FAILED\n")
      cat(sprintf("    Missing plot fields: %s\n", paste(missing_plot_fields, collapse = ", ")))
      return(FALSE)
    }
    
    cat(" OK\n")
    
    # Step 4: Validate ggplot object
    cat("  [4/5] Validating ggplot object...")
    
    if (!inherits(plots$main_plot, "ggplot")) {
      cat(" FAILED\n")
      cat("    main_plot is not a ggplot object\n")
      return(FALSE)
    }
    
    cat(" OK\n")
    
    # Step 5: Validate interactive plot 
    cat("  [5/5] Validating interactive plot...")
    
    if (!inherits(plots$interactive_plot, "plotly")) {
      cat(" FAILED\n")
      cat("    interactive_plot is not a plotly object\n")
      return(FALSE)
    }
    
    cat(" OK\n")
    
    # Print summary
    n_data_points <- nrow(plots$plot_data)
    power_range <- range(plots$plot_data$power, na.rm = TRUE)
    param_range <- range(plots$plot_data$parameter_value, na.rm = TRUE)
    
    cat(sprintf("  ✓ Plot type: %s\n", results$workflow_info$plot_type))
    cat(sprintf("  ✓ Data points: %d\n", n_data_points))
    cat(sprintf("  ✓ Power range: %.3f - %.3f\n", power_range[1], power_range[2]))
    cat(sprintf("  ✓ Parameter range: %.2f - %.2f\n", param_range[1], param_range[2]))
    cat(sprintf("  ✓ Plot title: %s\n", results$workflow_info$title))
    
    # Validate summary stats
    if (!is.null(plots$summary_stats)) {
      summary <- plots$summary_stats
      if (!is.null(summary$optimal_recommendation)) {
        optimal <- summary$optimal_recommendation
        cat(sprintf("  ✓ Optimal value: %s=%.2f\n", 
                   optimal$parameter %||% minimizing_param, 
                   optimal$optimal_value %||% 0))
        cat(sprintf("  ✓ Achieved power: %.1f%%\n", 
                   (optimal$achieved_power %||% 0) * 100))
      }
    }
    
    cat("  → PLOTTING TEST PASSED\n\n")
    return(TRUE)
    
  }, error = function(e) {
    cat(" FAILED\n")
    cat(sprintf("    Unexpected error: %s\n\n", e$message))
    return(FALSE)
  })
}

# Test error handling
test_plotting_error_handling <- function() {
  cat("--- Testing Plotting Error Handling ---\n")
  
  # Test with invalid results structure
  invalid_results <- list(
    error = "Mock analysis error",
    workflow_info = list(plot_type = "single_parameter_curve")
  )
  
  tryCatch({
    # This should handle the error gracefully
    if (invalid_results$workflow_info$plot_type == "single_parameter_curve") {
      plots <- create_single_parameter_plots(invalid_results)
    }
    
    cat("  ✗ Error handling: Should have failed with invalid results\n")
    
  }, error = function(e) {
    cat("  ✓ Error handling: Correctly catches plotting errors\n")
  })
  
  cat("\n")
}

# Run all tests
cat("Starting Phase 3 tests...\n\n")

# Test error handling
test_plotting_error_handling()

# Test plotting for all 4 power-only workflows
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
  success <- test_plotting_workflow(workflow$param, workflow$name)
  results_summary <- rbind(results_summary, data.frame(
    workflow = workflow$name,
    success = success
  ))
}

# Final summary
cat("=== PHASE 3 TEST SUMMARY ===\n")
for (i in 1:nrow(results_summary)) {
  status <- if (results_summary$success[i]) "✓ PASS" else "✗ FAIL"
  cat(sprintf("%s: %s\n", results_summary$workflow[i], status))
}

passed <- sum(results_summary$success)
total <- nrow(results_summary)
cat(sprintf("\nPhase 3 Result: %d/%d workflows passed\n", passed, total))

if (passed == total) {
  cat("🎉 Phase 3 COMPLETE: Plotting engine working with real perturbplan data!\n")
  cat("✅ Interactive plots generated successfully for all workflows\n")
  cat("✅ Ready to proceed to Phase 4: Testing & Validation\n")
} else {
  cat("⚠️  Phase 3 needs attention before proceeding\n")
}

cat("\n=== TEST COMPLETE ===\n")