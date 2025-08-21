# Debug single plotting issue

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

cat("=== DEBUGGING SINGLE PLOT CREATION ===\n")

# Create test configuration for cells_per_target only
test_config <- list(
  design_options = list(
    optimization_type = "power_only",
    minimization_target = "cells_per_target",
    target_power = 0.8,
    parameter_controls = list(
      cells_per_target = list(type = "minimizing"),
      reads_per_cell = list(type = "fixed"),
      TPM_threshold = list(type = "fixed"),
      min_fold_change = list(type = "fixed")
    )
  ),
  experimental_setup = list(
    biological_system = "K562",
    pilot_data = list(type = "default", biological_system = "K562"),
    MOI = 10,
    num_targets = 100,
    gRNAs_per_target = 4,
    non_targeting_gRNAs = 10,
    reads_fixed = 30000
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

cat("Step 1: Generate analysis results\n")
workflow_info <- detect_workflow_scenario(test_config)
results <- generate_real_analysis(test_config, workflow_info)

if (!is.null(results$error)) {
  cat("ERROR in analysis:", results$error, "\n")
  quit()
}

cat("Step 2: Check analysis results structure\n")
cat("Power data columns:", paste(names(results$power_data), collapse = ", "), "\n")
cat("Power data rows:", nrow(results$power_data), "\n")
cat("Target power:", results$user_config$design_options$target_power, "\n")
cat("Workflow title:", results$workflow_info$title, "\n")

cat("Step 3: Test manual ggplot creation\n")
power_data <- results$power_data
target_power <- results$user_config$design_options$target_power
varying_param <- results$workflow_info$minimizing_parameter
param_label <- format_parameter_name(varying_param)
plot_title <- results$workflow_info$title

print("Creating basic ggplot...")
p <- ggplot(power_data, aes(x = parameter_value, y = power)) +
  geom_line() +
  geom_point() +
  geom_hline(yintercept = target_power, linetype = "dashed") +
  labs(title = plot_title, x = param_label, y = "Power") +
  theme_bw()

print("Manual ggplot created successfully!")

cat("Step 4: Test ggplot with .data pronoun\n")
tryCatch({
  # Test the exact same ggplot call as in the function
  p2 <- ggplot(power_data, aes(x = .data$parameter_value, y = .data$power)) +
    geom_line() +
    geom_point() +
    geom_hline(yintercept = target_power, linetype = "dashed") +
    labs(
      title = plot_title,
      x = param_label,
      y = "Power"
    ) +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5))
  
  cat("SUCCESS: ggplot with .data pronoun worked!\n")
}, error = function(e) {
  cat("ERROR with .data pronoun:", e$message, "\n")
})

cat("Step 5: Test create_single_parameter_plots function step by step\n")

# Recreate the function manually step by step to isolate the error
power_data <- results$power_data
optimal_design <- results$optimal_design
target_power <- results$user_config$design_options$target_power
workflow_info <- results$workflow_info

# Create base ggplot
varying_param <- workflow_info$minimizing_parameter
param_label <- format_parameter_name(varying_param)
plot_title <- workflow_info$title

cat("  Creating ggplot...")
tryCatch({
  p <- ggplot(power_data, aes(x = .data$parameter_value, y = .data$power)) +
    geom_line() +
    geom_point() +
    geom_hline(yintercept = target_power, linetype = "dashed") +
    labs(
      title = plot_title,
      x = param_label,
      y = "Power"
    ) +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5))
  cat(" OK\n")
}, error = function(e) {
  cat(sprintf(" ERROR: %s\n", e$message))
})

cat("  Creating plotly...")
tryCatch({
  p_interactive <- ggplotly(p, tooltip = c("x", "y")) %>%
    layout(
      title = list(
        text = paste0("<b>", workflow_info$title, "</b><br>",
                     "<sup>", workflow_info$description, "</sup>"),
        font = list(size = 14)
      ),
      showlegend = FALSE,
      hovermode = "closest"
    ) %>%
    config(
      displayModeBar = FALSE,
      displaylogo = FALSE,
      modeBarButtonsToRemove = list("all")
    )
  cat(" OK\n")
}, error = function(e) {
  cat(sprintf(" ERROR: %s\n", e$message))
})

cat("  Checking optimal_design structure...")
cat("\n    optimal_design fields:", paste(names(optimal_design), collapse = ", "), "\n")
cat("    optimal_design$found:", optimal_design$found %||% "NULL", "\n")
cat("    optimal_design$parameter:", optimal_design$parameter %||% "NULL", "\n")
cat("    optimal_design$parameter_value:", optimal_design$parameter_value %||% "NULL", "\n")

cat("  Creating summary stats...")
tryCatch({
  summary_stats <- create_power_curve_summary(power_data, optimal_design, target_power, workflow_info)
  cat(" OK\n")
}, error = function(e) {
  cat(sprintf(" ERROR: %s\n", e$message))
  cat("  This is likely where the 'argument is of length zero' error occurs!\n")
})

cat("Step 6: Test full create_single_parameter_plots function\n")
tryCatch({
  plots <- create_single_parameter_plots(results)
  cat("SUCCESS: create_single_parameter_plots worked!\n")
}, error = function(e) {
  cat("ERROR in create_single_parameter_plots:", e$message, "\n")
})

cat("=== DEBUG COMPLETE ===\n")