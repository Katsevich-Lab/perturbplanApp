# CLAUDE.md - PerturbPlan Shiny App v2

This file provides guidance to Claude Code when working with the PerturbPlan Shiny application built using the Golem framework.

## Package Overview

PerturbPlanApp is a Golem-based Shiny application for constraint-driven experimental design in CRISPR perturb-seq experiments. It provides an intuitive interface for power analysis and experimental optimization based on user-specified constraints and objectives.

## Architecture

### Golem Framework Structure

- **Main App Files**: `R/app_*.R` - Core application structure
- **Modules**: `R/mod_*.R` - Modular Shiny components  
- **Utilities**: `R/fct_*.R` and `R/utils_*.R` - Helper functions
- **Assets**: `inst/app/www/` - CSS, JS, images
- **Configuration**: `inst/golem-config.yml` - App configuration

### Core Modules (Based on PRD)

1. **`mod_design_options.R`** - Main design configuration module
   - Optimization objective selection (power-only vs power+cost)
   - Minimization target selection
   - Parameter control panel with Varying/Fixed/Minimizing status

2. **`mod_experimental_setup.R`** - Pilot data and library parameters
   - Modified from original app, removes TPM/fold change controls
   - Handles baseline expression and library parameter uploads

3. **`mod_analysis_results.R`** - Results visualization and display
   - Placeholder visualizations for 11 workflow scenarios
   - Power curves, cost curves, and optimization results

4. **`mod_results_export.R`** - Data export functionality
   - Excel downloads with organized sheets
   - Results summaries and parameter documentation

## Key Design Principles

### Constraint-Driven Workflow

The app follows a constraint-driven design where users specify:
1. **Optimization objective**: Power-only or Power + cost considerations
2. **Minimization target**: What parameter to optimize (cells, reads, cost, TPM, fold change)
3. **Parameter controls**: Which parameters vary, are fixed, or are being minimized

### 11 Supported Workflows

#### Power-Only Optimization (5 workflows)
- Workflows 1-4: Single parameter optimization (cells, reads, TPM, fold change)
- Workflow 5: Total cost minimization with cells + reads varying

#### Power + Cost Optimization (6 workflows)  
- Workflows 6-8: TPM minimization with different parameter combinations
- Workflows 9-11: Fold change minimization with different parameter combinations

### Parameter Control Logic

**Business Rules:**
- Only one parameter can have "Minimizing" status at a time
- When a parameter is selected as minimization target, it's auto-set to "Minimizing" and grayed out
- All other parameters are grayed out when one is "Minimizing"
- Fixed parameters provide constraints (up to 2 values each)
- Varying parameters interact with the minimizing parameter during optimization

## Development Workflow

### Golem Development Commands

```r
# Load and test the app during development
golem::run_dev()

# Document the package
devtools::document()

# Check the package
devtools::check()

# Install development dependencies
golem::install_dev_deps()
```

### File Organization

```
R/
├── app_*.R           # Core Golem app files
├── mod_*.R           # Shiny modules (our main focus)
├── fct_*.R           # Business logic functions  
├── utils_*.R         # Utility functions
└── run_app.R         # App launcher

inst/app/www/
├── custom.css        # Custom styling (preserve visual continuity)
├── custom.js         # JavaScript enhancements
└── favicon.ico       # App icon

dev/
├── 01_start.R        # Initial setup and dependencies
├── 02_dev.R          # Development helpers
└── 03_deploy.R       # Deployment configuration
```

## Integration with perturbplan Package

### Dependency Setup

Add to DESCRIPTION:
```
Imports:
    perturbplan,
    shiny,
    golem,
    DT,
    plotly,
    ggplot2
```

### Function Usage

```r
# Use functions from perturbplan package
perturbplan::calculate_power_grid()
perturbplan::compute_power_grid_efficient()
perturbplan::library_computation()
```

## Module Development Guidelines

### mod_design_options Structure

```r
# UI function with 3 main sections
mod_design_options_ui <- function(id) {
  ns <- NS(id)
  tagList(
    # Section 1: Optimization Framework  
    radioButtons(ns("optimization_type"), ...),
    
    # Section 2: Minimization Target
    checkboxGroupInput(ns("minimization_target"), ...),
    
    # Section 3: Parameter Control Matrix
    # Dynamic UI with Varying/Fixed/Minimizing radio buttons
  )
}

# Server function with reactive logic
mod_design_options_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    # Business logic for parameter control
    # Auto-update "Minimizing" status
    # Return design configuration reactive
  })
}
```

### Visual Continuity Requirements

- **Preserve color scheme** from original app
- **Reuse UI components** and styling patterns
- **Maintain responsive layouts**
- **Keep familiar interaction patterns**

Copy CSS/JS from original app:
```bash
cp ../perturbplan/inst/shiny/ui/* inst/app/www/
```

## Placeholder Development Approach

### Phase 1: Structure Setup
- Create module skeletons with placeholder content
- Implement navigation and basic UI layout
- Focus on constraint-driven workflow logic

### Phase 2: Business Logic  
- Implement parameter control panel logic
- Add workflow detection (identify which of 11 scenarios)
- Create design config data structure

### Phase 3: Placeholder Visualizations
- Static placeholder plots for each workflow type
- Power curves, cost curves, equi-power/equi-cost plots
- Interactive elements (gray-out regions, dashed lines, markers)

### Phase 4: Integration
- Connect to perturbplan package functions
- Real data processing and analysis
- Dynamic plot generation based on user inputs

## Configuration Management

### golem-config.yml Structure

```yaml
default:
  golem_name: perturbplanApp
  golem_version: 0.0.1
  app_prod: no
  
production:
  app_prod: yes
  
dev:
  golem_wd: !expr here::here()
```

### Environment-Specific Settings

- **Development**: Full debugging, reload capabilities
- **Production**: Optimized performance, error handling
- **Testing**: Mock data, simplified workflows

## Testing Strategy

### Module Testing

```r
# Test individual modules
testServer(mod_design_options_server, {
  # Test parameter control logic
  # Test workflow detection
  # Test design config output
})
```

### Integration Testing

```r
# Test complete workflows
# Test data flow between modules
# Test edge cases and error handling
```

## Performance Considerations

### Efficient Reactive Design

- Use `req()` for input validation
- Implement `debounce()` for expensive calculations
- Cache results where appropriate
- Lazy loading for large datasets

### Memory Management

- Clean up large objects when not needed
- Use data.table for efficient data manipulation
- Stream large downloads rather than loading in memory

## Deployment

### Preparation

```r
# Build and check
devtools::check()

# Create deployment bundle
golem::add_dockerfile()
```

### Hosting Options

- **Shinyapps.io**: Quick deployment for testing
- **Docker**: Containerized deployment
- **Shiny Server**: Self-hosted options

## Common Development Patterns

### Module Communication

```r
# Pass data between modules using reactive values
design_config <- mod_design_options_server("design")
mod_analysis_results_server("analysis", design_config)
```

### Dynamic UI Updates

```r
# Update UI based on user selections
observeEvent(input$optimization_type, {
  updateCheckboxGroupInput(...)
})
```

### Error Handling

```r
# Graceful error handling with user feedback
tryCatch({
  # Analysis logic
}, error = function(e) {
  showNotification("Error: Please check your inputs", type = "error")
})
```

## Git Workflow

### Commit Strategy

When committing changes:
- **Complete commits**: Ensure app is in working state
- **Clear messages**: Describe what module/feature was added
- **Test before commit**: Verify app loads and basic functionality works

### Branch Management

- **main**: Stable releases
- **dev**: Development work
- **feature/***: Specific feature development

## File Naming Conventions

### Modules
- `mod_design_options.R` - Main design configuration
- `mod_experimental_setup.R` - Data upload and parameters
- `mod_analysis_results.R` - Results and visualization
- `mod_results_export.R` - Export functionality

### Functions
- `fct_workflow_detection.R` - Business logic for detecting user workflow
- `fct_parameter_validation.R` - Input validation functions
- `fct_plot_generation.R` - Plot creation helpers

### Utilities  
- `utils_ui.R` - UI helper functions
- `utils_data.R` - Data processing utilities
- `utils_validation.R` - Input validation helpers

## Parameter Naming Convention

**IMPORTANT**: Use `tpm_threshold` instead of `tmp_threshold` everywhere in the package.

- All function parameters should use `tpm_threshold`
- All variable names should use `tpm_threshold`  
- All documentation should reference `tpm_threshold`
- UI inputs should use `"tpm_threshold"` as input ID

This ensures consistency across the entire codebase and avoids confusion between "TPM" (Transcripts Per Million) and "tmp" (temporary).

### Common Typos to Avoid

- **`tpm_threshold` vs `tmp_threshold`**: Always use `tpm_threshold` (Transcripts Per Million), not `tmp_threshold` (temporary)
- **Parameter consistency**: When adding parameters to functions, double-check spelling matches existing usage
- **Function signatures**: Ensure parameter names match between function definitions and calls
- **Careful attention to existing code**: When modifying existing functions, preserve existing parameter names exactly as they are

## Documentation Requirements

### Module Documentation

Each module should include:
- **Purpose**: What the module does
- **Inputs**: Expected input parameters
- **Outputs**: What the module returns
- **Business Logic**: Key decision points and rules

### Function Documentation

Use roxygen2 for all functions:
```r
#' Design Options Module UI
#'
#' @param id Module namespace ID
#' @return Shiny UI elements
#' @export
```

## Known Issues and TODOs

### Current Limitations

- Placeholder visualizations need real plot implementation
- Parameter ranges need dynamic detection from perturbplan package
- Error handling needs comprehensive testing

### Development Priorities

1. Complete mod_design_options implementation
2. Add workflow detection logic  
3. Create placeholder visualization framework
4. Integrate with perturbplan analysis functions

## Success Metrics

### Development Milestones

- [ ] All 11 workflows can be configured through UI
- [ ] Parameter control panel behaves according to business rules
- [ ] Placeholder visualizations display correctly for each workflow
- [ ] Integration with perturbplan package functions works
- [ ] Visual continuity with original app maintained

### User Experience Goals

- Faster time to relevant analysis results
- Clear workflow guidance based on user constraints  
- Intuitive parameter control interface
- Comprehensive results export functionality