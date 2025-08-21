# CLAUDE.md - PerturbPlan Shiny App

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

### Golem Development Commands (Current Best Practices)

```r
# Primary development workflow
golem::run_dev()              # Launch app in development mode
devtools::test()              # Run all tests (40 tests should pass)
devtools::check()             # Full package check (should show 0 errors/warnings)

# Documentation and dependencies
devtools::document()          # Update NAMESPACE and man files
golem::install_dev_deps()     # Install all development dependencies

# For new modules (ALWAYS use Golem):
golem::add_module(name = "module_name", with_test = TRUE, open = FALSE)

# For new assets (ALWAYS use Golem):
golem::add_css_file("filename")      # Add CSS files
golem::add_js_file("filename")       # Add JavaScript files

# For new dependencies (ALWAYS use usethis):
usethis::use_package("package_name") # Add to DESCRIPTION
```

### ⚠️ Critical Golem Rules

**NEVER do these** (violations of Golem best practices):
```r
# ❌ Manual module creation
# Create files manually in R/ directory

# ❌ Manual asset inclusion  
tags$script(src = "file.js")
tags$link(rel = "stylesheet", href = "file.css")

# ❌ Missing dependency declarations
# Using package::function without declaring in DESCRIPTION
```

**ALWAYS do these** (proper Golem practices):
```r
# ✅ Proper module creation
golem::add_module(name = "module_name", with_test = TRUE)

# ✅ Proper asset management
# Assets automatically bundled by bundle_resources() in golem_add_external_resources()

# ✅ Proper dependency management  
usethis::use_package("package_name")     # Adds to DESCRIPTION
#' @importFrom package function          # Adds to NAMESPACE via devtools::document()
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

### CRITICAL: Always Use Golem Commands for Module Creation

**IMPORTANT**: ALL modules must be created using proper Golem commands. Never manually create module files.

#### Creating New Modules

```r
# Always use this command to create new modules
golem::add_module(name = "module_name", with_test = TRUE)

# Examples:
golem::add_module(name = "cost_info", with_test = TRUE)
golem::add_module(name = "results_display", with_test = TRUE)
golem::add_module(name = "data_export", with_test = TRUE)
```

#### Why Use Golem Commands?

1. **Proper File Structure**: Creates files in correct locations with proper naming
2. **Template Generation**: Provides proper roxygen documentation templates
3. **Test Integration**: Automatically creates corresponding test files
4. **Import Management**: Ensures proper `@importFrom` declarations
5. **Consistency**: Maintains consistent code structure across the project
6. **Documentation**: Updates package documentation appropriately

#### Module Creation Workflow

```r
# Step 1: Create the module using Golem
golem::add_module(name = "your_module", with_test = TRUE)

# Step 2: Document the package
devtools::document()

# Step 3: Implement the module logic
# Edit R/mod_your_module.R with proper business logic

# Step 4: Write comprehensive tests
# Edit tests/testthat/test-mod_your_module.R

# Step 5: Test and validate
devtools::test()
devtools::check()
```

#### Module File Locations

When using `golem::add_module()`, files are created at:
- **Module File**: `R/mod_[name].R`
- **Test File**: `tests/testthat/test-mod_[name].R`

#### Golem Module Template Structure

```r
# Generated by golem::add_module(name = "example", with_test = TRUE)

#' example UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_example_ui <- function(id){
  ns <- NS(id)
  tagList(
    # Module UI logic here
  )
}
    
#' example Server Functions
#'
#' @noRd 
mod_example_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    # Module server logic here
  })
}
```

### Module Integration Best Practices

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

## Golem Compliance Completed ✅

This app now follows **strict Golem best practices** after systematic compliance work. All violations have been addressed:

### Priority 1: Module Recreation ✅ COMPLETE
**Issue**: All modules were manually created instead of using `golem::add_module()`
**Resolution**: Systematically recreated all modules using proper Golem commands:

```bash
# Pattern followed for each module:
Rscript -e "golem::add_module(name = 'design_options', with_test = TRUE, open = FALSE)"
Rscript -e "golem::add_module(name = 'cost_info', with_test = TRUE, open = FALSE)" 
Rscript -e "golem::add_module(name = 'experimental_setup', with_test = TRUE, open = FALSE)"
Rscript -e "golem::add_module(name = 'analysis_choices', with_test = TRUE, open = FALSE)"
Rscript -e "golem::add_module(name = 'effect_sizes', with_test = TRUE, open = FALSE)"
Rscript -e "golem::add_module(name = 'sidebar', with_test = TRUE, open = FALSE)"
```

**Result**: All modules now have proper Golem structure with template comments and comprehensive tests (40 tests passing).

### Priority 2: Asset Management ✅ COMPLETE
**Issue**: Manual JavaScript inclusion instead of proper Golem asset management
**Resolution**: 
- Registered assets with `golem::add_css_file('perturbplan_styles')` and `golem::add_js_file('perturbplan_interactions')`
- Removed manual `tags$script()` inclusion from `golem_add_external_resources()`
- Assets now properly managed by `bundle_resources()` function

**Result**: All static assets (CSS/JS) automatically bundled by Golem framework.

### Priority 3: Dependencies ✅ COMPLETE
**Issue**: Missing package declarations in DESCRIPTION file
**Resolution**: Added all missing dependencies using `usethis::use_package()`:

```r
usethis::use_package('config')  # Configuration management
usethis::use_package('scales')  # Number formatting
usethis::use_package('tools')   # File extension validation  
usethis::use_package('utils')   # CSV reading functions
```

**Result**: All dependencies properly declared in DESCRIPTION and NAMESPACE files.

### Priority 4: Dev Workflow Documentation ✅ COMPLETE
**Current Task**: Updating this documentation to reflect completed compliance work.

### Golem Compliance Verification

Run these commands to verify continued compliance:

```r
# Check package structure
devtools::check()     # Should show 0 errors, 0 warnings
devtools::test()      # All 40 tests should pass

# Verify Golem structure  
golem::run_dev()      # App should launch successfully

# Module creation (for future modules)
golem::add_module(name = "new_module", with_test = TRUE, open = FALSE)
```

### Current Development State

**✅ Completed**:
- All UI modules implemented with constraint-driven logic
- Full parameter control with business rule enforcement  
- Conditional cost module display
- Interactive collapsible sections
- Complete test coverage (40 tests)
- All assets properly managed
- All dependencies declared

**🎯 Ready for Next Phase**: The codebase is now fully Golem-compliant and ready for Phase 2: Placeholder Visualizations.

## Development Phases

### Phase 1: Foundation ✅ COMPLETE
- ✅ Create module skeletons with proper Golem structure
- ✅ Implement constraint-driven UI with progressive disclosure
- ✅ Business logic for parameter control and workflow detection
- ✅ Complete sidebar integration with all modules
- ✅ Full Golem compliance (modules, assets, dependencies)

### Phase 2: Placeholder Visualizations 🎯 CURRENT PRIORITY
- Static placeholder plots for all 11 workflow scenarios
- Power curves, cost curves, and equi-power/equi-cost plots  
- Interactive plot elements (hover, zoom, selection regions)
- Results export module with Excel download functionality

#### Phase 2 Implementation Plan:
1. **Results Display Module**: Create `mod_results_display` with placeholder plots
2. **Workflow-Specific Plots**: 11 different plot types based on user configuration
3. **Interactive Elements**: Plotly integration for enhanced user experience
4. **Export Functionality**: `mod_results_export` with Excel generation

### Phase 3: Analysis Engine Integration
- Connect to perturbplan package functions (`calculate_power_grid`, etc.)
- Real data processing and dynamic plot generation  
- Performance optimization for large parameter grids
- Advanced error handling and validation

### Phase 4: Production Deployment
- Docker containerization with proper environment setup
- Deployment configuration (Shinyapps.io, Shiny Server)
- Performance monitoring and logging
- User documentation and tutorials

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

## CRITICAL PARAMETER NAMING REMINDER

**IMPORTANT**: Recheck the script whenever tpm/tmp is used. The correct one is tpm. Double check.

- For `extract_expression_info`: Use `tmp_threshold` parameter
- For `cost_power_computation`: Use `tmp_threshold` in fixed_variable mapping
- Always double-check parameter names when integrating with perturbplan package