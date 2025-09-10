# CLAUDE.md - PerturbPlan Shiny App

This file provides guidance to Claude Code when working with the PerturbPlan Shiny application built using the Golem framework.

## Package Overview

PerturbPlanApp is a **production-ready** Golem-based Shiny application for constraint-driven experimental design in CRISPR perturb-seq experiments. It provides a sophisticated interface for real-time power analysis and experimental optimization with **full perturbplan package integration**.

## Architecture

### Golem Framework Structure

- **Main App Files**: `R/app_*.R` - Core application structure
- **Modules**: `R/mod_*.R` - Modular Shiny components  
- **Utilities**: `R/fct_*.R` and `R/utils_*.R` - Helper functions
- **Assets**: `inst/app/www/` - CSS, JS, images
- **Configuration**: `inst/golem-config.yml` - App configuration

### Core Architecture: Clean 4-Module Design

**Production-Ready Analysis Pipeline:**
```
Input Collection → Analysis Engine → Plotting Engine → Results Display
     ↓                    ↓              ↓                ↓
mod_sidebar    →  mod_analysis_engine → mod_plotting_engine → mod_results_display
```

#### **Key Modules:**

1. **`mod_sidebar.R`** - Unified input collection
   - Integrates all parameter modules (design_options, experimental_setup, analysis_choices, effect_sizes)
   - Centralized configuration management with parameter translation
   - Real-time validation and business rule enforcement

2. **`mod_analysis_engine.R`** - **THE CRITICAL SWAP POINT**
   - **REAL MODE**: Full perturbplan package integration (DEFAULT)
   - **Placeholder Mode**: Available via environment variable for development
   - Three sophisticated analysis engines: Standard, Cost Minimization, Constrained Optimization

3. **`mod_plotting_engine.R`** - Interactive visualization
   - Converts analysis data to Plotly interactive plots
   - Handles all 11 workflow scenarios with specialized plot types
   - Real-time plot generation with performance optimization

4. **`mod_results_display.R`** - Production UI presentation
   - Dynamic results presentation based on analysis completion
   - Error handling and loading states
   - Export functionality for analysis results

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

## Production Status

### **✅ CURRENT STATE: FULLY OPERATIONAL**

**Real Analysis Integration Complete:**
- **118 tests passing** ✔️ (comprehensive test coverage)
- **Full perturbplan integration** ✔️ (real mathematical optimization)  
- **Production-ready performance** ✔️ (cached results, optimized reactivity)
- **All 11 workflows implemented** ✔️ (power-only and power+cost scenarios)

### Development Workflow

### Golem Development Commands (Current Best Practices)

```r
# Primary development workflow
golem::run_dev()              # Launch app in production mode (real analysis)
devtools::test()              # Run all tests (118 tests should pass)
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

### Analysis Mode Control

```r
# PRODUCTION MODE (DEFAULT): Real perturbplan analysis
# Set in fct_analysis_config.R line 37: return(FALSE)

# DEVELOPMENT MODE: Placeholder analysis (if needed)
Sys.setenv(PERTURBPLAN_USE_PLACEHOLDER = "true")
golem::run_dev()
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

## perturbplan Package Integration ✅ COMPLETE

### **Real Analysis Engines Working:**

#### **1. Standard Power Analysis** (`generate_real_analysis`)
```r
# Full integration with perturbplan::cost_power_computation
results <- perturbplan::cost_power_computation(
  minimizing_variable = "TPM_threshold",  # or "minimum_fold_change"
  fixed_variable = list(minimum_fold_change = 0.8),
  baseline_expression_stats = pilot_data$baseline_expression_stats,
  library_parameters = pilot_data$library_parameters,
  # ... all other parameters mapped from UI
)
```

#### **2. Cost Minimization Analysis** (`perform_cost_minimization_analysis`)
```r  
# Workflow 5: Total cost minimization with power constraints
# Uses perturbplan cost optimization functions
# Returns interactive cost-power tradeoff visualizations
```

#### **3. Constrained Minimization Analysis** (`perform_constrained_minimization_analysis`)
```r
# Workflows 10-11: TPM/FC minimization with cost+power constraints  
# Advanced multi-parameter optimization
# Real mathematical convergence with constraint handling
```

### **Parameter Translation System:**
- **UI Layer**: `TPM_threshold`, `cells_per_target`, `mapped_reads_per_cell` 
- **Backend Layer**: Automatic translation to perturbplan-compatible parameters
- **Column Standardization**: `raw_reads_per_cell` → `sequenced_reads_per_cell`

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

## Advanced Features Completed ✅

### **Variable Naming Unification (Phase 2.5) ✅ COMPLETE**
**Achievement**: Complete 5-layer naming standardization documented in `R/naming_standards.R`:

```r
# Reads Parameter: Complex 5-layer transformation
UI Input:     "mapped_reads_per_cell"           # User's mental model
Config:       "mapped_reads_fixed"              # Module parameter passing  
API Input:    "reads_per_cell"                  # perturbplan expects
API Output:   "raw_reads_per_cell"              # perturbplan returns
Internal:     "sequenced_reads_per_cell"        # Standard for processing/display
```

**Result**: Zero parameter naming confusion, seamless perturbplan integration.

### **Component Library Architecture ✅ COMPLETE**
- **283 semantic components** replacing inline styles
- **Responsive design system** (variables.css, layout.css, components.css)
- **Consistent UI patterns** across all 11 workflow scenarios
- **Production-ready styling** with proper CSS organization

### **Comprehensive Testing ✅ COMPLETE**
```r
devtools::test()     # 118 tests pass
devtools::check()    # 0 errors, 0 warnings
```

### **Performance Optimization ✅ COMPLETE**
- **Cached analysis results** - prevents unnecessary recalculation
- **Intelligent reactivity** - only updates when sidebar changes
- **Background analysis** - non-blocking UI during computation
- **Error boundary** - graceful degradation on analysis failures

## Development Evolution: From Prototype to Production

### ✅ **COMPLETE: All Development Phases Finished**

#### **Phase 1: Foundation** ✅ COMPLETE  
- ✅ Golem-compliant module architecture
- ✅ Constraint-driven UI with business logic
- ✅ Complete parameter control system
- ✅ Full test coverage and dependency management

#### **Phase 2: Visualization Engine** ✅ COMPLETE
- ✅ **mod_plotting_engine**: Interactive Plotly visualizations 
- ✅ **11 workflow scenarios**: All plot types implemented
- ✅ **mod_results_display**: Dynamic results presentation
- ✅ **Export functionality**: Analysis results download

#### **Phase 2.5: Variable Naming Unification** ✅ COMPLETE  
- ✅ **5-layer naming architecture**: UI → Config → API → Processing → Display
- ✅ **Zero naming conflicts**: Complete parameter standardization
- ✅ **Component library integration**: 283 semantic components
- ✅ **Column access optimization**: Warning-free data processing

#### **Phase 3: Real Analysis Integration** ✅ COMPLETE
- ✅ **Full perturbplan integration**: `cost_power_computation`, cost optimization, constrained minimization
- ✅ **Three analysis engines**: Standard, Cost, Constrained scenarios
- ✅ **Parameter translation**: Seamless UI-to-perturbplan mapping
- ✅ **Performance optimization**: Caching, intelligent reactivity

#### **Phase 4: Production Readiness** ✅ COMPLETE
- ✅ **118 tests passing**: Comprehensive test coverage
- ✅ **Error handling**: Graceful degradation and user feedback
- ✅ **Mode switching**: Development/production analysis modes
- ✅ **Performance monitoring**: Optimized for real-time analysis

### 🎯 **CURRENT STATE: PRODUCTION-READY APPLICATION**
The app is now a **fully operational analysis platform** with real mathematical optimization, not a prototype.

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

## 🚧 ACTIVE DEVELOPMENT: Dual-Workflow Implementation

### **Current Project: Sidebar-Base + Slider-Override Architecture**

**Vision**: Implement a dual-workflow system where:
1. **Phase 1 (Sidebar Mode)**: Users configure all parameters via sidebar → Plan button → Analysis → Results
2. **Phase 2 (Slider Mode)**: After first results, sidebar freezes and sliders appear for real-time parameter overrides

### **Implementation Status**: Phase 1.1 COMPLETE ✅

#### **Architecture Overview**:
```
Phase 1: Sidebar (Complete Config) → Plan → Analysis → Results
Phase 2: Sidebar (Frozen) + Sliders (Override Subset) → Real-time Analysis
```

#### **Key Design Decisions**:
- **Sidebar-base**: Sidebar contains complete parameter set, remains master configuration source
- **Slider-override**: Sliders contain only subset of parameters, provide real-time overrides
- **Parameter Source Manager**: Central coordination hub managing sidebar-base + slider-override logic
- **App State Management**: Global reactiveValues for phase tracking and UI state control

### **7-Phase Implementation Plan**:

#### **✅ Phase 1.1: App State Foundation (COMPLETE)**
- **File**: `R/app_server.R:19-25`
- **Implementation**: Added global app state management
```r
app_state <- reactiveValues(
  phase = 1,                    # 1 = sidebar mode, 2 = slider mode
  sidebar_frozen = FALSE,       # Are sidebar inputs disabled?
  sliders_visible = FALSE,      # Should sliders be shown in results?
  initial_config_snapshot = NULL,  # Frozen sidebar config for Phase 2
  plan_button_text = "Plan"     # Button text: "Plan" or "Restart"
)
```

#### **🔄 Phase 1.2: Parameter Source Manager (NEXT)**
- **Goal**: Create central parameter coordination without changing current behavior
- **Implementation**: Pass-through mode where sidebar values flow directly to analysis
- **File**: New `R/mod_parameter_source_manager.R`

#### **📋 Phase 2: Integration Wiring**
- **Goal**: Connect parameter source manager to analysis engine without behavior changes
- **Implementation**: Update analysis engine to accept parameter_source_manager input

#### **📋 Phase 3: Plan/Restart Button Logic**
- **Goal**: Button switches between "Plan" and "Restart" based on app_state$phase
- **Implementation**: Dynamic button text and phase transition logic

#### **📋 Phase 4: Sidebar Freezing/Unfreezing**
- **Goal**: Disable sidebar inputs in Phase 2, enable in Phase 1
- **Implementation**: Conditional `disabled` attributes based on app_state$sidebar_frozen

#### **📋 Phase 5: Conditional Slider Visibility**
- **Goal**: Show sliders only in Phase 2 results panel
- **Implementation**: Conditional UI rendering based on app_state$sliders_visible

#### **📋 Phase 6: Real-time Slider Overrides**
- **Goal**: Sliders override sidebar parameters and trigger immediate analysis
- **Implementation**: Parameter source manager priority logic (slider > sidebar)

#### **📋 Phase 7: Performance & Polish**
- **Goal**: Optimize reactivity, add smooth transitions, error handling
- **Implementation**: Debouncing, caching, UI animations

### **Technical Architecture**:

#### **Parameter Flow (Target State)**:
```
Sidebar Parameters (Complete Set)
       ↓
Parameter Source Manager
       ↓ (Phase 1: sidebar-only)
       ↓ (Phase 2: sidebar + slider overrides)  
       ↓
Analysis Engine
       ↓
Results Display (+ Sliders in Phase 2)
```

#### **App State Transitions**:
```
Phase 1 (Initial):
- sidebar_frozen = FALSE
- sliders_visible = FALSE  
- plan_button_text = "Plan"

Phase 2 (Post-Plan):
- sidebar_frozen = TRUE
- sliders_visible = TRUE
- plan_button_text = "Restart"
```

### **Progress Tracking**:
- **Branch**: `intermediate-rebuild` 
- **Last Commit**: Phase 1.1 app_state foundation complete
- **Test Status**: All functionality preserved, zero regressions
- **Next Step**: Phase 1.2 parameter source manager creation

## Known Issues and TODOs

### Current Development Tasks (Active)

1. **Phase 1.2**: Create parameter source manager in pass-through mode
2. **Phase 2**: Wire integration without behavior changes  
3. **Phase 3-7**: Sequential implementation of dual-workflow features

### Future Enhancement Opportunities (Post-Dual-Workflow)

1. **Advanced Visualization**: Additional plot types (heatmaps, 3D surfaces)
2. **Export Formats**: PDF reports, PowerPoint presentations
3. **Batch Analysis**: Multiple experiment comparison
4. **API Integration**: RESTful API for programmatic access

## Success Metrics

### Development Milestones

- [x] All 11 workflows can be configured through UI
- [x] Parameter control panel behaves according to business rules
- [x] Interactive visualizations display correctly for each workflow
- [x] Integration with perturbplan package functions works
- [x] Visual continuity with original app maintained
- [x] **Real mathematical optimization implemented**
- [x] **Production-ready performance achieved**
- [x] **Comprehensive error handling completed**

### User Experience Goals ✅ ACHIEVED

- ✅ **Real-time analysis results** - Actual perturbplan optimization
- ✅ **Intelligent workflow guidance** - Constraint-driven parameter control
- ✅ **Intuitive parameter interface** - Progressive disclosure with business rules
- ✅ **Comprehensive results export** - Interactive plots and analysis data
- ✅ **Production performance** - Cached results, optimized reactivity
- ✅ **Error resilience** - Graceful degradation with user feedback

## PARAMETER NAMING - FULLY RESOLVED ✅

**COMPLETE**: Parameter naming has been comprehensively standardized in **Phase 2.5**.

### **Current Standard (ALL IMPLEMENTED)**:
- **UI Layer**: `TPM_threshold` (user-facing inputs)
- **Config Layer**: `TPM_threshold_fixed` (inter-module communication)  
- **API Layer**: Automatic translation to perturbplan-compatible parameters
- **Processing Layer**: `sequenced_reads_per_cell` (internal calculations)
- **Display Layer**: "TPM Threshold" (user-facing labels)

### **Translation Handled Automatically**:
```r
# Centralized parameter translation in mod_analysis_engine.R lines 118-126
# UI parameter names → perturbplan function parameters
# No manual intervention needed - system handles all conversions
```

**Zero outstanding parameter naming issues.** System is production-ready.