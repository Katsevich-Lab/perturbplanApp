# PerturbPlan: Constraint-Driven Experimental Design

A Golem-based Shiny application for constraint-driven experimental design and power analysis in CRISPR perturb-seq experiments.

## Quick Start

### 1. Open in RStudio
- Open `perturbplanApp.Rproj` in RStudio
- This will set up the proper package development environment

### 2. Set Up Development Environment
```r
# Install dependencies using Golem development setup
golem::install_dev_deps()
```

### 3. Launch the App
```r
# Load the package and run the app
golem::run_dev()

# Or alternatively:
devtools::load_all()
run_app()
```

The app will open in your browser with the constraint-driven design interface in the left sidebar.

## Development Workflow

### Core Commands
```r
# Development workflow with Golem:
golem::run_dev()        # Launch app in development mode

# After making changes to R files:
devtools::load_all()    # Reload package
run_app()               # Test your changes

# Update documentation:
devtools::document()    # Generate NAMESPACE and man files

# Check package health:
devtools::check()       # Check for issues
devtools::test()        # Run all tests
```

### Package Structure
```
perturbplanApp/
├── R/
│   ├── mod_design_options.R      # Core constraint-driven module ⭐
│   ├── mod_cost_info.R           # Cost parameter module
│   ├── mod_experimental_setup.R  # Biological system setup
│   ├── mod_analysis_choices.R    # Analysis parameters
│   ├── mod_effect_sizes.R        # Effect size assumptions
│   ├── mod_sidebar.R             # Main sidebar container
│   ├── app_*.R                   # Core Golem app files
│   ├── fct_*.R                   # Business logic functions
│   └── utils_*.R                 # Utility functions
├── inst/app/www/                 # Static assets (CSS, JS, images)
├── dev/                          # Golem development scripts
├── docs/                         # Documentation (PRD)
└── tests/testthat/               # Unit tests (Golem structure)
```

## Current Features

✅ **Constraint-Driven Design Interface**
- Progressive 3-step UI: Optimization Framework → Minimization Target → Parameter Control
- Business rule enforcement for parameter constraints
- Real-time validation and auto-parameter setting
- Supports all 11 workflow scenarios from PRD

✅ **Modular Architecture (Golem Best Practices)**
- All modules created with `golem::add_module()`
- Proper namespace management and test coverage
- Conditional cost module display
- Comprehensive sidebar integration

✅ **Complete UI Implementation**
- Design Options, Cost Information, Experimental Setup
- Analysis Choices, Effect Sizes modules
- Interactive collapsible sections
- Ready for placeholder visualizations

## Testing the App

### Core Functionality
1. **Optimization Framework**: Toggle between "Power-only" vs "Power + cost"
2. **Minimization Target**: Select from 5 optimization objectives
3. **Parameter Control**: Watch auto-updates based on target selection
4. **Business Rules**: Test cost restriction, single target enforcement

### Workflow Scenarios
The app supports all 11 workflow combinations:
- **Power-only**: 5 workflows (cells, reads, cost, TPM, fold change minimization)
- **Power + cost**: 6 workflows (TPM/fold change with varying parameter combinations)

## Next Development Steps

📋 **Phase 2: Placeholder Visualizations** (Current Priority)
- Static placeholder plots for all 11 workflow scenarios
- Power curves, cost curves, and equi-power/equi-cost plots
- Interactive plot elements (hover, zoom, selection)
- Results export module with Excel downloads

🔧 **Phase 3: Analysis Engine Integration**
- Connect to `perturbplan` package functions
- Real data processing and dynamic plot generation
- Performance optimization for large parameter grids
- Advanced error handling and validation

🚀 **Phase 4: Production Deployment**
- Docker containerization
- Deployment configuration (Shinyapps.io, Shiny Server)
- Performance monitoring and logging
- User documentation and tutorials

## Documentation

- **PRD**: `docs/PRD-PerturbPlan-v2.Rmd` - Complete product requirements
- **Development Guide**: `CLAUDE.md` - Comprehensive Golem development instructions  
- **Original App**: Located in sibling `perturbplan` package

## Dependencies

### Core Golem Stack
- `golem` - Production-ready Shiny app framework
- `shiny` - Web application framework  
- `shinydashboard` - Dashboard layout components
- `shinyjs` - JavaScript integration for dynamic UI

### UI Enhancement
- `DT`, `htmltools`, `bslib` - Enhanced UI components
- `plotly` - Interactive visualizations (for Phase 2)

### Analysis Integration (Phase 3)
- `perturbplan` - Core CRISPR power analysis functions
- `ggplot2` - Static plot generation

## Troubleshooting

### App Won't Load
```r
# Clear workspace and reload
rm(list = ls())
devtools::load_all()
run_app()
```

### Import Errors
```r
# Update documentation and reload
devtools::document()
devtools::load_all()
```

### Dependency Issues
```r
# Reinstall dependencies using Golem
golem::install_dev_deps()
```

### Module Creation
```r
# Always use Golem for new modules
golem::add_module(name = "module_name", with_test = TRUE)

# Never create modules manually - follow Golem best practices
```

---

**Happy coding!** 🚀 For questions, see `CLAUDE.md` or the PRD documentation.