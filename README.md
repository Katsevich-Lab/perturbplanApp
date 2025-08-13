# PerturbPlan App v2: Constraint-Driven Experimental Design

A Golem-based Shiny application for constraint-driven experimental design and power analysis in CRISPR perturb-seq experiments.

## Quick Start

### 1. Open in RStudio
- Open `perturbplanApp.Rproj` in RStudio
- This will set up the proper package development environment

### 2. Set Up Development Environment
```r
# Run the setup script (only needed once)
source("setup_dev.R")
```

### 3. Launch the App
```r
# Load the package and run the app
devtools::load_all()
run_app()
```

The app will open in your browser. Navigate to the **"Design Options"** tab to test the constraint-driven interface.

## Development Workflow

### Core Commands
```r
# After making changes to R files:
devtools::load_all()    # Reload package
run_app()               # Test your changes

# Update documentation:
devtools::document()    # Generate NAMESPACE and man files

# Check package health:
devtools::check()       # Check for issues
```

### Package Structure
```
perturbplanApp/
├── R/
│   ├── mod_design_options.R    # Core constraint-driven module ⭐
│   ├── app_ui.R                # Main UI with tab navigation
│   ├── app_server.R            # Main server logic
│   └── run_app.R               # App launcher
├── inst/app/www/               # Static assets (CSS, JS, images)
├── dev/                        # Development scripts
├── docs/                       # Documentation (PRD)
└── tests/                      # Unit tests
```

## Current Features (Phase 1)

✅ **Constraint-Driven Design Interface**
- 3-section UI: Optimization Framework, Minimization Target, Parameter Control
- Business logic for auto-setting minimizing parameters
- Real-time validation and user feedback
- Supports all 11 workflow scenarios from PRD

✅ **Golem Architecture**
- Modular Shiny design with proper namespace management
- Development-ready package structure
- Integrated navigation and placeholder modules

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

🔄 **Phase 2: Module Integration** (In Progress)
- Enhanced parameter control matrix UI
- Integration with perturbplan package functions
- Workflow detection and routing logic

📋 **Phase 3: Placeholder Visualizations**
- Power curves for 5 power-only designs
- Cost-power trade-off plots
- Interactive visualization features

🔧 **Phase 4: Analysis Engine**
- Real analysis function integration
- Results display and export functionality

## Documentation

- **PRD**: `docs/PRD-PerturbPlan-v2.Rmd` - Complete product requirements
- **Development Guide**: `CLAUDE.md` - Detailed development instructions
- **Original App**: Located in sibling `perturbplan` package

## Dependencies

### Core
- `golem` - Shiny app framework
- `shiny` - Web application framework
- `DT`, `htmltools`, `bslib` - UI components

### Analysis (Future)
- `perturbplan` - Core analysis functions (to be added)

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
# Reinstall dependencies
source("setup_dev.R")
```

---

**Happy coding!** 🚀 For questions, see `CLAUDE.md` or the PRD documentation.