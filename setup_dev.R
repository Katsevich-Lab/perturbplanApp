# Development Setup Script for PerturbPlan App
# Run this script to set up your local development environment

# Install required packages if not already installed
required_packages <- c(
  "devtools",
  "golem", 
  "shiny",
  "DT",
  "htmltools",
  "bslib",
  "testthat",
  "pkgload",
  "roxygen2"
)

# Function to install missing packages
install_if_missing <- function(packages) {
  new_packages <- packages[!(packages %in% installed.packages()[,"Package"])]
  if(length(new_packages)) {
    message("Installing missing packages: ", paste(new_packages, collapse = ", "))
    install.packages(new_packages, repos = "https://cloud.r-project.org/")
  } else {
    message("All required packages are already installed!")
  }
}

# Install missing packages
install_if_missing(required_packages)

# Load development tools
library(devtools)
library(golem)

# Document the package (generate NAMESPACE, man files)
message("\n=== Documenting package ===")
devtools::document()

# Load the package for development
message("\n=== Loading package ===")
devtools::load_all()

# Print helpful development commands
cat("\n")
cat("=== Development Environment Ready! ===\n")
cat("\n")
cat("Useful commands for development:\n")
cat("  devtools::load_all()     # Reload package after changes\n")
cat("  devtools::document()     # Update documentation\n") 
cat("  devtools::check()        # Check package for issues\n")
cat("  devtools::test()         # Run tests\n")
cat("  run_app()                # Launch the Shiny app\n")
cat("  golem::run_dev()         # Run app in development mode\n")
cat("\n")
cat("To launch the app:\n")
cat("  1. Run: run_app()\n")
cat("  2. Open browser to the displayed URL\n")
cat("  3. Navigate to 'Design Options' tab to test the constraint-driven interface\n")
cat("\n")
cat("Project structure:\n")
cat("  R/                       # R source files\n")
cat("  ├── mod_design_options.R # Core constraint-driven module\n")
cat("  ├── app_ui.R             # Main UI\n")
cat("  ├── app_server.R         # Main server\n")
cat("  └── run_app.R            # App launcher\n")
cat("  \n")
cat("  inst/app/www/           # Static assets (CSS, JS, images)\n")
cat("  dev/                    # Development scripts\n")
cat("  docs/                   # Documentation (PRD)\n")
cat("  tests/                  # Unit tests\n")
cat("\n")
cat("Happy coding! 🚀\n")