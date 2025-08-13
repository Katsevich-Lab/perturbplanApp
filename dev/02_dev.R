# Building a Prod-Ready, Robust Shiny Application.
#
# README: each step of the dev files is optional, and you don't have to
# fill every dev scripts before getting started.
# 01_start.R should be filled at start.
# 02_dev.R should be used to keep track of your development during the project.
# 03_deploy.R should be used once you need to deploy your app.
#
#
###################################
#### CURRENT FILE: DEV SCRIPT #####
###################################

# Engineering

## Dependencies ----
## Amend DESCRIPTION with dependencies read from package code parsing
## install.packages('attachment') # if needed.
attachment::att_amend_desc()

## Modules Created ----
## These modules have been implemented for the constraint-driven workflow:

# Core sidebar modules (created manually for modular architecture)
# golem::add_module(name = "design_options", with_test = TRUE)     # Constraint-driven workflow
# golem::add_module(name = "experimental_setup", with_test = TRUE) # Data uploads and system selection
# golem::add_module(name = "analysis_choices", with_test = TRUE)   # Test parameters and gene pairs  
# golem::add_module(name = "effect_sizes", with_test = TRUE)       # Assumed effect size parameters
# golem::add_module(name = "sidebar", with_test = TRUE)            # Main sidebar composition

## Helper functions Created ----
## Business logic and utility functions following Golem patterns:

# Workflow detection and validation
golem::add_fct("workflow_detection", with_test = TRUE)   # Detect 11 workflow types
golem::add_fct("parameter_helpers", with_test = TRUE)    # Parameter grid creation and validation

# Validation utilities  
golem::add_utils("validation", with_test = TRUE)         # File upload and data validation
golem::add_utils("ui_helpers", with_test = TRUE)         # UI component creation helpers

## External resources ----
## Assets have been created and configured for proper Golem asset management:

# CSS and JavaScript files (created manually to preserve original perturbplan styling)
# golem::add_css_file("perturbplan_styles")      # Main styling (copied from original app)
# golem::add_js_file("perturbplan_interactions") # Collapsible sections functionality

# Files created at:
# - inst/app/www/perturbplan_styles.css      # Complete styling from original app
# - inst/app/www/perturbplan_interactions.js # JavaScript for UI interactions
# - inst/app/www/favicon.ico                 # Application icon

## Asset Management ----
## Using proper Golem asset loading in golem_add_external_resources():
# - bundle_resources() automatically includes CSS files
# - JavaScript manually included with tags$script(src = "www/perturbplan_interactions.js")
# - Favicon loaded with favicon()
# - Resource path configured with add_resource_path("www", app_sys("app/www"))

## Tests Created ----
## Comprehensive test suite following Golem best practices:

# Basic app infrastructure
usethis::use_test("app")                       # Main app testing

# Module tests (created manually with testServer patterns)
# test-mod_design_options.R     # Constraint-driven workflow testing
# test-mod_experimental_setup.R # Data upload and system selection testing
# test-mod_analysis_choices.R   # Analysis parameter testing
# test-mod_effect_sizes.R       # Effect size parameter testing
# test-mod_sidebar.R            # Integrated sidebar testing

# Helper function tests (created by Golem)
# test-fct_workflow_detection.R # Workflow detection and validation testing
# test-fct_parameter_helpers.R  # Parameter grid and formatting testing
# test-utils_validation.R       # File upload and data validation testing
# test-utils_ui_helpers.R       # UI component helper testing

# Documentation

## Vignette ----
usethis::use_vignette("perturbplanApp")
devtools::build_vignettes()

## Code Coverage----
## Set the code coverage service ("codecov" or "coveralls")
usethis::use_coverage()

# Create a summary readme for the testthat subdirectory
covrpage::covrpage()

## CI ----
## Use this part of the script if you need to set up a CI
## service for your application
##
## (You'll need GitHub there)
usethis::use_github()

# GitHub Actions
usethis::use_github_action()
# Chose one of the three
# See https://usethis.r-lib.org/reference/use_github_action.html
usethis::use_github_action_check_release()
usethis::use_github_action_check_standard()
usethis::use_github_action_check_full()
# Add action for PR
usethis::use_github_action_pr_commands()

# Circle CI
usethis::use_circleci()
usethis::use_circleci_badge()

# Jenkins
usethis::use_jenkins()

# GitLab CI
usethis::use_gitlab_ci()

# You're now set! ----
# go to dev/03_deploy.R
rstudioapi::navigateToFile("dev/03_deploy.R")
