# perturbplanApp 0.1.1

## Changes

* Updated `perturbplan` dependency from v0.3.0 to v0.3.1.
* Updated custom pilot data validation to match `library_estimation()` format.
* Fixed deployment issue where `pkgload`, `desc`, and `pkgbuild` were dropped from manifest.
* Updated 15 dependency packages to latest versions.

# perturbplanApp 0.1.0

## New features

* Full integration with `perturbplan` package (v0.3.1) for real-time power analysis and experimental optimization.
* Constraint-driven workflow supporting 11 design scenarios: 5 power-only and 6 power+cost optimization workflows.
* Three analysis engines: standard power analysis, cost minimization, and constrained optimization.
* Interactive Plotly visualizations for all workflow scenarios with log-scale axes for cells and reads parameters.
* Dual-workflow architecture: sidebar-based configuration (Phase 1) with slider overrides for real-time parameter adjustment (Phase 2).
* Support for both Perturb-seq and TAP-seq assay types with assay-aware parameter labels and transformations.
* Custom pilot data upload with validation for Perturb-seq experiments (RDS format).
* Built-in K562 reference expression data for quick-start analysis.
* Custom gene list upload functionality.
* Excel export of analysis results including parameter settings and pilot data.
* PDF export for publication-ready plots with legend annotations.
* Comprehensive tooltip system providing parameter descriptions and guidance.
* Progressive disclosure UI with collapsible sidebar sections.
* pkgdown documentation site with launch button and app schematic.

## UI improvements

* Slider-adjustable badges on sidebar inputs with widened sidebar layout.
* Two-row slider layout with responsive design for parameter adjustment.
* Solution table with bold formatting for varying and optimizing parameters.
* Diamond annotations marking optimal solutions on plots.
* Cost budget horizontal lines on constrained optimization plots.
* Blur-based input clamping for all numeric parameters.
* MOI=1 control group logic with explanatory text.
* Assay-aware Expression threshold labeling (TPM for Perturb-seq, UMIs/cell for TAP-seq).

## Bug fixes

* Fixed Expression threshold not freezing in Phase 2 sidebar.
* Fixed app crash when MOI field is cleared.
* Fixed fold change selection logic to choose value closest to 1.
* Fixed constrained minimization plot to use points instead of duplicate lines.
* Corrected MOI value in control group description.
* Fixed layout issue causing results panels to shift to bottom.
* Added informative error when no designs meet cost budget in constrained minimization.

## Infrastructure

* Golem framework architecture with modular Shiny components.
* 5-layer parameter naming standardization (UI, Config, API, Processing, Display).
* Component library with 283 semantic CSS components.
* 118 passing tests with comprehensive coverage.
* Deployment via Posit Connect Cloud with auto-deployment from `dev` branch.
