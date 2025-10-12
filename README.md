# PerturbPlan App

PerturbPlan is a Shiny app for perturb-seq and TAP-seq experimental design. It is structured around solving 11 commonly encountered design problems, balancing power and cost of the planned experiment. The workflow of the app proceeds as follows:

1. **Select a design problem**: Choose one of 11 predefined design problems that best matches your experimental goals.

2. **Configure design parameters**: Set the parameters for your experimental choices, analysis choices, expected effect sizes, and (optionally) advanced settings. Click "Plan".

3. **View your analysis results**: The plot illustrates graphically how the design problem was solved, and the table below summarizes the optimal design parameters.

4. **Explore parameter settings.** Use the sliders to adjust key parameters and see how they affect the optimal design. Pin parameter settings to compare multiple designs.

5. **Export your results.** Click the export buttons to download the plot and a detailed Excel spreadsheet containing the results.

6. **Start over.** Click the "Restart" button to start from scratch.

We elaborate on steps 1-3, and 5 in the [full documentation](https://katsevich-lab.github.io/perturbplanApp/).

## Quick Start

### Installation
```r
# Install from GitHub
devtools::install_github("Katsevich-Lab/perturbplanApp")
```

### Launch the App
```r
library(perturbplanApp)
run_app()
```

## Documentation

For complete documentation on design problems, parameter configuration, and result interpretation, please visit the [PerturbPlan App documentation](https://katsevich-lab.github.io/perturbplanApp/).

## Development

See [CLAUDE.md](CLAUDE.md) for comprehensive development guidelines and Golem best practices.
