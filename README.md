# PerturbPlan App

<style>
.launch-button {
  display: inline-block;
  padding: 16px 16px;
  background-color: #4472C4;
  color: white;
  text-decoration: none;
  border-radius: 6px;
  font-weight: bold;
  font-size: 24px;
  border: 2px solid black;
  transition: all 0.3s ease;
}
.launch-button:hover {
  background-color: #365a9e;
  box-shadow: 0 4px 8px rgba(0, 0, 0, 0.2);
}
</style>

PerturbPlan is a Shiny app for Perturb-seq and TAP-seq experimental design. It helps balance power and cost, with an emphasis on speed, flexibility, and interactivity.

<p align="center">
  <a href="https://katsevich-lab-perturbplan.share.connect.posit.cloud/" class="launch-button">
    🚀 Launch App
  </a>
</p>

## Interface overview

<p align="center">
  <img src="man/figures/schematic.png" alt="PerturbPlan App Schematic" width="100%"/>
</p>

PerturbPlan is structured around solving 11 commonly encountered design problems. The workflow of the app is as follows:

1. **Select a design problem**: Choose one of 11 predefined design problems that best matches your experimental goals.

2. **Configure design parameters**: Set the parameters for your experimental choices, analysis choices, expected effect sizes, and (optionally) advanced settings. Click "Plan".

3. **View your analysis results**: The plot illustrates graphically how the design problem was solved, and the table below summarizes the optimal design parameters.

4. **Explore parameter settings.** Use the sliders to adjust key parameters and see how they affect the optimal design. Pin parameter settings to compare multiple designs.

5. **Export your results.** Click the export buttons to download the plot and a detailed Excel spreadsheet containing the results.

6. **Start over.** Click the "Restart" button to start from scratch.

We elaborate on steps 1 and 2 in the [full documentation](articles/perturbplanapp.html).
