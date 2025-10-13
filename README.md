# PerturbPlan App

PerturbPlan is a Shiny app for Perturb-seq and TAP-seq experimental design. In both types of experiments, many genetic perturbations are introduced into a population of cells, and the transcriptomes of the perturbed cells are profiled using single-cell RNA-sequencing (for TAP-seq, only a set of pre-selected genes is sequenced). A key task for Perturb-seq and TAP-seq data analysis is to identify among a set of perturbation-gene pairs those where the perturbation has a significant effect on the expression of the gene.

The key metrics for experimental design are **power** (the expected proportion of true perturbation-gene effects that are detected as significant) and **cost** (the total cost of the experiment, equal to the sum of library preparation and sequencing costs). There are a number of concrete design problems that arise when balancing power and cost. The PerturbPlan is structured around solving 11 commonly encountered design problems. The workflow of the app is as follows:

1. **Select a design problem**: Choose one of 11 predefined design problems that best matches your experimental goals.

2. **Configure design parameters**: Set the parameters for your experimental choices, analysis choices, expected effect sizes, and (optionally) advanced settings. Click "Plan". 

3. **View your analysis results**: The plot illustrates graphically how the design problem was solved, and the table below summarizes the optimal design parameters.

4. **Explore parameter settings.** Use the sliders to adjust key parameters and see how they affect the optimal design. Pin parameter settings to compare multiple designs.

5. **Export your results.** Click the export buttons to download the plot and a detailed Excel spreadsheet containing the results.

6. **Start over.** Click the "Restart" button to start from scratch.

We elaborate on steps 1 and 2 in the [full documentation](https://katsevich-lab-perturbplanappdev.share.connect.posit.cloud/_w_29fe7fc525454dc3948529f065cec6b5/www/perturbplanapp.html). 
