# External Data Directory

This directory contains example data files for the PerturbPlan Shiny application.

## Contents

### Custom Pilot Data Examples
- `iPSC_10x.rda` - Example custom pilot data file with the correct structure (iPSC cell line data)

## Custom Pilot Data Structure

Custom pilot data files should be RDS files containing a list with this structure:

```r
pilot_data <- list(
  baseline_expression_stats = data.frame(
    response_id = c("ENSG00000141510", "ENSG00000157764", ...),        # Ensembl gene IDs
    relative_expression = c(1.23e-05, 4.56e-06, ...),                  # TPM/1e6 scale  
    expression_size = c(0.45, 1.23, ...)                               # Dispersion parameters
  ),
  library_parameters = list(
    UMI_per_cell = 15000,    # Maximum UMI per cell parameter
    variation = 0.25         # PCR bias variation parameter  
  )
)
```

## Usage

1. **Upload in App**: Use the "Custom" option in "Reference expression data" section
2. **File Requirements**: 
   - Must be `.rds` format
   - Maximum size: 50MB
   - Must contain the exact structure shown above
3. **Validation**: The app will validate structure and provide feedback

## Creating Custom Data

Use the data creation functions available in the main perturbplan package to generate properly formatted custom pilot data from your own expression data.