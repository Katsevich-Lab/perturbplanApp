---
title: "PerturbPlan App"
output:
  html_document:
    toc: true
    toc_float: true
knit: (function(inputFile, encoding) {
  rmarkdown::render(inputFile, encoding = encoding,
  output_file = file.path(dirname(dirname(inputFile)), 'inst/app/www/perturbplanapp.html')) })
vignette: >
  %\VignetteIndexEntry{PerturbPlan App}
  %\VignetteEngine{knitr::rmarkdown}
  %\VignetteEncoding{UTF-8}
---




``` r
library(perturbplanApp)
```
