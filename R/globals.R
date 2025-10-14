#' Global variables
#'
#' @description Defines global variables used by various functions to avoid R CMD check NOTEs
#' about "no visible binding for global variable"
#'
#' @noRd
#' @importFrom utils globalVariables
utils::globalVariables(c(
  # Variables from data manipulation (dplyr/ggplot2)
  "cells_per_target",
  "cost_of_interest", 
  "sequenced_reads_per_cell",
  "x",
  "y",
  "total_cost",
  "cells",
  "reads", 
  "power",
  "meets_threshold"
))