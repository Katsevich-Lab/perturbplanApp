#' Biological System Configuration
#'
#' @description Centralized configuration for biological systems with author attribution.
#' Maps UI display names (with author), backend values (for perturbplan), and dataset names.
#'
#' @return List of biological system configurations, each containing:
#'   \itemize{
#'     \item ui_display: User-facing name with author (e.g., "K562 (Gasperini)")
#'     \item backend_value: Value used in code/perturbplan (e.g., "K562")
#'     \item dataset_name: Full dataset name in perturbplan package (e.g., "K562_Gasperini")
#'   }
#'
#' @details
#' This configuration excludes K562_10x and reference_expression_datasets from perturbplan.
#' The mapping handles minor naming differences (e.g., "THP-1" backend vs "THP1_Yao" dataset).
#'
#' @noRd
get_biological_system_config <- function() {
  list(
    list(
      ui_display = "K562 (Gasperini)",
      backend_value = "K562",
      dataset_name = "K562_Gasperini"
    ),
    list(
      ui_display = "A549 (Sakellaropoulos)",
      backend_value = "A549",
      dataset_name = "A549_Sakellaropoulos"
    ),
    list(
      ui_display = "THP-1 (Yao)",
      backend_value = "THP-1",
      dataset_name = "THP1_Yao"
    ),
    list(
      ui_display = "T CD8 (Shifrut)",
      backend_value = "T_CD8",
      dataset_name = "T_CD8_Shifrut"
    ),
    list(
      ui_display = "iPSC (Tian)",
      backend_value = "iPSC",
      dataset_name = "iPSC_Tian"
    ),
    list(
      ui_display = "iPSC neuron (Tian)",
      backend_value = "iPSC_neuron",
      dataset_name = "iPSC_neuron_Tian"
    )
  )
}

#' Get UI Choices for Biological System Selector
#'
#' @description Creates a named list suitable for shiny::selectInput choices parameter.
#' Display names include author attribution, values are backend-compatible identifiers.
#'
#' @return Named list with structure: list("Display Name" = "backend_value", ...)
#'   Always includes "Custom" as the last option.
#'
#' @examples
#' \dontrun{
#' selectInput("biological_system", "Reference expression data:",
#'            choices = get_biological_system_choices(),
#'            selected = "K562")
#' }
#'
#' @noRd
get_biological_system_choices <- function() {
  config <- get_biological_system_config()

  # Convert config to named list for selectInput
  choices <- lapply(config, function(x) {
    stats::setNames(x$backend_value, x$ui_display)
  })

  # Combine all choices and add "Custom" option
  c(do.call(c, choices), list("Custom" = "Custom"))
}
