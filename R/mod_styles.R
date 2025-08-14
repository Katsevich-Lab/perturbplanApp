#' UI Styles Module - Golem Asset Management
#'
#' @description Initializes UI enhancements using proper Golem workflow.
#' Static assets (CSS/JS) are automatically bundled by golem_add_external_resources().
#' 
#' Assets managed by Golem:
#' - perturbplan_styles.css: Custom styling and color scheme
#' - perturbplan_interactions.js: Collapsible sections functionality
#'
#' @noRd
#'
#' @importFrom shinyjs useShinyjs
create_styles <- function() {
  list(
    useShinyjs()
    # All static assets automatically included via bundle_resources()
    # in golem_add_external_resources() function
  )
}