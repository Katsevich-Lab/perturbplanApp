#' UI Styles Module - Now Using Golem Asset Management
#'
#' @description Loads CSS and JavaScript using proper Golem workflow
#' Assets are stored in inst/app/www/ and managed by Golem
#'
#' @noRd
#'
#' @importFrom shinyjs useShinyjs
create_styles <- function() {
  list(
    useShinyjs()
    # CSS and JS are now automatically loaded by golem_add_external_resources()
    # Files: inst/app/www/perturbplan_styles.css and perturbplan_interactions.js
  )
}