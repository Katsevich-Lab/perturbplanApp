#' Create Navbar Links for Dashboard Header
#'
#' @description Generates navigation links for external resources (documentation and GitHub)
#' to be displayed in the dashboard header. Links open in new tabs.
#'
#' @return A tagList containing the navbar HTML structure with styled links
#'
#' @details
#' The navbar includes two links:
#' \itemize{
#'   \item Documentation: Links to the app's vignette documentation
#'   \item GitHub: Links to the app repository (dev branch)
#' }
#'
#' All links open in new browser tabs.
#'
#' @examples
#' \dontrun{
#' # In dashboard header
#' dashboardHeader(
#'   title = "PerturbPlan",
#'   tags$li(class = "dropdown", create_navbar_links())
#' )
#' }
#'
#' @noRd
#'
#' @importFrom shiny tags
create_navbar_links <- function() {
  tags$ul(
    class = "nav navbar-nav navbar-right perturbplan-navbar",

    # Documentation link
    tags$li(
      tags$a(
        href = "www/perturbplanapp.html",
        target = "_blank",
        tags$i(class = "fa fa-book"),
        " Documentation"
      )
    ),

    # GitHub link
    tags$li(
      tags$a(
        href = "https://github.com/Katsevich-Lab/perturbplanApp/tree/dev",
        target = "_blank",
        tags$i(class = "fa fa-github"),
        " GitHub"
      )
    )
  )
}
