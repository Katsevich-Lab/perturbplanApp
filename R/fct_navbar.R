#' Create Navbar Links for Dashboard Header
#'
#' @description Generates navigation links for external resources (documentation and GitHub)
#' to be displayed in the dashboard header. Links open in new tabs.
#'
#' @return A tagList containing the navbar HTML structure with styled links
#'
#' @details
#' The navbar includes:
#' \itemize{
#'   \item Help dropdown menu with:
#'     \itemize{
#'       \item Documentation: Links to the app's vignette documentation
#'       \item Questions and Feature Requests: Links to GitHub Discussions
#'       \item Bug Reports: Links to GitHub Issues
#'     }
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

    # Help dropdown menu
    tags$li(
      class = "dropdown",
      tags$a(
        href = "#",
        class = "dropdown-toggle",
        `data-toggle` = "dropdown",
        tags$i(class = "fa fa-question-circle"),
        " Help ",
        tags$span(class = "caret")
      ),
      tags$ul(
        class = "dropdown-menu",
        tags$li(
          tags$a(
            href = "www/perturbplanapp.html",
            target = "_blank",
            tags$i(class = "fa fa-book"),
            " Documentation"
          )
        ),
        tags$li(
          tags$a(
            href = "https://github.com/Katsevich-Lab/perturbplan/discussions",
            target = "_blank",
            tags$i(class = "fa fa-comments"),
            " Questions and Feature Requests"
          )
        ),
        tags$li(
          tags$a(
            href = "https://github.com/Katsevich-Lab/perturbplanApp/issues",
            target = "_blank",
            tags$i(class = "fa fa-bug"),
            " Bug Reports"
          )
        )
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
