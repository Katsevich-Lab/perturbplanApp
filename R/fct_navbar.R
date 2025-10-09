#' Create Navbar Links for Dashboard Header
#'
#' @description Generates navigation links for external resources (GitHub, package docs, documentation)
#' to be displayed in the dashboard header. Links open in new tabs.
#'
#' @param show_documentation Logical, whether to enable the documentation link (default: FALSE).
#'   When FALSE, the documentation link appears grayed out and non-clickable.
#'
#' @return A tagList containing the navbar HTML structure with styled links
#'
#' @details
#' The navbar includes three links:
#' \itemize{
#'   \item GitHub: Links to the app repository (dev branch)
#'   \item perturbplan: Links to the perturbplan package documentation
#'   \item Documentation: Placeholder link (disabled by default, for future use)
#' }
#'
#' All active links open in new browser tabs. The disabled documentation link
#' is styled with reduced opacity and no hover effects.
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
create_navbar_links <- function(show_documentation = FALSE) {
  tags$ul(
    class = "nav navbar-nav navbar-right perturbplan-navbar",

    # GitHub link
    tags$li(
      tags$a(
        href = "https://github.com/Katsevich-Lab/perturbplanApp/tree/dev",
        target = "_blank",
        tags$i(class = "fa fa-github"),
        " GitHub"
      )
    ),

    # perturbplan Package link
    tags$li(
      tags$a(
        href = "https://katsevich-lab.github.io/perturbplan",
        target = "_blank",
        tags$i(class = "fa fa-cube"),
        " perturbplan"
      )
    ),

    # Documentation link (placeholder - disabled by default)
    tags$li(
      tags$a(
        href = if (show_documentation) "#" else "#",  # Will be updated when docs are ready
        class = if (!show_documentation) "disabled" else NULL,
        target = if (show_documentation) "_blank" else NULL,
        tags$i(class = "fa fa-book"),
        " Documentation"
      )
    )
  )
}
