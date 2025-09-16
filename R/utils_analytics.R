#' Configure Google Analytics
#' This function must be called from a Shiny app's UI.
#' You can call use_google_analytics() from anywhere inside the UI, as long as the final app UI
#' (HTML code) contains the result of use_google_analytics().
#' @param ga_id The Google Analytics Tag ID, something like "G-ABCD12E34F".
use_google_analytics <- function(ga_id) {
  shiny::tags$head(shiny::HTML(paste0(
    "<!-- Google tag (gtag.js) -->\n",
    '<script async src="https://www.googletagmanager.com/gtag/js?id=', ga_id, '"></script>\n',
    "<script>\n",
    "  window.dataLayer = window.dataLayer || [];\n",
    "  function gtag(){dataLayer.push(arguments);}\n",
    "  gtag('js', new Date());\n",
    "  gtag('config', '", ga_id, "');\n",
    "</script>\n"
  )))
}