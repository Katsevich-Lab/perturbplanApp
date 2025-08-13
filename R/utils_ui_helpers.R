#' Create Collapsible Section Header
#'
#' @description Creates a collapsible section header with chevron icon
#'
#' @param ns Namespace function from module
#' @param id Character, section ID
#' @param title Character, section title
#' @param is_open Logical, whether section starts open
#'
#' @return Shiny tag for collapsible header
#'
#' @noRd
create_collapsible_header <- function(ns, id, title, is_open = FALSE) {
  chevron_class <- if (is_open) "fa fa-chevron-down" else "fa fa-chevron-right"
  
  tags$div(
    id = ns(paste0(id, "-header")),
    style = "padding: 10px 15px; cursor: pointer; border-radius: 4px 4px 0 0;",
    onclick = paste0("toggleSection('", ns(paste0(id, "-content")), "', '", ns(paste0(id, "-chevron")), "')"),
    tags$i(id = ns(paste0(id, "-chevron")), class = chevron_class, style = "margin-right: 8px;"),
    tags$strong(title)
  )
}

#' Create Collapsible Section Content
#'
#' @description Creates a collapsible section content container
#'
#' @param ns Namespace function from module
#' @param id Character, section ID
#' @param content Shiny UI content for the section
#' @param is_open Logical, whether section starts open
#'
#' @return Shiny tag for collapsible content
#'
#' @noRd
create_collapsible_content <- function(ns, id, content, is_open = FALSE) {
  display_style <- if (is_open) "block" else "none"
  
  tags$div(
    id = ns(paste0(id, "-content")),
    style = paste0("padding: 15px; display: ", display_style, ";"),
    content
  )
}

#' Create Form Field with Label
#'
#' @description Creates a consistently styled form field with label
#'
#' @param label Character, field label
#' @param input Shiny input element
#' @param help_text Character, optional help text
#'
#' @return Shiny tag for form field
#'
#' @noRd
create_form_field <- function(label, input, help_text = NULL) {
  field_content <- list(
    tags$strong(paste0(label, ":"), style = "font-size: 13px;"),
    input
  )
  
  if (!is.null(help_text)) {
    field_content <- append(field_content, 
      tags$small(help_text, style = "color: #6C757D; font-style: italic; font-size: 11px;")
    )
  }
  
  tags$div(
    style = "margin-bottom: 8px;",
    field_content
  )
}

#' Create Step Section
#'
#' @description Creates a step section with consistent styling
#'
#' @param ns Namespace function from module
#' @param step_id Character, step ID
#' @param step_number Integer, step number
#' @param title Character, step title
#' @param description Character, step description
#' @param content Shiny UI content for the step
#' @param is_visible Logical, whether step is initially visible
#'
#' @return Shiny tag for step section
#'
#' @noRd
create_step_section <- function(ns, step_id, step_number, title, description, content, is_visible = TRUE) {
  display_style <- if (is_visible) "block" else "none"
  
  tags$div(
    id = ns(step_id),
    style = paste0("margin-bottom: 15px; padding-bottom: 10px; border-bottom: 1px solid #E3E6EA; display: ", display_style, ";"),
    tags$h5(paste("Step", step_number, ":", title), style = "color: #4A6B82; margin-bottom: 10px;"),
    tags$p(description, style = "font-size: 12px; margin-bottom: 8px;"),
    content
  )
}

#' Create Info Banner
#'
#' @description Creates an information banner with icon
#'
#' @param message Character, banner message
#' @param type Character, banner type ("info", "warning", "success", "error")
#' @param icon Character, FontAwesome icon name
#'
#' @return Shiny tag for info banner
#'
#' @noRd
create_info_banner <- function(message, type = "info", icon = "info-circle") {
  type_classes <- list(
    "info" = "file-upload-info",
    "warning" = "alert alert-warning",
    "success" = "file-upload-success", 
    "error" = "alert alert-danger"
  )
  
  tags$div(
    class = type_classes[[type]] %||% "alert alert-info",
    style = "border-radius: 3px; padding: 6px; margin: 5px 0;",
    tags$small(
      tags$i(class = paste("fa", paste0("fa-", icon)), style = "margin-right: 3px;"),
      message,
      style = "font-size: 11px;"
    )
  )
}

#' Create Parameter Control Row
#'
#' @description Creates a parameter control row with varying/fixed/minimizing options
#'
#' @param ns Namespace function from module
#' @param param_name Character, parameter name
#' @param param_label Character, parameter label for display
#' @param control_input Shiny input for parameter control
#' @param fixed_input Shiny input for fixed value (shown conditionally)
#'
#' @return Shiny tag for parameter control row
#'
#' @noRd
create_parameter_control_row <- function(ns, param_name, param_label, control_input, fixed_input = NULL) {
  content <- list(
    tags$strong(paste0(param_label, ":"), style = "font-size: 13px;"),
    control_input
  )
  
  if (!is.null(fixed_input)) {
    content <- append(content, 
      conditionalPanel(
        condition = paste0("input['", ns(paste0(param_name, "_control")), "'] == 'fixed'"),
        fixed_input
      )
    )
  }
  
  tags$div(
    style = "margin-bottom: 8px;",
    content
  )
}
