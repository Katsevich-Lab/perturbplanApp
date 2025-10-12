#' UI Tooltip Helper Functions
#'
#' @description Reusable functions for adding hover tooltips to UI elements.
#' Uses HTML title attribute (Option A) for simple, accessible tooltips.
#' Font Awesome 6 icons are used for visual indicators.
#'
#' @name ui-tooltips
NULL

#' Add Tooltip to UI Label
#'
#' @description Wraps a label with hover tooltip using HTML title attribute.
#' Provides two display modes: info icon next to label, or hoverable label.
#'
#' @param label Character. The label text to display
#' @param tooltip_id Character. ID to look up tooltip content from tooltip library
#' @param use_icon Logical. If TRUE, shows info icon; if FALSE, makes label hoverable
#'
#' @return HTML tag with tooltip (shiny::tags object)
#'
#' @examples
#' # With info icon
#' add_tooltip("Target Power:", "target_power", use_icon = TRUE)
#'
#' # Hoverable label
#' add_tooltip("Cost/cell ($):", "cost_per_cell", use_icon = FALSE)
#'
#' @noRd
#'
#' @importFrom shiny tags
add_tooltip <- function(label, tooltip_id, use_icon = TRUE) {
  # Get tooltip text from centralized library
  tooltip_text <- get_tooltip_text(tooltip_id)

  # If no tooltip available, return plain label
  if (tooltip_text == "") {
    if (!validate_tooltip_id(tooltip_id)) {
      warning("Tooltip ID '", tooltip_id, "' not found in tooltip library")
    }
    return(label)
  }

  if (use_icon) {
    # Mode 1: CSS-based tooltip (works when browser tooltips don't)
    # Tooltip is positioned relative to the icon, not the whole container
    tags$span(
      label,
      tags$sup(
        class = "tooltip-icon",
        "ⓘ",
        tags$span(
          class = "tooltip-text",
          tooltip_text
        )
      )
    )
  } else {
    # Mode 2: Hoverable label without icon (for dropdown labels)
    tags$span(
      class = "tooltip-label-container",
      tags$span(
        label,
        class = "tooltip-label-text",
        style = "border-bottom: 1px dotted #888; cursor: help;"
      ),
      tags$span(
        class = "tooltip-text",
        tooltip_text
      )
    )
  }
}

#' Add Tooltip to Numeric Input
#'
#' @description Wraps a numericInput label with tooltip.
#' Helper function specifically for numericInput fields.
#'
#' @param input_id Character. The input ID for the numeric input
#' @param label Character. The label text
#' @param tooltip_id Character. ID to look up tooltip content
#' @param value Numeric. Default value
#' @param min Numeric. Minimum value
#' @param max Numeric. Maximum value
#' @param step Numeric. Step size
#' @param ns Function. Namespace function (optional)
#'
#' @return Shiny UI element with tooltip
#'
#' @noRd
#'
#' @importFrom shiny numericInput
add_tooltip_numeric_input <- function(input_id, label, tooltip_id, value, min = NA, max = NA, step = NA, ns = NULL) {
  # Create label with tooltip
  label_with_tooltip <- add_tooltip(label, tooltip_id, use_icon = TRUE)

  # Create numeric input with namespaced ID if ns provided
  final_id <- if (!is.null(ns)) ns(input_id) else input_id

  numericInput(
    inputId = final_id,
    label = label_with_tooltip,
    value = value,
    min = min,
    max = max,
    step = step
  )
}

#' Add Tooltip to Select Input
#'
#' @description Wraps a selectInput label with tooltip.
#' Helper function specifically for selectInput fields.
#'
#' @param input_id Character. The input ID for the select input
#' @param label Character. The label text
#' @param tooltip_id Character. ID to look up tooltip content
#' @param choices List. Named list of choices
#' @param selected Character. Selected value
#' @param ns Function. Namespace function (optional)
#'
#' @return Shiny UI element with tooltip
#'
#' @noRd
#'
#' @importFrom shiny selectInput
add_tooltip_select_input <- function(input_id, label, tooltip_id, choices, selected = NULL, ns = NULL) {
  # Create label with tooltip
  label_with_tooltip <- add_tooltip(label, tooltip_id, use_icon = TRUE)

  # Create select input with namespaced ID if ns provided
  final_id <- if (!is.null(ns)) ns(input_id) else input_id

  selectInput(
    inputId = final_id,
    label = label_with_tooltip,
    choices = choices,
    selected = selected
  )
}

#' Add Tooltip to Header Tag
#'
#' @description Adds tooltip to h5/h6 header tags commonly used in sidebar.
#' Useful for section headers with info icons.
#'
#' @param label Character. The header text
#' @param tooltip_id Character. ID to look up tooltip content
#' @param header_level Character. HTML header level ("h5" or "h6")
#' @param class Character. CSS class for the header
#'
#' @return Shiny tags$h5 or tags$h6 with tooltip
#'
#' @noRd
#'
#' @importFrom shiny tags
add_tooltip_header <- function(label, tooltip_id, header_level = "h6", class = "step-header") {
  # Create label with tooltip
  label_with_tooltip <- add_tooltip(label, tooltip_id, use_icon = TRUE)

  # Create appropriate header tag
  if (header_level == "h5") {
    tags$h5(label_with_tooltip, class = class)
  } else if (header_level == "h6") {
    tags$h6(label_with_tooltip, class = class)
  } else {
    stop("header_level must be 'h5' or 'h6'")
  }
}

#' Add Tooltip to Inline Span
#'
#' @description Adds tooltip to inline text spans (like "Cost/cell ($):").
#' Useful for inline labels in custom input layouts.
#'
#' @param label Character. The span text
#' @param tooltip_id Character. ID to look up tooltip content
#' @param style Character. Additional inline CSS styles
#'
#' @return Shiny tags$span with tooltip
#'
#' @noRd
#'
#' @importFrom shiny tags
add_tooltip_span <- function(label, tooltip_id, style = NULL) {
  # Get tooltip text
  tooltip_text <- get_tooltip_text(tooltip_id)

  # If no tooltip, return plain span
  if (tooltip_text == "") {
    return(tags$span(label, style = style))
  }

  # Create span with CSS-based tooltip (same as icon version)
  tags$span(
    style = style,
    tags$span(label),
    tags$sup(
      class = "tooltip-icon",
      "ⓘ",
      tags$span(
        class = "tooltip-text",
        tooltip_text
      )
    )
  )
}
