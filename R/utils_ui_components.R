#' UI Component Library
#' 
#' @description Reusable UI components that use semantic CSS classes
#' from the Phase 1 CSS Foundation (variables.css, layout.css, components.css)
#' 
#' @name ui_components
#' @keywords internal
#' @importFrom shiny tags NS numericInput selectInput fileInput tagList
NULL

# Removed unused collapsible_section function

# Removed unused parameter_input_group function

# Removed unused currency_input function

#' Create a Parameter Control Matrix
#'
#' @description Creates a standardized parameter control matrix with 
#' radio buttons for Varying/Fixed/Minimizing states. Handles the complex
#' business logic for parameter control in constraint-driven workflows.
#'
#' @param ns Namespace function from the calling module
#' @param parameters Named list of parameters with their current states
#' @param workflow_info Current workflow information for business logic
#' @param disabled_params Vector of parameter names that should be disabled
#'
#' @return A div with parameter matrix structure
#' @export
#'
#' @examples
#' \dontrun{
#' parameter_matrix(ns, 
#'   list(cells_per_target = "varying", reads_per_cell = "fixed"),
#'   workflow_info
#' )
#' }
parameter_matrix <- function(ns, parameters, workflow_info = NULL, disabled_params = c()) {
  if (length(parameters) == 0) {
    return(tags$div())
  }
  
  tags$div(
    class = "parameter-matrix",
    lapply(names(parameters), function(param_name) {
      current_state <- parameters[[param_name]]
      is_disabled <- param_name %in% disabled_params
      
      tags$div(
        class = "parameter-row",
        # Parameter name
        tags$div(
          class = "parameter-name",
          # Use canonical display names from naming standards
          if (!is.null(PARAMETER_DISPLAY_NAMES[[param_name]])) {
            PARAMETER_DISPLAY_NAMES[[param_name]]
          } else {
            param_name
          }
        ),
        # Parameter controls
        tags$div(
          class = "parameter-controls",
          # Varying radio button
          tags$div(
            class = "radio",
            tags$label(
              tags$input(
                type = "radio",
                name = ns(paste0(param_name, "_control")),
                value = "varying",
                checked = if (current_state == "varying") "checked" else NULL,
                disabled = if (is_disabled) "disabled" else NULL
              ),
              "Varying"
            )
          ),
          # Fixed radio button  
          tags$div(
            class = "radio",
            tags$label(
              tags$input(
                type = "radio", 
                name = ns(paste0(param_name, "_control")),
                value = "fixed",
                checked = if (current_state == "fixed") "checked" else NULL,
                disabled = if (is_disabled) "disabled" else NULL
              ),
              "Fixed"
            )
          ),
          # Minimizing radio button
          tags$div(
            class = "radio",
            tags$label(
              tags$input(
                type = "radio",
                name = ns(paste0(param_name, "_control")), 
                value = "minimizing",
                checked = if (current_state == "minimizing") "checked" else NULL,
                disabled = if (is_disabled) "disabled" else NULL
              ),
              "Minimizing"
            )
          )
        )
      )
    })
  )
}

#' Create a Step Container
#'
#' @description Creates a standardized step container for multi-step forms
#' with proper styling and semantic structure.
#'
#' @param step_number Step number (e.g., 1, 2, 3)
#' @param title Step title
#' @param content Step content
#' @param show_divider Whether to show bottom divider (default: TRUE)
#'
#' @return A div with step container structure
#' @export
#'
#' @examples  
#' \dontrun{
#' step_container(1, "Optimization Constraints", 
#'   selectInput(ns("opt_type"), "Type:", choices = list(...))
#' )
#' }
step_container <- function(step_number, title, content, show_divider = TRUE) {
  tags$div(
    class = if (show_divider) "step-container" else "step-container-last",
    tags$h5(paste("Step", step_number, ":", title), class = "step-header"),
    content
  )
}

#' Create a File Upload Zone
#'
#' @description Creates a standardized file upload area with info text
#' and proper styling using semantic CSS classes.
#'
#' @param ns Namespace function from the calling module
#' @param id Input ID for file input
#' @param label File input label (optional, set to NULL for no label)
#' @param accept File types to accept
#' @param info_text Informational text about file format
#' @param placeholder Placeholder text for file input
#'
#' @return A div with file upload structure
#' @export
#'
#' @examples
#' \dontrun{
#' file_upload_zone(ns, "pilot_data", 
#'   accept = c(".rds", ".RDS"),
#'   info_text = "Combined RDS file with baseline expression",
#'   placeholder = "Choose reference expression data..."
#' )
#' }
file_upload_zone <- function(ns, id, label = NULL, accept = NULL, info_text = NULL, placeholder = NULL) {
  tags$div(
    if (!is.null(info_text)) {
      tags$div(
        class = "file-upload-info",
        tags$small(
          tags$i(class = "fa fa-info-circle"),
          tags$strong("Format: "), info_text
        )
      )
    },
    fileInput(ns(id), 
      label = label,
      accept = accept,
      placeholder = placeholder
    )
  )
}