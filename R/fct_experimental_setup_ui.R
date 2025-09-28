#' Experimental Setup UI Management Functions
#'
#' @description Helper functions for managing UI state and interactions in the
#' experimental setup module. These functions have side effects (updating UI)
#' but are extracted for better organization and testability.
#'
#' @noRd
NULL

#' Update Fixed Parameter Visibility
#'
#' @description Manages the visibility of fixed parameter input sections based
#' on parameter control configuration from design options.
#'
#' @param session Shiny session object
#' @param parameter_controls List containing parameter control configuration
#'   from design options module
#'
#' @return NULL (side effects only)
#'
#' @noRd
update_fixed_parameter_visibility <- function(session, parameter_controls) {
  if (is.null(parameter_controls)) {
    # Hide all sections when no parameter controls available
    shinyjs::hide("experimental_fixed_params")
    shinyjs::hide("cells_fixed_div")
    shinyjs::hide("reads_per_cell_fixed_div")
    return()
  }

  cells_type <- parameter_controls$cells_per_target$type
  reads_type <- parameter_controls$reads_per_cell$type

  # Show cells fixed input when cells parameter is set to "fixed"
  # This includes both user-selected "fixed" and auto-determined "fixed" in power-only mode
  if (!is.null(cells_type) && cells_type == "fixed") {
    shinyjs::show("cells_fixed_div")
    shinyjs::show("experimental_fixed_params")
  } else {
    shinyjs::hide("cells_fixed_div")
  }

  # Show reads fixed input when reads parameter is set to "fixed"
  # This includes both user-selected "fixed" and auto-determined "fixed" in power-only mode
  if (!is.null(reads_type) && reads_type == "fixed") {
    shinyjs::show("reads_per_cell_fixed_div")
    shinyjs::show("experimental_fixed_params")
  } else {
    shinyjs::hide("reads_per_cell_fixed_div")
  }

  # Hide the entire section if neither parameter is fixed
  if ((is.null(cells_type) || cells_type != "fixed") &&
      (is.null(reads_type) || reads_type != "fixed")) {
    shinyjs::hide("experimental_fixed_params")
  }
}

#' Reset Pilot Data Status
#'
#' @description Resets pilot data status and UI elements to default state.
#' Centralizes the repeated 3-line reset pattern.
#'
#' @param session Shiny session object
#' @param custom_pilot_data ReactiveVal containing custom pilot data
#' @param output Shiny output object
#'
#' @return NULL (side effects only)
#'
#' @noRd
reset_pilot_data_status <- function(session, custom_pilot_data, output) {
  custom_pilot_data(NULL)
  output$pilot_data_uploaded <- reactive(FALSE)
  outputOptions(output, "pilot_data_uploaded", suspendWhenHidden = FALSE)
}

#' Reset Fixed Parameter Inputs
#'
#' @description Resets fixed parameter inputs to default values when
#' optimization mode changes.
#'
#' @param session Shiny session object
#' @param defaults List of default values from get_experimental_defaults()
#'
#' @return NULL (side effects only)
#'
#' @noRd
reset_fixed_parameter_inputs <- function(session, defaults) {
  # Reset fixed value inputs when switching modes
  updateNumericInput(session, "cells_fixed", value = defaults$cells_fixed_default)
  updateNumericInput(session, "reads_per_cell_fixed", value = defaults$reads_per_cell_fixed_default)

  # Hide all fixed parameter sections initially
  shinyjs::hide("experimental_fixed_params")
  shinyjs::hide("cells_fixed_div")
  shinyjs::hide("reads_per_cell_fixed_div")
}

#' Toggle Input States for Phase Management
#'
#' @description Enables or disables experimental setup inputs based on app phase.
#' Used for dual-workflow implementation where inputs are frozen in Phase 2.
#'
#' @param session Shiny session object
#' @param inputs_disabled Logical indicating if inputs should be disabled
#'
#' @return NULL (side effects only)
#'
#' @noRd
toggle_input_states <- function(session, inputs_disabled) {
  # Valid input IDs that exist in the UI (corrected from original bug)
  valid_input_ids <- c(
    "biological_system",
    "MOI",
    "num_targets",
    "gRNAs_per_target",
    "non_targeting_gRNAs",
    "cells_fixed",
    "reads_per_cell_fixed"
    # Note: removed "pilot_data_choice" and "pilot_data_upload" as they don't exist
  )

  # Toggle state for all valid inputs
  for (input_id in valid_input_ids) {
    shinyjs::toggleState(input_id, condition = !inputs_disabled)
  }
}

#' Handle File Upload Processing
#'
#' @description Processes uploaded pilot data file with validation and error handling.
#' Manages UI updates and status display throughout the upload process.
#'
#' @param session Shiny session object
#' @param file_info List containing file information from shiny::fileInput
#' @param biological_system Character current biological system selection
#' @param custom_pilot_data ReactiveVal for storing custom pilot data
#' @param output Shiny output object
#' @param defaults List of default values from get_experimental_defaults()
#'
#' @return NULL (side effects only)
#'
#' @noRd
handle_file_upload <- function(session, file_info, biological_system, custom_pilot_data, output, defaults) {
  # Only process if Custom biological system is selected
  if (biological_system != "Custom") {
    return()
  }

  # Validate file using extracted function
  validation <- validate_upload_file(file_info, defaults$max_file_size_mb)
  if (!validation$valid) {
    # Validation failed - reset status
    reset_pilot_data_status(session, custom_pilot_data, output)
    return()
  }

  # Process the validated file
  tryCatch({
    # Read RDS file
    uploaded_data <- readRDS(file_info$datapath)

    # Validate the pilot data structure
    validation_result <- validate_custom_pilot_data(uploaded_data, file_info$name)

    if (validation_result$valid) {
      # Store validated pilot data
      custom_pilot_data(validation_result$data)

      # Create success message using extracted function
      status_msg <- format_success_message(validation_result$summary, validation_result$warnings)

      # Update status display
      output$pilot_data_status <- renderUI({
        HTML(status_msg)
      })

      output$pilot_data_uploaded <- reactive(TRUE)
      outputOptions(output, "pilot_data_uploaded", suspendWhenHidden = FALSE)

    } else {
      # Show validation errors using extracted function
      error_msg <- paste0("Validation failed:<br/>",
                        paste(validation_result$errors, collapse = "<br/>"))

      output$pilot_data_status <- renderUI({
        HTML(format_error_message("validation", error_msg))
      })

      reset_pilot_data_status(session, custom_pilot_data, output)
    }

  }, error = function(e) {
    # Enhanced error handling using extracted functions
    error_type <- categorize_file_error(e$message)
    error_msg <- format_error_message(error_type, e$message)

    # Error notification removed
    output$pilot_data_status <- renderUI({
      HTML(error_msg)
    })
    reset_pilot_data_status(session, custom_pilot_data, output)
  })
}

#' Handle Optimization Mode Changes
#'
#' @description Manages UI state when optimization mode changes, including
#' resetting fixed parameter inputs and tracking mode transitions.
#'
#' @param session Shiny session object
#' @param config List design configuration from design options
#' @param previous_mode ReactiveVal tracking previous optimization mode
#' @param defaults List of default values
#'
#' @return NULL (side effects only)
#'
#' @noRd
handle_mode_change <- function(session, config, previous_mode, defaults) {
  if (is.null(config) || is.null(config$optimization_type)) {
    return()
  }

  current_mode <- config$optimization_type

  # If mode switched, reset all fixed parameter inputs
  if (!is.null(previous_mode()) && previous_mode() != current_mode) {
    reset_fixed_parameter_inputs(session, defaults)
  }

  # Update previous mode tracker
  previous_mode(current_mode)
}