#' results_cache UI Function
#'
#' @description Cache management module for pin-based solution storage.
#' No UI component - pure server-side cache management.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList moduleServer reactive reactiveValues observe observeEvent req showNotification
mod_results_cache_ui <- function(id) {
  ns <- NS(id)
  tagList(
    # No UI - cache is server-side only
  )
}

#' results_cache Server Functions
#'
#' @description Manages pin-based results caching system where "Current" results
#' can be pinned to become numbered solutions.
#'
#' @param analysis_results Reactive containing analysis results from analysis engine
#' @param pin_trigger Reactive trigger for pinning current result (from parameter sliders)
#' @param clear_trigger Reactive trigger for clearing all pinned solutions
#'
#' @return List with cached_results reactive and pin management functions
#' @noRd
mod_results_cache_server <- function(id, analysis_results, pin_trigger, clear_trigger) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ========================================================================
    # CACHE STATE MANAGEMENT
    # ========================================================================

    # Pin-based cache structure
    results_cache <- reactiveValues(
      current_result = NULL,        # Always "Current" (dashed line in plots)
      pinned_solutions = list(),    # Named list: "Setting 1", "Setting 2", etc.
      next_solution_id = 1         # Counter for naming new pins
    )

    # ========================================================================
    # PIN MANAGEMENT
    # ========================================================================

    # Pin current result when pin button is clicked
    observeEvent(pin_trigger(), {
      current_cached <- cached_results()
      results_cache$current_result <- current_cached$current_result
      current <- results_cache$current_result

      if (!is.null(current) && is.null(current$error)) {
        # Create solution name
        solution_name <- paste0("Setting ", results_cache$next_solution_id)

        # Move current result to pinned solutions (not copy)
        results_cache$pinned_solutions[[solution_name]] <- current

        # Clear current result since it's now pinned
        results_cache$current_result <- NULL

        # Increment counter for next pin
        results_cache$next_solution_id <- results_cache$next_solution_id + 1

        # Show confirmation
        showNotification(
          paste("Pinned as", solution_name),
          type = "message",
          duration = 3
        )
      }
    }, ignoreNULL = TRUE, ignoreInit = TRUE)

    # Clear all pinned solutions when clear button is clicked
    observeEvent(clear_trigger(), {
      if (length(results_cache$pinned_solutions) > 0) {
        results_cache$pinned_solutions <- list()
        results_cache$next_solution_id <- 1

        showNotification(
          "Cleared all pinned solutions",
          type = "message",
          duration = 3
        )
      }
    }, ignoreNULL = TRUE, ignoreInit = TRUE)

    # ========================================================================
    # COMBINED RESULTS FOR DISPLAY
    # ========================================================================

    # Combine current + pinned results for unified display
    cached_results <- reactive({

      results <- analysis_results()
      pinned <- results_cache$pinned_solutions

      # initialze when next_solution_id is 1
      if(results_cache$next_solution_id == 1 && !is.null(results)){
        list(
          current_result = results,
          pinned_solutions = list(),          # For "Solution X" displays (solid lines)
          all_results = c(
            list("Current" = results)
          )
        )
      }else if(length(pinned) == 0 && is.null(results)){
        NULL
      }else{

        # define current variable
        if(is.null(isolate(results_cache$current_result))){
          current <- NULL
          results_cache$current_result <- results
        }else{
          current <- results
        }

        # Return structure for display modules
        list(
          current_result = current,
          pinned_solutions = pinned,          # For "Solution X" displays (solid lines)
          all_results = c(
            if (!is.null(current)) list("Current" = current) else list(),
            pinned
          )
        )
      }
    })

    # ========================================================================
    # RETURN INTERFACE
    # ========================================================================

    return(cached_results)
  })
}

## To be copied in the UI
# mod_results_cache_ui("results_cache_1")

## To be copied in the server
# mod_results_cache_server("results_cache_1")
