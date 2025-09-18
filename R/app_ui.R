#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @importFrom shinydashboard dashboardPage dashboardHeader dashboardBody
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Add Google Analytics
    use_google_analytics("G-W43V869MN9"),

    # Dashboard page layout (matching original perturbplan app exactly)
    dashboardPage(
      header = dashboardHeader(
        title = "PerturbPlan",
        tags$li(
          class = "dropdown",
          style = "float: right; margin-right: 20px;",
          # Export buttons container (conditionally shown)
          uiOutput("header_export_buttons")
        )
      ),
      
      sidebar = mod_sidebar_ui("sidebar"),
      
      body = dashboardBody(
        # Add the exact same styles from original perturbplan app
        create_styles(),
        
        # Main content area with results display module
        tags$div(
          style = "padding: 20px;",
          
          # Results Display Module - handles both welcome state and results
          mod_results_display_ui("display")
        )
      )
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @importFrom shinyjs useShinyjs
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "perturbplanApp"
    ),
    # Initialize shinyjs for JavaScript functionality
    useShinyjs()
    # CSS and JS assets are automatically included by bundle_resources()
    # Files: variables.css, layout.css, components.css, perturbplan_styles.css, perturbplan_interactions.js
    # Managed by golem::add_css_file() and golem::add_js_file()
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
