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
        
        # Simple toggle button - no shadow, positioned outside collapse zone
        tags$div(
          id = "simple-toggle",
          style = "position: fixed; top: 50%; left: 0px; transform: translateY(-50%); 
                   background: white; border: 1px solid #ddd; border-left: none;
                   width: 24px; height: 60px; border-radius: 0 8px 8px 0;
                   cursor: pointer; z-index: 9999; display: none; 
                   align-items: center; justify-content: center;
                   font-size: 12px; color: #666;",
          "▶"
        ),
        
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
    )
    # CSS and JS assets are automatically included by bundle_resources()
    # Files: variables.css, layout.css, components.css, perturbplan_styles.css, perturbplan_interactions.js  
    # Managed by golem::add_css_file() and golem::add_js_file()
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
