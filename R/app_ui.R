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
        title = "PerturbPlan v2",
        tags$li(
          class = "dropdown",
          style = "float: right; margin-right: 20px;",
          # Placeholder for download button (will be conditionally shown later)
          tags$div()
        )
      ),
      
      sidebar = mod_sidebar_ui("sidebar"),
      
      body = dashboardBody(
        # Add the exact same styles from original perturbplan app
        create_styles(),
        
        # Main content area - placeholder for results visualization
        tags$div(
          style = "padding: 20px;",
          h3("PerturbPlan v2: Constraint-Driven Experimental Design"),
          
          # Instruction message before planning
          tags$div(
            id = "need_plan_message",
            class = "alert alert-info",
            style = "margin: 20px 0;",
            tags$h4("Welcome to PerturbPlan v2!"),
            tags$p("Configure your experimental design parameters in the left sidebar, then click 'Plan' to begin analysis."),
            tags$ul(
              tags$li(tags$strong("Design Options:"), " Specify your optimization objective and constraints"),
              tags$li(tags$strong("Experimental Setup:"), " Choose biological system and reference data"),
              tags$li(tags$strong("Analysis Choices:"), " Configure perturbation-gene pairs and statistical parameters"),
              tags$li(tags$strong("Effect Sizes:"), " Set assumed effect size parameters")
            )
          ),
          
          # Placeholder for results (will be populated after planning)
          tags$div(
            id = "results_placeholder",
            style = "display: none;",
            h4("Analysis Results"),
            p("Results will be displayed here after clicking 'Plan' in the sidebar.")
          )
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
    ),
    # Add JavaScript for collapsible sidebar functionality
    tags$script(src = "www/perturbplan_interactions.js")
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
