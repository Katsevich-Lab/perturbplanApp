#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    fluidPage(
      titlePanel("PerturbPlan v2: Constraint-Driven Experimental Design"),
      
      # Main navigation tabs
      navbarPage(
        title = "",
        id = "main_nav",
        
        # Tab 1: Design Options (Core module)
        tabPanel(
          "Design Options",
          value = "design_options",
          mod_design_options_ui("design_options")
        ),
        
        # Tab 2: Experimental Setup (placeholder)
        tabPanel(
          "Experimental Setup", 
          value = "experimental_setup",
          h3("Experimental Setup"),
          p("Pilot data upload and library parameters will be implemented here."),
          p("This will be adapted from the original app, removing TPM/fold change controls.")
        ),
        
        # Tab 3: Analysis Results (placeholder) 
        tabPanel(
          "Analysis Results",
          value = "analysis_results", 
          h3("Analysis Results"),
          p("Workflow-specific visualizations will be displayed here."),
          p("Plots will be generated based on the design configuration from the Design Options tab.")
        ),
        
        # Tab 4: Export Results (placeholder)
        tabPanel(
          "Export Results",
          value = "export_results",
          h3("Export Results"), 
          p("Excel downloads and data export functionality will be implemented here.")
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
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
