#' effect_sizes UI Function
#'
#' @description Creates the Effect Sizes section for assumed effect size parameters
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList tags div strong numericInput moduleServer reactive observe
#' @importFrom shinyjs show hide
mod_effect_sizes_ui <- function(id) {
  ns <- NS(id)
  
  # Assumed effect sizes (collapsible) - FROM ORIGINAL
  tagList(
    tags$div(
      style = "border-radius: 4px; margin-bottom: 5px;",
      tags$div(
        id = ns("effects-header"),
        style = "padding: 10px 15px; cursor: pointer; border-radius: 4px 4px 0 0;",
        onclick = paste0("toggleSection('", ns("effects-content"), "', '", ns("effects-chevron"), "')"),
        tags$i(id = ns("effects-chevron"), class = "fa fa-chevron-right", style = "margin-right: 8px;"),
        tags$strong("Effect sizes")
      ),
      tags$div(
        id = ns("effects-content"),
        style = "padding: 15px; display: none;",
        
        # Fixed value input for fold change (always show since panel only appears when needed)
        numericInput(ns("minimum_fold_change_fixed"), "Fold change:", 
                    value = 0.8, min = 0.5, max = 10, step = 0.1),
        
      )
    )
  )
}
    
#' effect_sizes Server Functions
#'
#' @description Server logic for effect size parameters
#'
#' @param design_config Reactive containing design options configuration
#'
#' @noRd 
mod_effect_sizes_server <- function(id, design_config){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    # Note: No conditional logic needed here since entire panel is conditionally displayed by sidebar
    
    # Return effect sizes configuration
    effect_sizes_config <- reactive({
      list(
        # Fixed value input
        minimum_fold_change_fixed = input$minimum_fold_change_fixed,
        timestamp = Sys.time()
      )
    })
    
    return(effect_sizes_config)
  })
}
    
## To be copied in the UI
# mod_effect_sizes_ui("effect_sizes_1")
    
## To be copied in the server
# mod_effect_sizes_server("effect_sizes_1")
