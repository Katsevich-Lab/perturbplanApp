#' effect_sizes UI Function
#'
#' @description Creates the Effect Sizes section for assumed effect size parameters
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList tags div strong numericInput moduleServer reactive observe updateNumericInput
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
        
        # Fixed value input for fold change (conditional)
        tags$div(
          id = ns("minimum_fold_change_fixed_div"),
          style = "display: none; margin-bottom: 15px; padding-bottom: 15px; border-bottom: 1px solid #E3E6EA;",
          numericInput(ns("minimum_fold_change_fixed"), "Fold change:", 
                      value = 0.8, min = 0.5, max = 10, step = 0.1)
        ),
        
        # Proportion of non-null pairs (moved back from advanced settings)
        numericInput(ns("prop_non_null"), "Proportion of non-null pairs:", 0.1, 0, 1, 0.01),
        
      )
    )
  )
}
    
#' effect_sizes Server Functions
#'
#' @description Server logic for effect size parameters
#'
#' @param design_config Reactive containing design options configuration
#' @param external_updates Reactive containing parameter updates from sliders
#'
#' @noRd 
mod_effect_sizes_server <- function(id, design_config, external_updates = reactive(NULL)){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    # Update fold change from slider
    observe({
      updates <- external_updates()
      
      if (!is.null(updates) && !is.null(updates$effect_sizes) && 
          !is.null(updates$effect_sizes$minimum_fold_change_fixed) && 
          !is.na(updates$effect_sizes$minimum_fold_change_fixed)) {
        updateNumericInput(session, "minimum_fold_change_fixed", 
                          value = updates$effect_sizes$minimum_fold_change_fixed)
      }
    })
    
    # Conditional display logic for fold change input (panel is now always visible)
    observe({
      config <- design_config()
      
      if (!is.null(config) && !is.null(config$parameter_controls)) {
        fc_type <- config$parameter_controls$minimum_fold_change$type
        
        # Show FC fixed input only when FC parameter is set to "fixed"
        if (!is.null(fc_type) && fc_type == "fixed") {
          shinyjs::show("minimum_fold_change_fixed_div")
        } else {
          shinyjs::hide("minimum_fold_change_fixed_div")
        }
      } else {
        shinyjs::hide("minimum_fold_change_fixed_div")
      }
    })
    
    # Return effect sizes configuration
    effect_sizes_config <- reactive({
      list(
        # Fixed value input
        minimum_fold_change_fixed = input$minimum_fold_change_fixed,
        # Proportion of non-null pairs
        prop_non_null = input$prop_non_null,
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
