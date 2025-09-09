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
                      value = 0.8, min = 0.3, max = 2, step = 0.02)
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
#' @param param_manager Parameter manager instance (central hub)
#' @param external_updates Reactive containing parameter updates from sliders (DEPRECATED)
#'
#' @noRd 
mod_effect_sizes_server <- function(id, design_config, param_manager, analysis_config = reactive(NULL)){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    # ========================================================================
    # INPUT FEEDING - SAFE: Using isolate() to break reactive cycles
    # ========================================================================
    
    # Use observeEvent + isolate to prevent circular reactive dependencies
    observeEvent(input$minimum_fold_change_fixed, {
      isolate({
        param_manager$update_parameter("minimum_fold_change", input$minimum_fold_change_fixed, "sidebar")
      })
    })
    
    # ========================================================================
    # UI UPDATES - SAFE: Using observeEvent + isolate for controlled updates
    # ========================================================================
    
    # PHASE 3: PARAMETER MANAGER → SIDEBAR SYNC REMOVED
    # Eliminated fold change sync observer for UI independence  
    # Sidebar fold change input now works independently from sliders
    
    
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
    
    # Update fold change input range based on analysis side parameter
    observe({
      analysis_choices <- analysis_config()
      if (!is.null(analysis_choices) && !is.null(analysis_choices$side)) {
        side <- analysis_choices$side
        
        # Dynamic range based on side
        if (side == "right") {
          # Right-sided test: fold change from 1.0 to 2.0 (upregulation)
          updateNumericInput(session, "minimum_fold_change_fixed", min = 1.0, max = 2.0)
        } else {
          # Left-sided or both: fold change from 0.3 to 1.0 (downregulation)
          updateNumericInput(session, "minimum_fold_change_fixed", min = 0.3, max = 1.0)
        }
      }
    })
    
    # Return effect sizes configuration
    effect_sizes_config <- reactive({
      list(
        # Fixed value input (only default if actually hidden)  
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
