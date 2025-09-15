#' perturbation_choices UI Function
#'
#' @description A shiny Module for perturbation choices configuration.
#' This module handles experimental design parameters related to the 
#' perturbation setup, including MOI, targets, and gRNA configuration.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_perturbation_choices_ui <- function(id){
  ns <- NS(id)
  tagList(
    tags$div(
      style = "border-radius: 4px; margin-bottom: 5px;",
      tags$div(
        id = ns("perturbation-header"),
        style = "padding: 10px 15px; cursor: pointer; border-radius: 4px 4px 0 0;",
        onclick = paste0("toggleSection('", ns("perturbation-content"), "', '", ns("perturbation-chevron"), "')"),
        tags$i(id = ns("perturbation-chevron"), class = "fa fa-chevron-right", style = "margin-right: 8px;"),
        tags$strong("Perturbation choices")
      ),
      tags$div(
        id = ns("perturbation-content"),
        style = "padding: 15px;",
        
        # MOI (Multiplicity of Infection)
        numericInput(ns("MOI"),
                    "Multiplicity of infection (MOI):",
                    value = 10,
                    min = 1,
                    max = 30,
                    step = 1),
        
        # Number of targets
        numericInput(ns("num_targets"), 
                    "Number of targets:",
                    value = 100,
                    min = 10,
                    max = 1000,
                    step = 10),
        
        # gRNAs per target
        numericInput(ns("gRNAs_per_target"), 
                    "gRNAs per target:",
                    value = 4,
                    min = 1,
                    max = 10,
                    step = 1),
        
        # Non-targeting gRNAs
        numericInput(ns("non_targeting_gRNAs"), 
                    "Non-targeting gRNAs:",
                    value = 10,
                    min = 0,
                    max = 100,
                    step = 1)
      )
    )
  )
}
    
#' perturbation_choices Server Functions
#'
#' @param id Module ID
#' @return Reactive list containing perturbation choices configuration
#' @noRd 
mod_perturbation_choices_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    # Return reactive list with core perturbation parameters
    reactive({
      list(
        MOI = input$MOI %||% 10,
        num_targets = input$num_targets %||% 100,
        gRNAs_per_target = input$gRNAs_per_target %||% 4,
        non_targeting_gRNAs = input$non_targeting_gRNAs %||% 10
      )
    })
  })
}