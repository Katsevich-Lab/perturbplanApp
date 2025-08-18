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
      class = "collapsible-section",
      # Collapsible header
      tags$div(
        class = "collapsible-header",
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
                    max = 50,
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
                    step = 1),
        
        # gRNA variability
        numericInput(ns("gRNA_variability"), 
                    "gRNA variability:",
                    value = 0.13,
                    min = 0.01,
                    max = 1.0,
                    step = 0.01),
        
        # Control group type
        selectInput(ns("control_group"), 
                   "Control group:",
                   choices = c("Complement" = "complement", 
                              "Non-targeting cells" = "nt_cells"),
                   selected = "complement"),
        
        # Platform efficiency parameters
        tags$hr(style = "margin: 15px 0; border-color: #E3E6EA;"),
        tags$h6("Platform efficiency", style = "color: #6B6B6B; margin-bottom: 10px;"),
        
        numericInput(ns("mapping_efficiency"), 
                    "Mapping efficiency:",
                    value = 0.72,
                    min = 0.1,
                    max = 1.0,
                    step = 0.01),
        
        numericInput(ns("cell_recovery_rate"), 
                    "Cell recovery rate:",
                    value = 0.5,
                    min = 0.1,
                    max = 1.0,
                    step = 0.01),
        
        numericInput(ns("QC_rate"), 
                    "QC rate:",
                    value = 1.0,
                    min = 0.5,
                    max = 1.0,
                    step = 0.01)
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
    
    # Return reactive list with all perturbation parameters
    reactive({
      list(
        MOI = input$MOI %||% 10,
        num_targets = input$num_targets %||% 100,
        gRNAs_per_target = input$gRNAs_per_target %||% 4,
        non_targeting_gRNAs = input$non_targeting_gRNAs %||% 10,
        gRNA_variability = input$gRNA_variability %||% 0.13,
        control_group = input$control_group %||% "complement",
        mapping_efficiency = input$mapping_efficiency %||% 0.72,
        cell_recovery_rate = input$cell_recovery_rate %||% 0.5,
        QC_rate = input$QC_rate %||% 1.0
      )
    })
  })
}