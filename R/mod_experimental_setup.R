#' experimental_setup UI Function
#'
#' @description Creates the Experimental Setup section for biological system
#' selection and reference data uploads
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList tags div strong selectInput fileInput conditionalPanel numericInput moduleServer reactive observe observeEvent req reactiveVal showNotification renderUI outputOptions htmlOutput HTML updateSelectInput updateNumericInput
#' @importFrom shinyjs show hide
#' @importFrom tools file_ext
mod_experimental_setup_ui <- function(id) {
  ns <- NS(id)
  
  # Experimental setup (collapsible) - ADAPTED FROM ORIGINAL
  tagList(
    tags$div(
      style = "border-radius: 4px; margin-bottom: 5px;",
      tags$div(
        id = ns("exp-header"),
        style = "padding: 10px 15px; cursor: pointer; border-radius: 4px 4px 0 0;",
        onclick = paste0("toggleSection('", ns("experimental-content"), "', '", ns("exp-chevron"), "')"),
        tags$i(id = ns("exp-chevron"), class = "fa fa-chevron-right", style = "margin-right: 8px;"),
        tags$strong("Experimental choices")
      ),
      tags$div(
        id = ns("experimental-content"),
        style = "padding: 15px;",
        selectInput(ns("biological_system"), "Biological system:", 
                   choices = list("K562" = "K562", 
                                "A549" = "A549", 
                                "THP-1" = "THP-1", 
                                "T CD8" = "T_CD8", 
                                "iPSC" = "iPSC", 
                                "iPSC neuron" = "iPSC_neuron", 
                                "Other" = "Other"),
                   selected = "K562"),
        selectInput(ns("pilot_data_choice"), "Reference expression data:",
                   choices = c("Built-in" = "default", "Custom" = "custom"),
                   selected = "default"),
        conditionalPanel(
          condition = paste0("input['", ns("pilot_data_choice"), "'] != 'default'"),
          tags$div(
            class = "file-upload-info",
            style = "border-radius: 3px; padding: 6px; margin: 5px 0;",
            tags$small(
              tags$i(class = "fa fa-info-circle", style = "margin-right: 3px;"),
              tags$strong("Format: "), "Combined RDS file with baseline expression and library parameters",
              style = "font-size: 11px;"
            )
          ),
          fileInput(ns("pilot_data_file"), 
                   label = NULL,
                   accept = c(".rds", ".RDS"),
                   placeholder = "Choose reference expression data RDS file..."),
          
          # Upload status display (conditional)
          conditionalPanel(
            condition = "output.pilot_data_uploaded",
            ns = ns,
            tags$div(
              class = "file-upload-success",
              style = "border-radius: 4px; padding: 8px; margin: 10px 0 15px 0; background-color: #d4edda; border: 1px solid #c3e6cb; color: #155724;",
              tags$i(class = "fa fa-check-circle", style = "margin-right: 5px; color: #28a745;"),
              htmlOutput(ns("pilot_data_status"), inline = TRUE)
            )
          )
        ),
        
        # Perturbation choices section (integrated from mod_perturbation_choices)
        tags$div(
          style = "margin-top: 20px; padding-top: 15px; border-top: 1px solid #E3E6EA;",
          tags$h5("Perturbation Setup", style = "color: #2E4A62; margin-bottom: 15px;"),
          
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
                      min = 50,
                      max = 50000,
                      step = 50),
          
          # gRNAs per target
          numericInput(ns("gRNAs_per_target"), 
                      "gRNAs per target:",
                      value = 4,
                      min = 1,
                      max = 20,
                      step = 1),
          
          # Non-targeting gRNAs
          numericInput(ns("non_targeting_gRNAs"), 
                      "Non-targeting gRNAs:",
                      value = 10,
                      min = 0,
                      max = 100,
                      step = 1)
        ),
        
        # Fixed value inputs for experimental parameters (conditional)
        tags$div(
          id = ns("experimental_fixed_params"),
          style = "margin-top: 20px; padding-top: 15px; border-top: 1px solid #E3E6EA; display: none;",
          
          # Cells per target fixed value (conditional)
          tags$div(
            id = ns("cells_fixed_div"),
            style = "display: none; margin-bottom: 15px;",
            numericInput(ns("cells_fixed"), "Cells per target:", 
                        value = 1000, min = 20, max = 10000, step = 20)
          ),
          
          # Reads per cell fixed value (conditional)
          tags$div(
            id = ns("mapped_reads_fixed_div"),
            style = "display: none; margin-bottom: 15px;",
            numericInput(ns("mapped_reads_fixed"), "Mapped reads per cell:", 
                        value = 5000, min = 1000, max = 500000, step = 1000)
          )
        )
      )
    )
  )
}
    
#' experimental_setup Server Functions
#'
#' @description Server logic for experimental setup parameters and file uploads
#'
#' @param design_config Reactive containing design options configuration
#' @param param_manager Parameter manager instance (central hub)
#' @param external_updates Reactive containing parameter updates from sliders (DEPRECATED)
#'
#' @noRd 
mod_experimental_setup_server <- function(id, design_config, param_manager){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    # ========================================================================
    # INPUT FEEDING - Sidebar → Central Manager
    # ========================================================================
    
    # Feed all sidebar parameter changes to central manager
    observe({
      if (!is.null(input$MOI)) {
        param_manager$update_parameter("MOI", input$MOI, "sidebar")
      }
    })
    
    observe({
      if (!is.null(input$num_targets)) {
        param_manager$update_parameter("num_targets", input$num_targets, "sidebar")
      }
    })
    
    observe({
      if (!is.null(input$gRNAs_per_target)) {
        param_manager$update_parameter("gRNAs_per_target", input$gRNAs_per_target, "sidebar")
      }
    })
    
    observe({
      if (!is.null(input$cells_fixed)) {
        param_manager$update_parameter("cells_per_target", input$cells_fixed, "sidebar")
      }
    })
    
    observe({
      if (!is.null(input$mapped_reads_fixed)) {
        param_manager$update_parameter("reads_per_cell", input$mapped_reads_fixed, "sidebar")
      }
    })
    
    # ========================================================================
    # UI UPDATES - Central Manager → Sidebar (Direct Reactive Observers)
    # ========================================================================
    
    # Direct reactive observers to update sidebar when parameter manager changes
    observe({
      if (!identical(input$MOI, param_manager$parameters$MOI)) {
        updateNumericInput(session, "MOI", value = param_manager$parameters$MOI)
      }
    })
    
    observe({
      if (!identical(input$num_targets, param_manager$parameters$num_targets)) {
        updateNumericInput(session, "num_targets", value = param_manager$parameters$num_targets)
      }
    })
    
    observe({
      if (!identical(input$gRNAs_per_target, param_manager$parameters$gRNAs_per_target)) {
        updateNumericInput(session, "gRNAs_per_target", value = param_manager$parameters$gRNAs_per_target)
      }
    })
    
    observe({
      if (!identical(input$cells_fixed, param_manager$parameters$cells_per_target)) {
        updateNumericInput(session, "cells_fixed", value = param_manager$parameters$cells_per_target)
      }
    })
    
    observe({
      if (!identical(input$mapped_reads_fixed, param_manager$parameters$reads_per_cell)) {
        updateNumericInput(session, "mapped_reads_fixed", value = param_manager$parameters$reads_per_cell)
      }
    })
    
    
    # Logic for "Other" biological system selection
    observeEvent(input$biological_system, {
      if (input$biological_system == "Other") {
        # When "Other" is selected, automatically set to custom data
        updateSelectInput(session, "pilot_data_choice", selected = "custom")
      }
    })
    
    # Logic for custom pilot data choice
    observeEvent(input$pilot_data_choice, {
      if (input$pilot_data_choice == "custom") {
        # When custom is selected, automatically set biological system to "Other" if it's not already
        if (input$biological_system != "Other") {
          updateSelectInput(session, "biological_system", selected = "Other")
        }
      } else {
        # When built-in is selected, revert from "Other" to a default system if needed
        if (input$biological_system == "Other") {
          updateSelectInput(session, "biological_system", selected = "K562")
        }
      }
    })
    
    # Track previous optimization type for mode switching
    previous_mode <- reactiveVal(NULL)
    
    # Conditional display logic for fixed value inputs
    observe({
      config <- design_config()
      
      # Check if optimization mode has changed - if so, refresh UI state
      if (!is.null(config) && !is.null(config$optimization_type)) {
        current_mode <- config$optimization_type
        
        # If mode switched, reset all fixed parameter inputs
        if (!is.null(previous_mode()) && previous_mode() != current_mode) {
          # Reset fixed value inputs when switching modes
          updateNumericInput(session, "cells_fixed", value = 1000)
          updateNumericInput(session, "mapped_reads_fixed", value = 5000)
          
          # Hide all fixed parameter sections initially
          shinyjs::hide("experimental_fixed_params")
          shinyjs::hide("cells_fixed_div")
          shinyjs::hide("mapped_reads_fixed_div")
        }
        
        # Update previous mode tracker
        previous_mode(current_mode)
      }
      
      if (!is.null(config) && !is.null(config$parameter_controls)) {
        cells_type <- config$parameter_controls$cells_per_target$type
        reads_type <- config$parameter_controls$mapped_reads_per_cell$type
        
        # Show cells fixed input when cells parameter is set to "fixed" 
        # This includes both user-selected "fixed" and auto-determined "fixed" in power-only mode
        if (!is.null(cells_type) && cells_type == "fixed") {
          shinyjs::show("cells_fixed_div")
          shinyjs::show("experimental_fixed_params")
        } else {
          shinyjs::hide("cells_fixed_div")
        }
        
        # Show reads fixed input when reads parameter is set to "fixed"
        # This includes both user-selected "fixed" and auto-determined "fixed" in power-only mode  
        if (!is.null(reads_type) && reads_type == "fixed") {
          shinyjs::show("mapped_reads_fixed_div")
          shinyjs::show("experimental_fixed_params")
        } else {
          shinyjs::hide("mapped_reads_fixed_div")
        }
        
        # Hide the entire section if neither parameter is fixed
        if ((is.null(cells_type) || cells_type != "fixed") && 
            (is.null(reads_type) || reads_type != "fixed")) {
          shinyjs::hide("experimental_fixed_params")
        }
      } else {
        shinyjs::hide("experimental_fixed_params")
        shinyjs::hide("cells_fixed_div")
        shinyjs::hide("mapped_reads_fixed_div")
      }
    })
    
    # Custom pilot data reactive value
    custom_pilot_data <- reactiveVal(NULL)
    
    # File upload processing with validation
    observeEvent(input$pilot_data_file, {
      req(input$pilot_data_file)
      req(input$pilot_data_choice == "custom")
      
      # Check file size (limit to 50MB for RDS files)
      file_size_mb <- file.size(input$pilot_data_file$datapath) / (1024^2)
      if (file_size_mb > 50) {
        showNotification(
          paste("File size (", round(file_size_mb, 1), "MB) exceeds the 50MB limit. Please use a smaller dataset."),
          type = "error", duration = 10
        )
        custom_pilot_data(NULL)
        output$pilot_data_uploaded <- reactive(FALSE)
        outputOptions(output, "pilot_data_uploaded", suspendWhenHidden = FALSE)
        return()
      }
      
      tryCatch({
        # Read the uploaded RDS file
        file_ext <- tolower(tools::file_ext(input$pilot_data_file$name))
        
        if (file_ext == "rds") {
          # Read RDS file
          uploaded_data <- readRDS(input$pilot_data_file$datapath)
          
          # Validate the pilot data structure
          validation_result <- validate_custom_pilot_data(uploaded_data, input$pilot_data_file$name)
          
          if (validation_result$valid) {
            # Store validated pilot data
            custom_pilot_data(validation_result$data)
            
            # Create success message with summary and warnings
            status_msg <- validation_result$summary
            if (length(validation_result$warnings) > 0) {
              warning_msg <- paste0("<br/><em style='color:orange;'>", 
                                  paste(validation_result$warnings, collapse = "<br/>"), 
                                  "</em>")
              status_msg <- paste0(status_msg, warning_msg)
            }
            
            # Update status display
            output$pilot_data_status <- renderUI({
              HTML(status_msg)
            })
            
            output$pilot_data_uploaded <- reactive(TRUE)
            outputOptions(output, "pilot_data_uploaded", suspendWhenHidden = FALSE)
            
          } else {
            # Show validation errors
            error_msg <- paste0("Validation failed:<br/>", 
                              paste(validation_result$errors, collapse = "<br/>"))
            
            output$pilot_data_status <- renderUI({
              HTML(paste0("<em style='color:red;'>", error_msg, "</em>"))
            })
            
            custom_pilot_data(NULL)
            output$pilot_data_uploaded <- reactive(FALSE)
            outputOptions(output, "pilot_data_uploaded", suspendWhenHidden = FALSE)
          }
          
        } else {
          showNotification("Please upload an RDS file with the required pilot data structure", type = "error")
          custom_pilot_data(NULL)
          output$pilot_data_uploaded <- reactive(FALSE)
          outputOptions(output, "pilot_data_uploaded", suspendWhenHidden = FALSE)
        }
        
      }, error = function(e) {
        # Enhanced error handling for different types of errors
        error_msg <- if (grepl("cannot open the connection", e$message, ignore.case = TRUE)) {
          "Cannot read the uploaded file. Please ensure it's a valid RDS file."
        } else if (grepl("magic number", e$message, ignore.case = TRUE) || grepl("format", e$message, ignore.case = TRUE)) {
          "File appears to be corrupted or not a valid RDS file. Please check the file format."
        } else if (grepl("version", e$message, ignore.case = TRUE)) {
          "RDS file was created with a newer version of R. Please re-save the file with your current R version."
        } else {
          paste("Error reading pilot data file:", e$message)
        }
        
        showNotification(error_msg, type = "error", duration = 10)
        output$pilot_data_status <- renderUI({
          HTML(paste0("<em style='color:red;'>", error_msg, "</em>"))
        })
        custom_pilot_data(NULL)
        output$pilot_data_uploaded <- reactive(FALSE)
        outputOptions(output, "pilot_data_uploaded", suspendWhenHidden = FALSE)
      })
    })
    
    # Reset pilot data when choice changes to default or file is removed
    observe({
      should_reset <- is.null(input$pilot_data_file) || input$pilot_data_choice == "default"
      
      if (should_reset) {
        custom_pilot_data(NULL)
        output$pilot_data_uploaded <- reactive(FALSE)
        outputOptions(output, "pilot_data_uploaded", suspendWhenHidden = FALSE)
      }
    })
    
    # Pilot data reactive - updated to include custom data handling
    pilot_data <- reactive({
      if (input$pilot_data_choice == "custom" && !is.null(input$pilot_data_file)) {
        list(
          type = "custom",
          file_path = input$pilot_data_file$datapath,
          file_name = input$pilot_data_file$name,
          data = custom_pilot_data()  # Include validated data
        )
      } else {
        list(
          type = "default",
          biological_system = input$biological_system
        )
      }
    })
    
    # Return experimental setup configuration (now includes perturbation choices)
    experimental_config <- reactive({
      list(
        biological_system = input$biological_system,
        pilot_data_choice = input$pilot_data_choice,
        pilot_data = pilot_data(),
        # Fixed value inputs (only provide defaults if inputs are actually hidden/NULL)
        cells_fixed = input$cells_fixed,
        mapped_reads_fixed = input$mapped_reads_fixed,
        # Perturbation choices (integrated from mod_perturbation_choices)
        MOI = input$MOI %||% 10,
        num_targets = input$num_targets %||% 100,
        gRNAs_per_target = input$gRNAs_per_target %||% 4,
        non_targeting_gRNAs = input$non_targeting_gRNAs %||% 10,
        timestamp = Sys.time()
      )
    })
    
    return(experimental_config)
  })
}
    
## To be copied in the UI
# mod_experimental_setup_ui("experimental_setup_1")
    
## To be copied in the server
# mod_experimental_setup_server("experimental_setup_1")
