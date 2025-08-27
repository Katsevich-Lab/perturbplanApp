#' Results Display Module UI Function
#'
#' @description Creates UI for displaying analysis results including
#' interactive plots, summary tables, and export functionality.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList fluidRow column h3 h4 wellPanel
#' @importFrom shiny conditionalPanel renderUI uiOutput downloadButton
#' @importFrom shinydashboard box
#' @importFrom plotly plotlyOutput
#' @importFrom DT DTOutput
mod_results_display_ui <- function(id) {
  ns <- NS(id)
  tagList(
    # Main results container
    fluidRow(
      # Main plot column (made wider for better visualization)
      column(
        width = 9,
        box(
          title = "Analysis Results",
          status = "primary",
          solidHeader = TRUE,
          width = NULL,
          height = 500,
          
          # Conditional display based on analysis state (mutually exclusive)
          conditionalPanel(
            condition = "output.show_results == false && output.show_error == false",
            ns = ns,
            wellPanel(
              style = "text-align: center; padding: 60px;",
              h3("Ready for Analysis", style = "color: #5A6B73;"),
              tags$p("Configure your experimental design in the sidebar and click 'Plan' to generate results.",
                     style = "color: #7A8B93; font-size: 16px;")
            )
          ),
          
          conditionalPanel(
            condition = "output.show_results == true && output.show_error == false",
            ns = ns,
            # Interactive plot output
            plotlyOutput(ns("main_plot"), height = "400px")
          ),
          
          # Error display panel (only when there's an actual error)
          conditionalPanel(
            condition = "output.show_error == true",
            ns = ns,
            tags$div(
              style = "background-color: #f8d7da; border: 1px solid #f5c6cb; border-radius: 5px; padding: 20px; margin: 20px 0;",
              tags$h4("Analysis Error", style = "color: #721c24; margin-top: 0;"),
              uiOutput(ns("error_message"))
            )
          )
        )
      ),
      
      # Summary and controls column (narrower to give more space to plot)
      column(
        width = 3,
        # Analysis summary box
        box(
          title = "Solution",
          status = "info", 
          solidHeader = TRUE,
          width = NULL,
          
          conditionalPanel(
            condition = "output.show_results == true",
            ns = ns,
            uiOutput(ns("analysis_summary"))
          ),
          
          conditionalPanel(
            condition = "output.show_results == false", 
            ns = ns,
            wellPanel(
              style = "text-align: center; padding: 20px;",
              tags$p("Analysis summary will appear here after running the analysis.",
                     style = "color: #7A8B93;")
            )
          )
        )
      )
    ),
    
    # Horizontal Parameter Sliders Row (placeholder for now)
    conditionalPanel(
      condition = "output.show_sliders == true",
      ns = ns,
      fluidRow(
        column(12,
          tags$div(
            class = "slider-panel",
            style = "background: white; border: 1px solid #dee2e6; border-radius: 8px; box-shadow: 0 2px 4px rgba(0,0,0,0.1);",
            mod_parameter_sliders_ui(ns("sliders"))
          )
        )
      )
    )
  )
}
    
#' Results Display Server Functions
#'
#' @description Server logic for displaying analysis results.
#' Handles plot rendering, summary generation, and export functionality.
#'
#' @param id Module namespace ID
#' @param plot_objects Reactive containing plot objects from mod_plotting_engine
#' @param analysis_results Reactive containing analysis results from mod_analysis_engine
#' 
#' @noRd 
#' 
#' @importFrom shiny moduleServer reactive observe req renderUI
#' @importFrom shiny showNotification downloadHandler renderPlot observeEvent
#' @importFrom plotly renderPlotly
# DT import removed - detailed results table no longer used
#' @importFrom openxlsx write.xlsx
#' @importFrom ggplot2 ggsave ggplot annotate theme_void
mod_results_display_server <- function(id, plot_objects, analysis_results, user_config = reactive(NULL)) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # ========================================================================
    # REACTIVE DISPLAY STATE
    # ========================================================================
    
    
    # Determine if results should be shown
    output$show_results <- reactive({
      # Use tryCatch to handle any errors in plot_objects() or analysis_results()
      tryCatch({
        plots <- plot_objects()
        results <- analysis_results()
        
        !is.null(plots) && !is.null(results) && 
          is.null(plots$error) && is.null(results$error)
      }, error = function(e) {
        # If there's an error accessing plots or results, don't show results
        FALSE
      })
    })
    outputOptions(output, "show_results", suspendWhenHidden = FALSE)
    
    # Determine if errors should be shown
    output$show_error <- reactive({
      tryCatch({
        plots <- plot_objects()
        results <- analysis_results()
        
        # Only show error if we have actual data with errors, not when data is missing
        has_plot_error <- !is.null(plots) && !is.null(plots$error)
        has_result_error <- !is.null(results) && !is.null(results$error)
        
        has_plot_error || has_result_error
      }, error = function(e) {
        # If there's an error accessing plots or results, don't show error state
        # This happens on app startup when no analysis has been run yet
        FALSE
      })
    })
    outputOptions(output, "show_error", suspendWhenHidden = FALSE)
    
    # Workflow-based slider visibility for 8 target workflows
    output$show_sliders <- reactive({
      tryCatch({
        # Use design configuration instead of analysis results for immediate visibility
        config <- user_config()
        
        if (is.null(config) || is.null(config$design_options)) {
          return(FALSE)
        }
        
        # Use the new lightweight detection function
        workflow_detection <- detect_slider_workflow(config$design_options)
        
        return(workflow_detection$should_show_sliders)
        
      }, error = function(e) {
        FALSE
      })
    })
    outputOptions(output, "show_sliders", suspendWhenHidden = FALSE)
    
    # Initialize parameter sliders module
    # Extract sidebar config and workflow info from analysis results
    sidebar_config <- reactive({
      # Use direct user config for immediate slider functionality (primary source)
      config <- user_config()
      if (!is.null(config)) {
        return(config)
      }
      
      # Fallback: get from analysis results (if user_config not available for some reason)
      results <- analysis_results()
      if (!is.null(results) && !is.null(results$user_config)) {
        return(results$user_config)
      }
      
      return(NULL)
    })
    
    workflow_info <- reactive({
      # Primary: generate workflow info from design config for immediate slider visibility
      config <- user_config()
      if (!is.null(config) && !is.null(config$design_options)) {
        workflow_detection <- detect_slider_workflow(config$design_options)
        if (!is.null(workflow_detection$workflow_id)) {
          return(list(workflow_id = workflow_detection$workflow_id))
        }
      }
      
      # Fallback: try to get from analysis results (if design config not available)
      results <- analysis_results()
      if (!is.null(results) && !is.null(results$workflow_info)) {
        return(results$workflow_info)
      }
      
      return(NULL)
    })
    
    # Initialize slider module server and handle updates
    slider_updates <- mod_parameter_sliders_server("sliders", sidebar_config, workflow_info)
    
    # Error message display
    output$error_message <- renderUI({
      error_msg <- NULL
      
      tryCatch({
        plots <- plot_objects()
        results <- analysis_results()
        
        # Check for plotting errors first
        if (!is.null(plots) && !is.null(plots$error)) {
          error_msg <- paste("Plotting Error:", plots$error)
        }
        # Check for analysis errors if no plotting error
        else if (!is.null(results) && !is.null(results$error)) {
          error_msg <- results$error
        }
      }, error = function(e) {
        error_msg <- paste("Display Error:", e$message)
      })
      
      if (!is.null(error_msg)) {
        tags$div(
          style = "color: #721c24; line-height: 1.5;",
          tags$p(error_msg),
          tags$p("Please check your input parameters and try again.", 
                 style = "margin-top: 10px; font-style: italic;")
        )
      }
    })
    
    # ========================================================================
    # MAIN PLOT RENDERING
    # ========================================================================
    
    output$main_plot <- renderPlotly({
      tryCatch({
        req(plot_objects())
        
        plots <- plot_objects()
        
        if (!is.null(plots$error)) {
          return(NULL)
        }
        
        # Return the interactive plot - handle different data structures
        if (!is.null(plots)) {
        }
        
        # Check for direct interactive_plot (cost minimization)
        if (!is.null(plots$interactive_plot)) {
          plots$interactive_plot
        }
        # Check for nested plots$interactive_plot (other workflows) 
        else if (!is.null(plots$plots) && !is.null(plots$plots$interactive_plot)) {
          plots$plots$interactive_plot
        }
        else {
          NULL
        }
      }, error = function(e) {
        # Return empty plot if there's an error
        return(NULL)
      })
    })
    
    # ========================================================================
    # ANALYSIS SUMMARY
    # ========================================================================
    
    output$analysis_summary <- renderUI({
      req(analysis_results(), plot_objects())
      
      results <- analysis_results()
      plots <- plot_objects()
      
      if (!is.null(results$error) || !is.null(plots$error)) {
        return(tags$div(
          style = "color: #C73E1D; padding: 10px;",
          h4("Analysis Error"),
          tags$p(results$error %||% plots$error)
        ))
      }
      
      # Generate summary based on plot type
      summary_data <- if (plots$plot_type == "single_parameter_curve") {
        plots$plots$summary_stats
      } else {
        plots$plots$cost_summary
      }
      
      workflow_info <- results$workflow_info
      
      render_solution_section(results, plots)
    })
    
    # Detailed results table removed - not useful for end users
    
    
    
    # ========================================================================
    # EXPORT FUNCTIONALITY
    # ========================================================================
    
    # Excel export using downloadHandler
    output$export_excel <- downloadHandler(
      filename = function() {
        paste0("perturbplan_analysis_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".xlsx")
      },
      content = function(file) {
        req(analysis_results(), plot_objects())
        
        results <- analysis_results()
        plots <- plot_objects()
        
        tryCatch({
          # Prepare Excel data
          excel_data <- list(
            "Summary" = create_excel_summary(results, plots),
            "Detailed_Results" = results$power_data,
            "Design_Options" = create_excel_design_options(results$user_config$design_options),
            "Experimental_Setup" = create_excel_experimental_setup(results$user_config$experimental_setup),
            "Analysis_Choices" = create_excel_analysis_choices(results$user_config$analysis_choices),
            "Effect_Sizes" = create_excel_effect_sizes(results$user_config$effect_sizes),
            "Metadata" = data.frame(
              Item = c("Analysis Mode", "Workflow Type", "Timestamp", "App Version"),
              Value = c(
                results$metadata$analysis_mode,
                results$workflow_info$workflow_id,
                as.character(results$metadata$analysis_timestamp),
                results$metadata$app_version
              )
            )
          )
          
          # Add cost information sheet if cost optimization is used
          if (!is.null(results$user_config$cost_info)) {
            excel_data[["Cost_Information"]] <- create_excel_cost_info(results$user_config$cost_info)
          }
          
          # Write Excel file to the specified path
          write.xlsx(excel_data, file = file)
          
        }, error = function(e) {
          showNotification(
            paste("Export failed:", e$message),
            type = "error",
            duration = 5
          )
          stop(e$message)
        })
      },
      contentType = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
    )
    
    # Plot download using downloadHandler
    output$export_plot <- downloadHandler(
      filename = function() {
        paste0("perturbplan_plot_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")
      },
      content = function(file) {
        req(plot_objects())
        
        plots <- plot_objects()
        
        tryCatch({
          # Get the static ggplot (not the interactive plotly version)
          if (!is.null(plots$plots$main_plot)) {
            # Save the ggplot as PNG with high resolution
            ggsave(
              filename = file,
              plot = plots$plots$main_plot,
              width = 12,
              height = 8,
              dpi = 300,
              units = "in",
              device = "png"
            )
          } else {
            stop("No plot available for download")
          }
          
        }, error = function(e) {
          showNotification(
            paste("Plot download failed:", e$message),
            type = "error",
            duration = 5
          )
          # Create a minimal error plot if main plot fails
          error_plot <- ggplot2::ggplot() + 
            ggplot2::annotate("text", x = 0.5, y = 0.5, 
                            label = "Plot generation failed", 
                            size = 6) +
            ggplot2::theme_void()
          
          ggplot2::ggsave(filename = file, plot = error_plot, 
                         width = 8, height = 6, dpi = 150, device = "png")
        })
      },
      contentType = "image/png"
    )
    
    # Return slider updates for main app integration
    return(list(
      slider_updates = slider_updates
    ))
  })
}


# ============================================================================
# SOLUTION DISPLAY HELPER FUNCTIONS
# ============================================================================

#' Render solution section with optimal parameters
#'
#' @param results Analysis results object
#' @param plots Plot objects
#' @return Shiny UI tagList
#' @noRd
render_solution_section <- function(results, plots) {
  if (is.null(results$optimal_design) || is.null(results$workflow_info)) {
    return(render_no_solution_fallback())
  }
  
  optimal <- results$optimal_design
  workflow_info <- results$workflow_info
  minimizing_param <- workflow_info$minimizing_parameter
  
  tagList(
    # SOLUTION SECTION
    tags$div(
      style = "background-color: #E8F4FD; padding: 18px; border-radius: 5px; margin-bottom: 15px; border-left: 4px solid #2E86AB;",
      
      # Solution header
      tags$div(
        tags$strong("Solution", style = "color: #2E4A62; font-size: 14px; margin-bottom: 12px; display: block;")
      ),
      
      # Show minimizing parameter based on workflow type
      render_minimizing_parameter_display(optimal, workflow_info, minimizing_param),
      
      # Show varying parameter for power+cost workflows
      render_varying_parameter_display(optimal, workflow_info),
      
      # Power achieved (for non-unified workflows)
      render_power_achieved_display(optimal, workflow_info)
    ),
    
    # FIXED PARAMETERS SECTION
    render_fixed_parameters_section(optimal, plots, minimizing_param, workflow_info)
  )
}

#' Render minimizing parameter display based on workflow type
#'
#' @param optimal Optimal design results
#' @param workflow_info Workflow information
#' @param minimizing_param Minimizing parameter name
#' @return Shiny UI tagList
#' @noRd
render_minimizing_parameter_display <- function(optimal, workflow_info, minimizing_param) {
  # Check if this is workflows 10-11 (unified constrained minimization)
  if (workflow_info$workflow_id %in% c("power_cost_TPM_cells_reads", "power_cost_fc_cells_reads")) {
    # Need to pass the full results object, not just optimal_design and workflow_info
    # We need to reconstruct the full results object from the parent context
    return(render_minimization_solution_display(optimal, workflow_info))
  }
  
  # Handle different minimizing parameters
  if (minimizing_param == "TPM_threshold") {
    return(render_tpm_minimization_display(optimal))
  } else if (minimizing_param == "minimum_fold_change") {
    return(render_fc_minimization_display(optimal))
  } else if (minimizing_param == "cells_per_target") {
    return(render_cells_minimization_display(optimal))
  } else if (minimizing_param %in% c("reads_per_cell", "mapped_reads_per_cell")) {
    return(render_reads_minimization_display(optimal))
  } else if (minimizing_param == "cost") {
    return(render_cost_minimization_display(optimal))
  }
  
  return(NULL)
}

#' Render minimization solution display (for unified workflows 10-11)
#'
#' @param optimal Optimal design results
#' @param workflow_info Workflow information
#' @return Shiny UI tagList
#' @noRd
render_minimization_solution_display <- function(optimal, workflow_info) {
  minimizing_param <- workflow_info$minimizing_parameter
  
  # For unified workflows, show the minimizing parameter and supporting parameters
  if (minimizing_param == "TPM_threshold") {
    tagList(
      create_parameter_display("Optimal TPM threshold: ", optimal$TPM_threshold, 0, "18px"),
      create_cost_display(optimal$total_cost),
      # If both cells and reads are varying (no specific varying_parameter), show them here
      if (is.null(workflow_info$varying_parameter)) {
        tagList(
          create_parameter_display("Optimal cells per target: ", optimal$cells_per_target, 0, "16px"),
          create_parameter_display("Optimal sequenced reads per cell: ", optimal$sequenced_reads_per_cell, 0, "16px")
        )
      },
      create_power_achieved_display(optimal$achieved_power)
    )
  } else if (minimizing_param == "minimum_fold_change") {
    tagList(
      create_parameter_display("Optimal fold change: ", optimal$minimum_fold_change, 2, "18px"),
      create_cost_display(optimal$total_cost),
      # If both cells and reads are varying (no specific varying_parameter), show them here
      if (is.null(workflow_info$varying_parameter)) {
        tagList(
          create_parameter_display("Optimal cells per target: ", optimal$cells_per_target, 0, "16px"),
          create_parameter_display("Optimal sequenced reads per cell: ", optimal$sequenced_reads_per_cell, 0, "16px")
        )
      },
      create_power_achieved_display(optimal$achieved_power)
    )
  } else {
    # Fallback for any other minimizing parameters
    tagList(
      create_cost_display(optimal$total_cost),
      create_power_achieved_display(optimal$achieved_power)
    )
  }
}

#' Render TPM minimization display
#'
#' @param optimal Optimal design results
#' @return Shiny UI tagList
#' @noRd
render_tpm_minimization_display <- function(optimal) {
  if (is.null(optimal$TPM_threshold) || is.na(optimal$TPM_threshold)) {
    return(NULL)
  }
  
  tagList(
    create_parameter_display("Optimal TPM threshold: ", optimal$TPM_threshold, 0, "18px"),
    create_cost_display(optimal$total_cost)
  )
}

#' Render fold change minimization display
#'
#' @param optimal Optimal design results
#' @return Shiny UI tagList
#' @noRd
render_fc_minimization_display <- function(optimal) {
  if (is.null(optimal$minimum_fold_change) || is.na(optimal$minimum_fold_change)) {
    return(NULL)
  }
  
  tagList(
    create_parameter_display("Optimal fold change: ", optimal$minimum_fold_change, 2, "18px"),
    create_cost_display(optimal$total_cost)
  )
}

#' Render cells minimization display
#'
#' @param optimal Optimal design results
#' @return Shiny UI tagList
#' @noRd
render_cells_minimization_display <- function(optimal) {
  if (is.null(optimal$cells_per_target) || is.na(optimal$cells_per_target)) {
    return(NULL)
  }
  
  create_parameter_display("Optimal cells per target: ", optimal$cells_per_target, 0, "18px")
}

#' Render reads minimization display
#'
#' @param optimal Optimal design results
#' @return Shiny UI tagList
#' @noRd
render_reads_minimization_display <- function(optimal) {
  if (is.null(optimal$sequenced_reads_per_cell) || is.na(optimal$sequenced_reads_per_cell)) {
    return(NULL)
  }
  
  create_parameter_display("Optimal sequenced reads per cell: ", optimal$sequenced_reads_per_cell, 0, "18px")
}

#' Render cost minimization display
#'
#' @param optimal Optimal design results
#' @return Shiny UI tagList
#' @noRd
render_cost_minimization_display <- function(optimal) {
  if (is.null(optimal$total_cost) || is.na(optimal$total_cost)) {
    return(NULL)
  }
  
  tagList(
    tags$div(
      style = "margin-bottom: 8px;",
      tags$span("Minimum total cost: ", style = "color: #5A6B73; font-weight: 500;"),
      tags$span(paste0("$", round(optimal$total_cost)), style = "color: #2E86AB; font-weight: bold; font-size: 18px;")
    ),
    create_parameter_display("Optimal cells per target: ", optimal$cells_per_target, 0, "16px"),
    create_parameter_display("Optimal sequenced reads per cell: ", optimal$sequenced_reads_per_cell, 0, "16px")
  )
}

#' Render varying parameter display for power+cost workflows
#'
#' @param optimal Optimal design results
#' @param workflow_info Workflow information
#' @return Shiny UI tagList or NULL
#' @noRd
render_varying_parameter_display <- function(optimal, workflow_info) {
  if (is.null(workflow_info$category) || workflow_info$category != "power_cost_single" || 
      is.null(workflow_info$varying_parameter)) {
    return(NULL)
  }
  
  varying_param <- workflow_info$varying_parameter
  
  if (varying_param == "cells" && !is.null(optimal$cells_per_target) && !is.na(optimal$cells_per_target)) {
    return(create_parameter_display("Cost-constrained cells per target: ", optimal$cells_per_target, 0, "18px"))
  } else if (varying_param == "reads" && !is.null(optimal$sequenced_reads_per_cell) && !is.na(optimal$sequenced_reads_per_cell)) {
    return(create_parameter_display("Cost-constrained sequenced reads per cell: ", optimal$sequenced_reads_per_cell, 0, "18px"))
  }
  
  return(NULL)
}

#' Render power achieved display
#'
#' @param optimal Optimal design results
#' @param workflow_info Workflow information
#' @return Shiny UI tags$div or NULL
#' @noRd
render_power_achieved_display <- function(optimal, workflow_info) {
  if (is.null(optimal$achieved_power) || is.na(optimal$achieved_power) || 
      workflow_info$workflow_id %in% c("power_cost_TPM_cells_reads", "power_cost_fc_cells_reads")) {
    return(NULL)
  }
  
  tags$div(
    style = "margin-top: 12px;",
    tags$span("Power achieved: ", style = "color: #5A6B73; font-weight: 500;"),
    tags$span(paste0(round(optimal$achieved_power * 100, 1), "%"), style = "color: #28A745; font-weight: bold; font-size: 16px;")
  )
}

#' Create a standardized parameter display
#'
#' @param label Parameter label
#' @param value Parameter value
#' @param decimals Number of decimal places
#' @param font_size Font size for the value
#' @return Shiny UI tags$div
#' @noRd
create_parameter_display <- function(label, value, decimals = 0, font_size = "16px") {
  if (is.null(value) || is.na(value)) {
    return(NULL)
  }
  
  formatted_value <- if (is.numeric(value)) {
    round(value, decimals)
  } else {
    "N/A"
  }
  
  tags$div(
    style = "margin-bottom: 8px;",
    tags$span(label, style = "color: #5A6B73; font-weight: 500;"),
    tags$span(formatted_value, style = paste0("color: #2E86AB; font-weight: bold; font-size: ", font_size, ";"))
  )
}

#' Create a cost display
#'
#' @param cost_value Cost value
#' @return Shiny UI tags$div or NULL
#' @noRd
create_cost_display <- function(cost_value) {
  if (is.null(cost_value) || is.na(cost_value)) {
    return(NULL)
  }
  
  tags$div(
    style = "margin-bottom: 8px;",
    tags$span("Total cost: ", style = "color: #5A6B73; font-weight: 500;"),
    tags$span(
      paste0("$", scales::comma(round(cost_value))), 
      style = "color: #2E86AB; font-weight: bold; font-size: 16px;"
    )
  )
}

#' Create a power achieved display
#'
#' @param power_value Power achieved value
#' @return Shiny UI tags$div or NULL
#' @noRd
create_power_achieved_display <- function(power_value) {
  if (is.null(power_value) || is.na(power_value)) {
    return(NULL)
  }
  
  tags$div(
    style = "margin-bottom: 8px;",
    tags$span("Achieved power: ", style = "color: #5A6B73; font-weight: 500;"),
    tags$span(
      paste0(round(power_value * 100, 1), "%"), 
      style = "color: #28A745; font-weight: bold; font-size: 16px;"
    )
  )
}

#' Render fixed parameters section
#'
#' @param optimal Optimal design results
#' @param plots Plot objects
#' @param minimizing_param Minimizing parameter name
#' @return Shiny UI tags$div
#' @noRd
render_fixed_parameters_section <- function(optimal, plots, minimizing_param, workflow_info = NULL) {
  tags$div(
    style = "background-color: #F8F9FA; padding: 18px; border-radius: 5px; margin-bottom: 15px;",
    
    # Fixed Parameters header
    tags$div(
      tags$strong("Fixed Parameters", style = "color: #2E4A62; font-size: 14px; margin-bottom: 12px; display: block;")
    ),
    
    # Show non-minimizing parameters
    tagList(
      render_fixed_tpm_display(optimal, plots, minimizing_param),
      render_fixed_fc_display(optimal, plots, minimizing_param),
      render_fixed_cells_display(optimal, minimizing_param, workflow_info),
      render_fixed_reads_display(optimal, minimizing_param, workflow_info),
      render_mapping_efficiency_display(optimal)
    )
  )
}

#' Render fixed TPM display
#'
#' @param optimal Optimal design results
#' @param plots Plot objects
#' @param minimizing_param Minimizing parameter name
#' @return Shiny UI tags$div or NULL
#' @noRd
render_fixed_tpm_display <- function(optimal, plots, minimizing_param) {
  if (minimizing_param == "TPM_threshold") {
    return(NULL)
  }
  
  TPM_value <- if (!is.null(optimal$TPM_threshold) && !is.na(optimal$TPM_threshold)) {
    optimal$TPM_threshold
  } else if (!is.null(plots$plots$plot_data) && "TPM_threshold" %in% names(plots$plots$plot_data)) {
    unique(plots$plots$plot_data$TPM_threshold)[1]
  } else {
    10  # Default TPM threshold commonly used
  }
  
  if (!is.null(TPM_value) && !is.na(TPM_value)) {
    tags$div(
      style = "margin-bottom: 6px;",
      tags$span("TPM analysis threshold: ", style = "color: #5A6B73; font-weight: 400; font-size: 13px;"),
      tags$span(round(TPM_value), style = "color: #6C757D; font-weight: 500;")
    )
  }
}

#' Render fixed fold change display
#'
#' @param optimal Optimal design results
#' @param plots Plot objects
#' @param minimizing_param Minimizing parameter name
#' @return Shiny UI tags$div or NULL
#' @noRd
render_fixed_fc_display <- function(optimal, plots, minimizing_param) {
  if (minimizing_param == "minimum_fold_change") {
    return(NULL)
  }
  
  fc_value <- if (!is.null(optimal$minimum_fold_change) && !is.na(optimal$minimum_fold_change)) {
    optimal$minimum_fold_change
  } else {
    if (!is.null(plots$plots$plot_data) && "minimum_fold_change" %in% names(plots$plots$plot_data)) {
      unique(plots$plots$plot_data$minimum_fold_change)[1]
    } else {
      0.5  # Default fold change commonly used for downregulation
    }
  }
  
  if (!is.null(fc_value) && !is.na(fc_value)) {
    tags$div(
      style = "margin-bottom: 6px;",
      tags$span("Fold change: ", style = "color: #5A6B73; font-weight: 400; font-size: 13px;"),
      tags$span(round(fc_value, 2), style = "color: #6C757D; font-weight: 500;")
    )
  }
}

#' Render fixed cells display
#'
#' @param optimal Optimal design results
#' @param minimizing_param Minimizing parameter name
#' @return Shiny UI tags$div or NULL
#' @noRd
render_fixed_cells_display <- function(optimal, minimizing_param, workflow_info = NULL) {
  excluded_params <- c("cells_per_target", "cost")
  
  # For workflows 10-11 and constrained minimization, handle different varying scenarios
  if (!is.null(workflow_info) && 
      workflow_info$workflow_id %in% c("power_cost_TPM_cells_reads", "power_cost_fc_cells_reads", "power_cost_TPM_cells", "power_cost_TPM_reads", "power_cost_fc_cells", "power_cost_fc_reads")) {
    # If both cells and reads are varying (no specific varying_parameter), don't show in Fixed Parameters
    if (is.null(workflow_info$varying_parameter)) {
      return(NULL)  # Both varying - show in main Solution section
    }
    # If cells is varying (cost-constrained), don't show it here
    if (workflow_info$varying_parameter == "cells") {
      return(NULL)
    }
    # Otherwise, cells is fixed, so show it in Fixed Parameters section
  } else {
    # For workflows where both cells and reads are varying (like cost minimization),
    # don't show them in Fixed Parameters - they should be in the main Solution section
    if (!is.null(workflow_info) && workflow_info$category == "cost_only") {
      return(NULL)
    }
    # For power+cost workflows, exclude TPM_threshold and minimum_fold_change 
    # But for power-only workflows, we want to show cells/reads even when TPM/FC is being minimized
    if (!is.null(workflow_info) && workflow_info$category != "power_only_single") {
      excluded_params <- c(excluded_params, "TPM_threshold", "minimum_fold_change")
    }
  }
  
  if (minimizing_param %in% excluded_params || 
      is.null(optimal$cells_per_target) || is.na(optimal$cells_per_target)) {
    return(NULL)
  }
  
  tags$div(
    style = "margin-bottom: 6px;",
    tags$span("Cells per target: ", style = "color: #5A6B73; font-weight: 400; font-size: 13px;"),
    tags$span(round(optimal$cells_per_target), style = "color: #6C757D; font-weight: 500;")
  )
}

#' Render fixed reads display
#'
#' @param optimal Optimal design results
#' @param minimizing_param Minimizing parameter name
#' @return Shiny UI tags$div or NULL
#' @noRd
render_fixed_reads_display <- function(optimal, minimizing_param, workflow_info = NULL) {
  excluded_params <- c("reads_per_cell", "mapped_reads_per_cell", "cost")
  
  # For workflows 10-11 and constrained minimization, handle different varying scenarios
  if (!is.null(workflow_info) && 
      workflow_info$workflow_id %in% c("power_cost_TPM_cells_reads", "power_cost_fc_cells_reads", "power_cost_TPM_cells", "power_cost_TPM_reads", "power_cost_fc_cells", "power_cost_fc_reads")) {
    # If both cells and reads are varying (no specific varying_parameter), don't show in Fixed Parameters
    if (is.null(workflow_info$varying_parameter)) {
      return(NULL)  # Both varying - show in main Solution section
    }
    # If reads is varying (cost-constrained), don't show it here
    if (workflow_info$varying_parameter == "reads") {
      return(NULL)
    }
    # Otherwise, reads is fixed, so show it in Fixed Parameters section
  } else {
    # For workflows where both cells and reads are varying (like cost minimization),
    # don't show them in Fixed Parameters - they should be in the main Solution section
    if (!is.null(workflow_info) && workflow_info$category == "cost_only") {
      return(NULL)
    }
    # For power+cost workflows, exclude TPM_threshold and minimum_fold_change 
    # But for power-only workflows, we want to show cells/reads even when TPM/FC is being minimized
    if (!is.null(workflow_info) && workflow_info$category != "power_only_single") {
      excluded_params <- c(excluded_params, "TPM_threshold", "minimum_fold_change")
    }
  }
  
  if (minimizing_param %in% excluded_params || 
      is.null(optimal$sequenced_reads_per_cell) || is.na(optimal$sequenced_reads_per_cell)) {
    return(NULL)
  }
  
  tags$div(
    style = "margin-bottom: 6px;",
    tags$span("Sequenced reads per cell: ", style = "color: #5A6B73; font-weight: 400; font-size: 13px;"),
    tags$span(
      if (is.numeric(optimal$sequenced_reads_per_cell)) round(optimal$sequenced_reads_per_cell) else "N/A",
      style = "color: #6C757D; font-weight: 500;"
    )
  )
}

#' Render mapping efficiency display
#'
#' @param optimal Optimal design results
#' @return Shiny UI tags$div
#' @noRd
render_mapping_efficiency_display <- function(optimal) {
  mapping_eff <- if (!is.null(optimal$mapping_efficiency) && !is.na(optimal$mapping_efficiency)) {
    optimal$mapping_efficiency
  } else {
    0.72  # Default mapping efficiency commonly used
  }
  
  tags$div(
    style = "margin-bottom: 6px;",
    tags$span("Mapping efficiency: ", style = "color: #5A6B73; font-weight: 400; font-size: 13px;"),
    tags$span(paste0(round(mapping_eff * 100, 1), "%"), style = "color: #6C757D; font-weight: 500;")
  )
}

#' Render fallback when no solution is available
#'
#' @return Shiny UI tags$div
#' @noRd
render_no_solution_fallback <- function() {
  tags$div(
    style = "background-color: #F8F9FA; padding: 18px; border-radius: 5px; text-align: center;",
    tags$span("Solution information not available", style = "color: #999; font-style: italic;")
  )
}


# ============================================================================
# UTILITY FUNCTIONS FOR EXPORT
# ============================================================================

#' Create Excel summary sheet
#'
#' @param results Analysis results
#' @param plots Plot objects
#' @return Data frame for Excel export
#' @noRd
create_excel_summary <- function(results, plots) {
  workflow_info <- results$workflow_info
  
  if (plots$plot_type == "single_parameter_curve") {
    summary_data <- plots$plots$summary_stats
    optimal <- summary_data$optimal_recommendation
    
    data.frame(
      Metric = c(
        "Workflow Type",
        "Description", 
        "Total Designs Evaluated",
        "Feasible Designs",
        "Success Rate (%)",
        "Optimal Parameter",
        "Optimal Value",
        "Achieved Power",
        "Recommendation"
      ),
      Value = c(
        workflow_info$workflow_id,
        workflow_info$description,
        summary_data$total_designs_evaluated,
        summary_data$feasible_designs,
        round(summary_data$feasibility_rate * 100, 1),
        optimal$parameter %||% "None",
        optimal$optimal_value %||% "N/A",
        if (!is.null(optimal$achieved_power)) paste0(round(optimal$achieved_power * 100, 1), "%") else "N/A",
        optimal$recommendation_text
      )
    )
  } else {
    summary_data <- plots$plots$cost_summary
    optimal <- summary_data$optimal_recommendation
    
    data.frame(
      Metric = c(
        "Workflow Type",
        "Description",
        "Total Combinations Evaluated", 
        "Power Feasible Designs",
        "Budget Feasible Designs",
        "Optimal Cells",
        "Optimal Reads",
        "Total Cost",
        "Achieved Power",
        "Recommendation"
      ),
      Value = c(
        workflow_info$workflow_id,
        workflow_info$description,
        summary_data$total_designs_evaluated,
        summary_data$power_feasible_designs,
        summary_data$budget_feasible_designs,
        optimal$optimal_cells %||% "N/A",
        optimal$optimal_reads %||% "N/A",
        if (!is.null(optimal$total_cost)) paste0("$", scales::comma(optimal$total_cost)) else "N/A",
        if (!is.null(optimal$achieved_power)) paste0(round(optimal$achieved_power * 100, 1), "%") else "N/A",
        optimal$recommendation_text
      )
    )
  }
}

#' Create Excel Design Options sheet
#'
#' @param design_options Design options configuration
#' @return Data frame for Excel export
#' @noRd
create_excel_design_options <- function(design_options) {
  data.frame(
    Setting = c(
      "Optimization Type",
      "Target Power",
      "Cost Budget",
      "Minimization Target",
      "Parameter Controls"
    ),
    Value = c(
      design_options$optimization_type,
      paste0(design_options$target_power * 100, "%"),
      if (!is.null(design_options$cost_budget)) paste0("$", scales::comma(design_options$cost_budget)) else "None",
      design_options$minimization_target,
      paste(names(design_options$parameter_controls), collapse = ", ")
    )
  )
}

#' Create Excel Experimental Setup sheet
#'
#' @param experimental_setup Experimental setup configuration
#' @return Data frame for Excel export
#' @noRd
create_excel_experimental_setup <- function(experimental_setup) {
  if (is.null(experimental_setup)) {
    return(data.frame(Setting = "No experimental setup data", Value = "Not configured"))
  }
  
  data.frame(
    Setting = c(
      "Pilot Data Source",
      "Baseline Expression File",
      "Library Parameters File",
      "Additional Notes"
    ),
    Value = c(
      experimental_setup$pilot_data_source %||% "Not specified",
      experimental_setup$baseline_expression_file %||% "Not uploaded",
      experimental_setup$library_params_file %||% "Not uploaded", 
      experimental_setup$notes %||% "None"
    )
  )
}

#' Create Excel Analysis Choices sheet
#'
#' @param analysis_choices Analysis choices configuration
#' @return Data frame for Excel export
#' @noRd
create_excel_analysis_choices <- function(analysis_choices) {
  if (is.null(analysis_choices)) {
    return(data.frame(Setting = "No analysis choices data", Value = "Not configured"))
  }
  
  data.frame(
    Setting = c(
      "Statistical Test",
      "Multiple Testing Correction",
      "Significance Level",
      "Analysis Method"
    ),
    Value = c(
      analysis_choices$statistical_test %||% "Default",
      analysis_choices$multiple_testing %||% "Default",
      analysis_choices$significance_level %||% "0.05",
      analysis_choices$analysis_method %||% "Standard"
    )
  )
}

#' Create Excel Effect Sizes sheet
#'
#' @param effect_sizes Effect sizes configuration
#' @return Data frame for Excel export
#' @noRd
create_excel_effect_sizes <- function(effect_sizes) {
  if (is.null(effect_sizes)) {
    return(data.frame(Setting = "No effect sizes data", Value = "Not configured"))
  }
  
  data.frame(
    Setting = c(
      "Expected Effect Size",
      "Minimum Detectable Effect",
      "Effect Size Distribution",
      "Variance Assumptions"
    ),
    Value = c(
      effect_sizes$expected_effect %||% "Not specified",
      effect_sizes$min_detectable_effect %||% "Not specified",
      effect_sizes$effect_distribution %||% "Default",
      effect_sizes$variance_model %||% "Standard"
    )
  )
}

#' Create Excel Cost Information sheet
#'
#' @param cost_info Cost information configuration
#' @return Data frame for Excel export
#' @noRd
create_excel_cost_info <- function(cost_info) {
  if (is.null(cost_info)) {
    return(data.frame(Setting = "No cost information data", Value = "Not configured"))
  }
  
  data.frame(
    Setting = c(
      "Cost per Cell",
      "Cost per Million Reads",
      "Fixed Costs",
      "Budget Constraints",
      "Cost Model"
    ),
    Value = c(
      if (!is.null(cost_info$cost_per_cell)) paste0("$", cost_info$cost_per_cell) else "Default",
      if (!is.null(cost_info$cost_per_million_reads)) paste0("$", cost_info$cost_per_million_reads) else "Default",
      cost_info$fixed_costs %||% "None specified",
      cost_info$budget_constraints %||% "None specified",
      cost_info$cost_model %||% "Standard"
    )
  )
}
