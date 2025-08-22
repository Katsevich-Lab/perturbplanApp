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
      # Main plot column
      column(
        width = 8,
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
      
      # Summary and controls column
      column(
        width = 4,
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
        ),
        
        # Export box
        box(
          title = "Export",
          status = "warning",
          solidHeader = TRUE, 
          width = NULL,
          
          conditionalPanel(
            condition = "output.show_results == true",
            ns = ns,
            # Export buttons
            tags$div(
              style = "margin-bottom: 15px;",
              downloadButton(
                ns("export_excel"),
                "Export to Excel",
                icon = icon("file-excel"),
                class = "btn-success",
                style = "width: 100%; margin-bottom: 8px;"
              ),
              downloadButton(
                ns("export_plot"),
                "Download Plot", 
                icon = icon("image"),
                class = "btn-info",
                style = "width: 100%;"
              )
            ),
            
          ),
          
          conditionalPanel(
            condition = "output.show_results == false",
            ns = ns,
            wellPanel(
              style = "text-align: center; padding: 20px;",
              tags$p("Export options will be available after analysis.",
                     style = "color: #7A8B93;")
            )
          )
        )
      )
    ),
    
    # Detailed results row (expandable)
    conditionalPanel(
      condition = "output.show_results == true",
      ns = ns,
      fluidRow(
        column(
          width = 12,
          box(
            title = "Detailed Results",
            status = "primary",
            solidHeader = TRUE,
            collapsible = TRUE,
            collapsed = TRUE,
            width = NULL,
            
            # Detailed results table
            h4("Parameter Analysis Details"),
            DTOutput(ns("detailed_table")),
            
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
#' @importFrom DT renderDT datatable
#' @importFrom openxlsx write.xlsx
#' @importFrom ggplot2 ggsave ggplot annotate theme_void
mod_results_display_server <- function(id, plot_objects, analysis_results) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # ========================================================================
    # REACTIVE DISPLAY STATE
    # ========================================================================
    
    # Cache analysis results locally to avoid multiple calls
    cached_analysis_results <- reactive({
      analysis_results()
    })
    
    # Cache plot objects locally to avoid multiple calls  
    cached_plot_objects <- reactive({
      plot_objects()
    })
    
    # Determine if results should be shown
    output$show_results <- reactive({
      # Use tryCatch to handle any errors in cached_plot_objects() or cached_analysis_results()
      tryCatch({
        plots <- cached_plot_objects()
        results <- cached_analysis_results()
        
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
        plots <- cached_plot_objects()
        results <- cached_analysis_results()
        
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
    
    # Error message display
    output$error_message <- renderUI({
      error_msg <- NULL
      
      tryCatch({
        plots <- cached_plot_objects()
        results <- cached_analysis_results()
        
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
        req(cached_plot_objects())
        
        plots <- cached_plot_objects()
        
        if (!is.null(plots$error)) {
          return(NULL)
        }
        
        # Return the interactive plot - handle different data structures
        # cat("=== RENDERING MAIN PLOT ===\n")
        # cat("Plots structure available:", !is.null(plots), "\n")
        if (!is.null(plots)) {
        # cat("Plot object keys:", paste(names(plots), collapse = ", "), "\n")
        }
        
        # Check for direct interactive_plot (cost minimization)
        if (!is.null(plots$interactive_plot)) {
        # cat("Using direct interactive_plot\n")
          plots$interactive_plot
        }
        # Check for nested plots$interactive_plot (other workflows) 
        else if (!is.null(plots$plots) && !is.null(plots$plots$interactive_plot)) {
        # cat("Using nested plots$interactive_plot\n")
          plots$plots$interactive_plot
        }
        else {
        # cat("No interactive plot found in either location\n")
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
      req(cached_analysis_results(), cached_plot_objects())
      
      results <- cached_analysis_results()
      plots <- cached_plot_objects()
      
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
      
      tagList(
        if (!is.null(results$optimal_design) && !is.null(results$workflow_info)) {
          optimal <- results$optimal_design
          workflow_info <- results$workflow_info
          minimizing_param <- workflow_info$minimizing_parameter
          
          tagList(
            # SOLUTION SECTION - Show only the minimizing variable and power achieved
            tags$div(
              style = "background-color: #E8F4FD; padding: 18px; border-radius: 5px; margin-bottom: 15px; border-left: 4px solid #2E86AB;",
              
              # Solution header
              tags$div(
                tags$strong("Solution", style = "color: #2E4A62; font-size: 14px; margin-bottom: 12px; display: block;")
              ),
              
              # Show minimizing parameter and varying parameter (for power+cost workflows)
              tagList(
                # Check if this is workflows 10-11 (unified constrained minimization)
                if (workflow_info$workflow_id %in% c("power_cost_TPM_cells_reads", "power_cost_fc_cells_reads")) {
                  # Use unified minimization solution display
                  render_minimization_solution(results)
                } else if (minimizing_param == "TPM_threshold" && !is.null(optimal$TPM_threshold) && !is.na(optimal$TPM_threshold)) {
                  # Legacy TPM minimization workflow display (for other TPM workflows)
                  tagList(
                    tags$div(
                      style = "margin-bottom: 8px;",
                      tags$span("Optimal TPM threshold: ", style = "color: #5A6B73; font-weight: 500;"),
                      tags$span(
                        if (!is.null(optimal$TPM_threshold) && is.numeric(optimal$TPM_threshold)) round(optimal$TPM_threshold, 1) else "N/A",
                        style = "color: #2E86AB; font-weight: bold; font-size: 18px;"
                      )
                    ),
                    tags$div(
                      style = "margin-bottom: 8px;",
                      tags$span("Optimal cells per target: ", style = "color: #5A6B73; font-weight: 500;"),
                      tags$span(
                        if (!is.null(optimal$cells_per_target) && is.numeric(optimal$cells_per_target)) round(optimal$cells_per_target) else "N/A",
                        style = "color: #2E86AB; font-weight: bold; font-size: 16px;"
                      )
                    ),
                    tags$div(
                      style = "margin-bottom: 8px;",
                      tags$span("Optimal reads per cell: ", style = "color: #5A6B73; font-weight: 500;"),
                      tags$span(
                        if (!is.null(optimal$reads_per_cell) && is.numeric(optimal$reads_per_cell)) round(optimal$reads_per_cell) else "N/A",
                        style = "color: #2E86AB; font-weight: bold; font-size: 16px;"
                      )
                    ),
                    tags$div(
                      style = "margin-bottom: 8px;",
                      tags$span("Total cost: ", style = "color: #5A6B73; font-weight: 500;"),
                      tags$span(
                        if (!is.null(optimal$total_cost) && is.numeric(optimal$total_cost)) paste0("$", scales::comma(round(optimal$total_cost))) else "N/A",
                        style = "color: #2E86AB; font-weight: bold; font-size: 16px;"
                      )
                    )
                  )
                } else if (minimizing_param == "minimum_fold_change" && !is.null(optimal$minimum_fold_change) && !is.na(optimal$minimum_fold_change)) {
                  # Legacy FC minimization workflow display (for other FC workflows)
                  tagList(
                    tags$div(
                      style = "margin-bottom: 8px;",
                      tags$span("Optimal fold change: ", style = "color: #5A6B73; font-weight: 500;"),
                      tags$span(
                        if (!is.null(optimal$minimum_fold_change) && is.numeric(optimal$minimum_fold_change)) round(optimal$minimum_fold_change, 2) else "N/A",
                        style = "color: #2E86AB; font-weight: bold; font-size: 18px;"
                      )
                    ),
                    tags$div(
                      style = "margin-bottom: 8px;",
                      tags$span("Optimal cells per target: ", style = "color: #5A6B73; font-weight: 500;"),
                      tags$span(
                        if (!is.null(optimal$cells_per_target) && is.numeric(optimal$cells_per_target)) round(optimal$cells_per_target) else "N/A",
                        style = "color: #2E86AB; font-weight: bold; font-size: 16px;"
                      )
                    ),
                    tags$div(
                      style = "margin-bottom: 8px;",
                      tags$span("Optimal reads per cell: ", style = "color: #5A6B73; font-weight: 500;"),
                      tags$span(
                        if (!is.null(optimal$reads_per_cell) && is.numeric(optimal$reads_per_cell)) round(optimal$reads_per_cell) else "N/A",
                        style = "color: #2E86AB; font-weight: bold; font-size: 16px;"
                      )
                    ),
                    tags$div(
                      style = "margin-bottom: 8px;",
                      tags$span("Total cost: ", style = "color: #5A6B73; font-weight: 500;"),
                      tags$span(
                        if (!is.null(optimal$total_cost) && is.numeric(optimal$total_cost)) paste0("$", scales::comma(round(optimal$total_cost))) else "N/A",
                        style = "color: #2E86AB; font-weight: bold; font-size: 16px;"
                      )
                    )
                  )
                } else if (minimizing_param == "cells_per_target" && !is.null(optimal$cells_per_target) && !is.na(optimal$cells_per_target)) {
                  tags$div(
                    style = "margin-bottom: 8px;",
                    tags$span("Optimal cells per target: ", style = "color: #5A6B73; font-weight: 500;"),
                    tags$span(round(optimal$cells_per_target), style = "color: #2E86AB; font-weight: bold; font-size: 18px;")
                  )
                } else if (minimizing_param == "reads_per_cell" && !is.null(optimal$reads_per_cell) && !is.na(optimal$reads_per_cell)) {
                  tags$div(
                    style = "margin-bottom: 8px;",
                    tags$span("Optimal raw reads per cell: ", style = "color: #5A6B73; font-weight: 500;"),
                    tags$span(round(optimal$reads_per_cell), style = "color: #2E86AB; font-weight: bold; font-size: 18px;")
                  )
                } else if (minimizing_param == "cost" && !is.null(optimal$total_cost) && !is.na(optimal$total_cost)) {
                  # Cost minimization workflow
                  tagList(
                    tags$div(
                      style = "margin-bottom: 8px;",
                      tags$span("Minimum total cost: ", style = "color: #5A6B73; font-weight: 500;"),
                      tags$span(paste0("$", round(optimal$total_cost)), style = "color: #2E86AB; font-weight: bold; font-size: 18px;")
                    ),
                    tags$div(
                      style = "margin-bottom: 8px;",
                      tags$span("Optimal cells per target: ", style = "color: #5A6B73; font-weight: 500;"),
                      tags$span(round(optimal$cells_per_target), style = "color: #2E86AB; font-weight: bold; font-size: 16px;")
                    ),
                    tags$div(
                      style = "margin-bottom: 8px;",
                      tags$span("Optimal reads per cell: ", style = "color: #5A6B73; font-weight: 500;"),
                      tags$span(round(optimal$reads_per_cell), style = "color: #2E86AB; font-weight: bold; font-size: 16px;")
                    )
                  )
                },
                
                # For power+cost workflows, also show the varying parameter from cost calculation
                if (!is.null(workflow_info$category) && workflow_info$category == "power_cost_single" && 
                    !is.null(workflow_info$varying_parameter)) {
                  varying_param <- workflow_info$varying_parameter
                  if (varying_param == "cells" && !is.null(optimal$cells_per_target) && !is.na(optimal$cells_per_target)) {
                    tags$div(
                      style = "margin-bottom: 8px;",
                      tags$span("Cost-constrained cells per target: ", style = "color: #5A6B73; font-weight: 500;"),
                      tags$span(round(optimal$cells_per_target), style = "color: #2E86AB; font-weight: bold; font-size: 18px;")
                    )
                  } else if (varying_param == "reads" && !is.null(optimal$reads_per_cell) && !is.na(optimal$reads_per_cell)) {
                    tags$div(
                      style = "margin-bottom: 8px;",
                      tags$span("Cost-constrained raw reads per cell: ", style = "color: #5A6B73; font-weight: 500;"),
                      tags$span(round(optimal$reads_per_cell), style = "color: #2E86AB; font-weight: bold; font-size: 18px;")
                    )
                  }
                },
                
                # Power achieved (only for non-unified workflows)
                if (!is.null(optimal$achieved_power) && !is.na(optimal$achieved_power) && 
                    !workflow_info$workflow_id %in% c("power_cost_TPM_cells_reads", "power_cost_fc_cells_reads")) {
                  tags$div(
                    style = "margin-top: 12px;",
                    tags$span("Power achieved: ", style = "color: #5A6B73; font-weight: 500;"),
                    tags$span(paste0(round(optimal$achieved_power * 100, 1), "%"), style = "color: #28A745; font-weight: bold; font-size: 16px;")
                  )
                }
              )
            ),
            
            # FIXED PARAMETERS SECTION - Show all non-minimizing parameters + mapping efficiency
            tags$div(
              style = "background-color: #F8F9FA; padding: 18px; border-radius: 5px; margin-bottom: 15px;",
              
              # Fixed Parameters header
              tags$div(
                tags$strong("Fixed Parameters", style = "color: #2E4A62; font-size: 14px; margin-bottom: 12px; display: block;")
              ),
              
              # Show non-minimizing parameters
              tagList(
                # TPM threshold (for cost minimization, get from config or power_data)
                if (minimizing_param != "TPM_threshold") {
                  TPM_value <- if (!is.null(optimal$TPM_threshold) && !is.na(optimal$TPM_threshold)) {
                    optimal$TPM_threshold
                  } else if (!is.null(plots$plots$plot_data) && "TPM_threshold" %in% names(plots$plots$plot_data)) {
                    # For cost minimization, get from nested plot data
                    unique(plots$plots$plot_data$TPM_threshold)[1]
                  } else {
                    10  # Default TPM threshold commonly used
                  }
                  if (!is.null(TPM_value) && !is.na(TPM_value)) {
                    tags$div(
                      style = "margin-bottom: 6px;",
                      tags$span("TPM analysis threshold: ", style = "color: #5A6B73; font-weight: 400; font-size: 13px;"),
                      tags$span(round(TPM_value, 1), style = "color: #6C757D; font-weight: 500;")
                    )
                  }
                },
                
                # Fold change (for cost minimization, get from config or power_data)
                if (minimizing_param != "minimum_fold_change") {
                  fc_value <- if (!is.null(optimal$minimum_fold_change) && !is.na(optimal$minimum_fold_change)) {
        # cat("DEBUG: Using fold change from optimal design:", optimal$minimum_fold_change, "\n")
                    optimal$minimum_fold_change
                  } else {
                    # For cost minimization, get from plot data or use common fixed value
                    if (!is.null(plots$plots$plot_data) && "minimum_fold_change" %in% names(plots$plots$plot_data)) {
                      fc_from_data <- unique(plots$plots$plot_data$minimum_fold_change)[1]
        # cat("DEBUG: Using fold change from nested plot data:", fc_from_data, "\n")
                      fc_from_data
                    } else {
        # cat("DEBUG: Using default fold change (nested plot_data not available)\n")
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
                },
                
                # Cells per target (if not minimizing AND not cost/TPM/FC minimization AND not varying in power+cost)
                if (minimizing_param != "cells_per_target" && minimizing_param != "cost" && 
                    minimizing_param != "TPM_threshold" && minimizing_param != "minimum_fold_change" &&
                    !is.null(optimal$cells_per_target) && !is.na(optimal$cells_per_target) &&
                    !(workflow_info$category == "power_cost_single" && workflow_info$varying_parameter == "cells")) {
                  tags$div(
                    style = "margin-bottom: 6px;",
                    tags$span("Cells per target: ", style = "color: #5A6B73; font-weight: 400; font-size: 13px;"),
                    tags$span(round(optimal$cells_per_target), style = "color: #6C757D; font-weight: 500;")
                  )
                },
                
                # Raw reads per cell (if not minimizing AND not cost/TPM/FC minimization AND not varying in power+cost)
                if (minimizing_param != "reads_per_cell" && minimizing_param != "cost" && 
                    minimizing_param != "TPM_threshold" && minimizing_param != "minimum_fold_change" &&
                    !is.null(optimal$reads_per_cell) && !is.na(optimal$reads_per_cell) &&
                    !(workflow_info$category == "power_cost_single" && workflow_info$varying_parameter == "reads")) {
                  tags$div(
                    style = "margin-bottom: 6px;",
                    tags$span("Raw reads per cell: ", style = "color: #5A6B73; font-weight: 400; font-size: 13px;"),
                    tags$span(round(optimal$reads_per_cell), style = "color: #6C757D; font-weight: 500;")
                  )
                },
                
                # Mapping efficiency (always in fixed parameters, use default if not available)
                {
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
              )
            )
          )
        } else {
          # Fallback if data not available
          tags$div(
            style = "background-color: #F8F9FA; padding: 18px; border-radius: 5px; text-align: center;",
            tags$span("Solution information not available", style = "color: #999; font-style: italic;")
          )
        }
      )
    })
    
    # ========================================================================
    # DETAILED RESULTS TABLE
    # ========================================================================
    
    output$detailed_table <- renderDT({
      req(cached_analysis_results())
      
      results <- cached_analysis_results()
      
      if (!is.null(results$error)) {
        return(NULL)
      }
      
      # Prepare data for table - handle different workflow structures
      if (results$workflow_info$plot_type == "single_parameter_curve") {
        
        # Check if this is a power+cost single parameter workflow
        if (results$workflow_info$category == "power_cost_single") {
          # Power+cost single parameter: has parameter_name, parameter_value, power, meets_threshold, plus cells/reads/cost
          table_data <- results$power_data
          table_data$power <- round(table_data$power, 3)
          table_data$meets_threshold <- ifelse(table_data$meets_threshold, "Yes", "No")
          
          # Round additional numeric columns if they exist
          if ("cost" %in% names(table_data)) {
            table_data$cost <- round(table_data$cost, 2)
          }
          if ("cells" %in% names(table_data)) {
            table_data$cells <- round(table_data$cells, 0)
          }
          if ("reads" %in% names(table_data)) {
            table_data$reads <- round(table_data$reads, 0)
          }
          
          # Create appropriate column names based on available columns
          base_cols <- c("Parameter", "Value", "Power", "Meets Target")
          additional_cols <- c()
          
          if ("cells" %in% names(table_data)) {
            additional_cols <- c(additional_cols, "Cells")
          }
          if ("reads" %in% names(table_data)) {
            additional_cols <- c(additional_cols, "Reads")
          }
          if ("cost" %in% names(table_data)) {
            additional_cols <- c(additional_cols, "Cost ($)")
          }
          
          colnames(table_data) <- c(base_cols, additional_cols)
          
        } else {
          # Regular single parameter workflow: parameter_value, power, meets_threshold (3 columns)
          table_data <- results$power_data
          table_data$power <- round(table_data$power, 3)
          table_data$meets_threshold <- ifelse(table_data$meets_threshold, "Yes", "No")
          
          colnames(table_data) <- c("Parameter Value", "Power", "Meets Target")
        }
        
      } else {
        # Multi-parameter cost tradeoff workflows
        table_data <- results$power_data
        table_data$power <- round(table_data$power, 3)
        table_data$cost <- round(table_data$cost, 2)
        table_data$meets_threshold <- ifelse(table_data$power >= results$user_config$design_options$target_power, "Yes", "No")
        
        colnames(table_data) <- c("Cells", "Reads", "TPM Threshold", "Fold Change", "Power", "Cost ($)", "Meets Target")
      }
      
      datatable(
        table_data,
        options = list(
          pageLength = 10,
          scrollX = TRUE,
          dom = 'rtip',  # Removed 'f' to disable search functionality
          searching = FALSE  # Explicitly disable searching
        ),
        rownames = FALSE
      )
    })
    
    
    
    # ========================================================================
    # EXPORT FUNCTIONALITY
    # ========================================================================
    
    # Excel export using downloadHandler
    output$export_excel <- downloadHandler(
      filename = function() {
        paste0("perturbplan_analysis_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".xlsx")
      },
      content = function(file) {
        req(cached_analysis_results(), cached_plot_objects())
        
        results <- cached_analysis_results()
        plots <- cached_plot_objects()
        
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
        req(cached_plot_objects())
        
        plots <- cached_plot_objects()
        
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
  })
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
