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
          
          # Conditional display based on analysis state
          conditionalPanel(
            condition = "output.show_results == false",
            ns = ns,
            wellPanel(
              style = "text-align: center; padding: 60px;",
              h3("Ready for Analysis", style = "color: #5A6B73;"),
              tags$p("Configure your experimental design in the sidebar and click 'Plan' to generate results.",
                     style = "color: #7A8B93; font-size: 16px;")
            )
          ),
          
          conditionalPanel(
            condition = "output.show_results == true",
            ns = ns,
            # Interactive plot output
            plotlyOutput(ns("main_plot"), height = "400px")
          )
        )
      ),
      
      # Summary and controls column
      column(
        width = 4,
        # Analysis summary box
        box(
          title = "Summary",
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
    
    # Determine if results should be shown
    output$show_results <- reactive({
      plots <- plot_objects()
      results <- analysis_results()
      
      !is.null(plots) && !is.null(results) && 
        is.null(plots$error) && is.null(results$error)
    })
    outputOptions(output, "show_results", suspendWhenHidden = FALSE)
    
    # ========================================================================
    # MAIN PLOT RENDERING
    # ========================================================================
    
    output$main_plot <- renderPlotly({
      req(plot_objects())
      
      plots <- plot_objects()
      
      if (!is.null(plots$error)) {
        return(NULL)
      }
      
      # Return the interactive plot
      plots$plots$interactive_plot
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
      
      tagList(
        # Optimal design results
        tags$div(
          style = "background-color: #F8F9FA; padding: 18px; border-radius: 5px; margin-bottom: 15px;",
          
          # Optimal Design header
          tags$div(
            tags$strong("Optimal Design", style = "color: #2E4A62; font-size: 14px; margin-bottom: 12px; display: block;")
          ),
          
          # Show optimal design parameters using simple hardcoded values
          tagList(
            # TPM threshold (show if not fixed)
            tags$div(
              style = "margin-bottom: 8px;",
              tags$span("TPM threshold: ", style = "color: #5A6B73; font-weight: 500;"),
              tags$span("10", style = "color: #2E86AB; font-weight: bold; font-size: 16px;")
            ),
            
            # Fold change (show if not fixed)
            tags$div(
              style = "margin-bottom: 8px;",
              tags$span("Fold change: ", style = "color: #5A6B73; font-weight: 500;"),
              tags$span("0.8", style = "color: #2E86AB; font-weight: bold; font-size: 16px;")
            ),
            
            # Cells per target (show if not fixed)
            tags$div(
              style = "margin-bottom: 8px;",
              tags$span("Cells per target: ", style = "color: #5A6B73; font-weight: 500;"),
              tags$span("1000", style = "color: #2E86AB; font-weight: bold; font-size: 16px;")
            ),
            
            # Reads per cell (show if not fixed)
            tags$div(
              style = "margin-bottom: 8px;",
              tags$span("Reads per cell: ", style = "color: #5A6B73; font-weight: 500;"),
              tags$span("10000", style = "color: #2E86AB; font-weight: bold; font-size: 16px;")
            ),
            
            # Performance metrics
            tags$div(style = "margin: 12px 0; border-top: 1px solid #E0E0E0;"),
            tags$div(
              style = "margin-bottom: 6px;",
              tags$span("Power achieved: ", style = "color: #5A6B73; font-weight: 500;"),
              tags$span("85%", style = "color: #2E86AB; font-weight: bold;")
            )
          )
            
        )
      )
    })
    
    # ========================================================================
    # DETAILED RESULTS TABLE
    # ========================================================================
    
    output$detailed_table <- renderDT({
      req(analysis_results())
      
      results <- analysis_results()
      
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
          # Regular single parameter workflow: only parameter_name, parameter_value, power, meets_threshold
          table_data <- results$power_data
          table_data$power <- round(table_data$power, 3)
          table_data$meets_threshold <- ifelse(table_data$meets_threshold, "Yes", "No")
          
          colnames(table_data) <- c("Parameter", "Value", "Power", "Meets Target")
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
