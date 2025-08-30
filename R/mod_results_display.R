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
    # Two-column layout: Plot | Parameters, with Solutions table at bottom
    tagList(
      # Top row: Plot and Parameters side by side
      fluidRow(
        # Plot Column (left) - 70% of width
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
        
        # Parameters Column (right) - 30% of width, split into 2 sub-columns with 3 rows each
        column(
          width = 4,
          box(
            title = "Parameters",
            status = "success",
            solidHeader = TRUE,
            width = NULL,
            height = 500,
            
            # Parameter sliders in 2-column, 3-row grid
            conditionalPanel(
              condition = "output.show_sliders == true",
              ns = ns,
              tags$div(
                style = "max-height: 400px; overflow-y: auto; padding: 10px;",
                mod_parameter_sliders_ui(ns("sliders"))
              )
            ),
            
            # Placeholder when no sliders
            conditionalPanel(
              condition = "output.show_sliders == false",
              ns = ns,
              wellPanel(
                style = "text-align: center; padding: 60px;",
                tags$p("Parameter sliders will appear here after running the analysis.", 
                       style = "color: #7A8B93; font-size: 16px;")
              )
            )
          )
        )
      ),
      
      # Bottom row: Solutions table (full width)
      conditionalPanel(
        condition = "output.show_results == true",
        ns = ns,
        fluidRow(
          column(
            width = 12,
            box(
              title = "Solutions",
              status = "info",
              solidHeader = TRUE,
              width = NULL,
              
              # Solution table
              uiOutput(ns("solutions_table"))
            )
          )
        )
      ),
      
      # Placeholder message when no results (full width)
      conditionalPanel(
        condition = "output.show_results == false && output.show_error == false",
        ns = ns,
        fluidRow(
          column(
            width = 12,
            wellPanel(
              style = "text-align: center; padding: 20px; margin-top: 20px;",
              tags$p("Solutions table will appear here after running the analysis.",
                     style = "color: #7A8B93;")
            )
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
#' @importFrom scales comma
NULL

#' Helper function to check if two parameter sets are identical
#'
#' @param params1 First parameter set
#' @param params2 Second parameter set
#' @return TRUE if parameters are identical, FALSE otherwise
#' @noRd
are_parameters_identical <- function(params1, params2) {
  if (is.null(params1) || is.null(params2)) {
    return(FALSE)
  }
  
  # Get relevant parameters for comparison (exclude NA values)
  relevant_params <- c("MOI", "num_targets", "gRNAs_per_target", "cells_per_target", 
                      "reads_per_cell", "TPM_threshold", "minimum_fold_change", "cost_budget")
  
  for (param_name in relevant_params) {
    val1 <- params1[[param_name]]
    val2 <- params2[[param_name]]
    
    # Skip comparison if both are NULL/NA
    if ((is.null(val1) || is.na(val1)) && (is.null(val2) || is.na(val2))) {
      next
    }
    
    # Different if one is NULL/NA and other is not
    if ((is.null(val1) || is.na(val1)) != (is.null(val2) || is.na(val2))) {
      return(FALSE)
    }
    
    # Compare actual values with tolerance for numeric
    if (is.numeric(val1) && is.numeric(val2)) {
      if (abs(val1 - val2) > 1e-6) {  # Small tolerance for floating point
        return(FALSE)
      }
    } else if (!identical(val1, val2)) {
      return(FALSE)
    }
  }
  
  return(TRUE)
}

mod_results_display_server <- function(id, plot_objects, analysis_results, user_config = reactive(NULL), param_manager = NULL, slider_actions = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # ========================================================================
    # REACTIVE DISPLAY STATE
    # ========================================================================
    
    
    # Determine if results should be shown
    output$show_results <- reactive({
      # Don't show results if we're waiting for user to act after design change
      if (design_changed_waiting()) {
        return(FALSE)
      }
      
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
      # Don't show errors if we're waiting for user to act after design change
      if (design_changed_waiting()) {
        return(FALSE)
      }
      
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
    
    # Initialize slider module server with parameter manager and capture return values
    slider_actions <- NULL
    if (!is.null(param_manager)) {
      slider_actions <- mod_parameter_sliders_server("sliders", param_manager, workflow_info, user_config)
    }
    
    # ========================================================================
    # PINNED SOLUTIONS STORAGE
    # ========================================================================
    
    # Reactive storage for pinned solutions
    pinned_solutions <- reactiveValues(
      solutions = list(),
      next_index = 1
    )
    
    # ========================================================================
    # DESIGN OPTION CHANGE DETECTION - Clear solutions when workflow changes
    # ========================================================================
    
    # Track previous design configuration to detect changes
    previous_design_config <- reactiveVal(NULL)
    
    # Clear pinned solutions and plots when design options change
    observe({
      config <- user_config()
      if (!is.null(config) && !is.null(config$design_options)) {
        # Extract only the control types (not fixed_values) to avoid slider triggers
        param_control_types <- NULL
        if (!is.null(config$design_options$parameter_controls)) {
          param_control_types <- lapply(config$design_options$parameter_controls, function(control) {
            list(type = control$type)  # Only track the type, not fixed_value
          })
        }
        
        current_design <- list(
          optimization_type = config$design_options$optimization_type,
          minimization_target = config$design_options$minimization_target,
          parameter_control_types = param_control_types
        )
        
        prev_design <- previous_design_config()
        if (!is.null(prev_design) && !identical(prev_design, current_design)) {
          # Design options changed - clear pinned solutions
          pinned_solutions$solutions <- list()
          pinned_solutions$next_index <- 1
          
          showNotification(
            "Design options changed - cleared all results. Click 'Plan' to generate new analysis.",
            type = "message",
            duration = 4
          )
        }
      }
    })
    
    # Track when design has changed and we're waiting for user to click Plan
    design_changed_waiting <- reactiveVal(FALSE)
    
    # Update waiting state when design changes (mirror analysis engine detection)
    observe({
      config <- user_config()
      if (!is.null(config) && !is.null(config$design_options)) {
        # Use EXACT same logic as analysis engine
        current_design <- list(
          optimization_type = config$design_options$optimization_type,
          minimization_target = config$design_options$minimization_target,
          parameter_control_types = if (!is.null(config$design_options$parameter_controls)) {
            lapply(config$design_options$parameter_controls, function(control) {
              list(type = control$type)  # Only track the type, not fixed_value
            })
          } else {
            NULL
          }
        )
        
        prev_design <- previous_design_config()
        if (!is.null(prev_design) && !identical(prev_design, current_design)) {
          design_changed_waiting(TRUE)  # Mark as waiting for user action
        }
        
        previous_design_config(current_design)  # Update tracking state
      }
    })
    
    # Reset waiting state when analysis completes successfully
    # IMPORTANT: Only reset if we're not currently waiting due to a recent design change
    observe({
      results <- analysis_results()
      plots <- plot_objects()
      
      # Only reset waiting state if analysis actually completed AND we have fresh results
      if (!is.null(results) && !is.null(plots) && 
          is.null(results$error) && is.null(plots$error) &&
          !is.null(results$metadata) && !is.null(results$metadata$timestamp)) {
        
        # Only reset if this is a genuinely new analysis (not stale results)
        current_time <- Sys.time()
        result_age <- as.numeric(difftime(current_time, results$metadata$timestamp, units = "secs"))
        
        # Only reset waiting state if results are very fresh (< 5 seconds old)
        # This prevents stale results from resetting the waiting state
        if (result_age < 5) {
          design_changed_waiting(FALSE)  # Reset waiting state when analysis succeeds
        }
      }
    })
    
    # ========================================================================
    # PIN BUTTON HANDLERS
    # ========================================================================
    
    # Pin button handler - add current solution to pinned solutions
    if (!is.null(slider_actions)) {
      observeEvent(slider_actions$pin_requested(), {
        req(analysis_results())
        
        current_results <- analysis_results()
        current_plots <- plot_objects()
        current_params <- slider_actions$current_parameters()
        
        # Extract plot data from plot_objects
        current_plot_data <- NULL
        if (!is.null(current_plots$plots) && !is.null(current_plots$plots$plot_data)) {
          current_plot_data <- current_plots$plots$plot_data
        } else if (!is.null(current_plots$plot_data)) {
          current_plot_data <- current_plots$plot_data
        }
        
        # Check for duplicates before pinning
        is_duplicate <- FALSE
        if (length(pinned_solutions$solutions) > 0) {
          for (existing_solution in pinned_solutions$solutions) {
            if (are_parameters_identical(current_params, existing_solution$parameters)) {
              is_duplicate <- TRUE
              break
            }
          }
        }
        
        if (is_duplicate) {
          showNotification("Solution already pinned! Please change parameters to pin a different solution.", duration = 3)
        } else {
          # Create solution entry
          new_solution <- list(
            index = pinned_solutions$next_index,
            timestamp = Sys.time(),
            parameters = current_params,
            results = current_results,
            plot_data = current_plot_data
          )
          
          # Add to pinned solutions
          pinned_solutions$solutions <- append(pinned_solutions$solutions, list(new_solution))
          pinned_solutions$next_index <- pinned_solutions$next_index + 1
          
          showNotification("Solution pinned successfully!", duration = 2)
        }
      })
      
      # Clear pins button handler - reset to empty
      observeEvent(slider_actions$clear_requested(), {
        pinned_solutions$solutions <- list()
        pinned_solutions$next_index <- 1
        showNotification("All pins cleared.", duration = 2)
      })
    }
    
    # ========================================================================
    # UNIFIED PLOT DATA STRUCTURE
    # ========================================================================
    
    # Create unified plot data combining pinned + current solutions
    unified_plot_data <- reactive({
      current_results <- analysis_results()
      current_plots <- plot_objects()
      
      
      # If no current results or plots, return NULL
      if (is.null(current_results) || is.null(current_plots)) {
        return(NULL)
      }
      
      # Extract plot data from plot_objects instead of analysis_results
      current_plot_data <- NULL
      if (!is.null(current_plots$plots) && !is.null(current_plots$plots$plot_data)) {
        current_plot_data <- current_plots$plots$plot_data
      } else if (!is.null(current_plots$plot_data)) {
        current_plot_data <- current_plots$plot_data
      }
      
      if (is.null(current_plot_data)) {
        return(NULL)
      }
      
      # If no pinned solutions, return current data as-is (single solution)
      if (length(pinned_solutions$solutions) == 0) {
        return(current_plot_data)
      }
      
      # Create multi-solution structure for pinned + current
      multi_data <- list(
        solutions = list(),
        color_palette = c("#2E86AB", "#A23B72", "#F18F01", "#4CAF50", "#E91E63", 
                         "#FF9800", "#9C27B0", "#00BCD4", "#607D8B", "#C73E1D")
      )
      
      # Add pinned solutions with solid styling
      for (i in seq_along(pinned_solutions$solutions)) {
        pinned <- pinned_solutions$solutions[[i]]
        color_index <- ((pinned$index - 1) %% length(multi_data$color_palette)) + 1
        
        
        multi_data$solutions[[i]] <- list(
          id = pinned$index,
          color = multi_data$color_palette[color_index],
          data = pinned$plot_data,
          label = paste("Solution", pinned$index),
          style = "solid"
        )
      }
      
      # Add current pending solution with dashed styling
      multi_data$solutions[[length(multi_data$solutions) + 1]] <- list(
        id = "pending",
        color = "#FF6B6B",  # Distinct pending color (red-ish)
        data = current_plot_data,
        label = "Current",
        style = "dashed"
      )
      
      return(multi_data)
    })
    
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
        # First check if we should use Pin functionality (single parameter workflows with pinned solutions)
        current_results <- analysis_results()
        if (!is.null(current_results) && !is.null(current_results$workflow_info)) {
          workflow_id <- current_results$workflow_info$workflow_id
          
          # Only handle Pin functionality for single parameter optimization workflows
          single_param_workflows <- c(
            "power_single_cells_per_target",
            "power_single_reads_per_cell", 
            "power_single_TPM_threshold",
            "power_single_minimum_fold_change"
          )
          
          if (workflow_id %in% single_param_workflows && length(pinned_solutions$solutions) > 0) {
            # We have pinned solutions - use multi-solution plotting
            unified_data <- unified_plot_data()
            
            if (!is.null(unified_data) && is.list(unified_data) && "solutions" %in% names(unified_data)) {
              enhanced_results <- current_results
              enhanced_results$plot_data <- unified_data
              
              tryCatch({
                multi_plots <- create_multi_solution_parameter_plots(enhanced_results)
                
                if (!is.null(multi_plots$interactive_plot)) {
                  return(multi_plots$interactive_plot)
                }
              }, error = function(e) {
                # Silent error handling - fall back to standard plot
              })
            }
          }
        }
        
        # Standard plot rendering (original logic)
        req(plot_objects())
        plots <- plot_objects()
        
        if (!is.null(plots$error)) {
          return(NULL)
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
    # SOLUTIONS TABLE (Phase 2 - to be implemented)
    # ========================================================================
    
    output$solutions_table <- renderUI({
      req(analysis_results(), plot_objects())
      
      results <- analysis_results()
      plots <- plot_objects()
      
      if (!is.null(results$error) || !is.null(plots$error)) {
        return(tags$div(
          style = "color: #C73E1D; padding: 10px;",
          tags$p("Error in analysis - table cannot be generated.")
        ))
      }
      
      # Create enhanced solutions table with pinned solutions + current solution
      # Pass user_config and param_manager to access actual slider values
      create_enhanced_solutions_table(results, plots, user_config, param_manager, pinned_solutions)
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
    
    # No return needed - parameter manager handles all coordination
  })
}


# ============================================================================
# SOLUTIONS TABLE FUNCTIONS (Phase 2)
# ============================================================================

#' Create solutions table with hierarchical parameter display
#'
#' @description Creates a comprehensive solutions table showing current solution
#' as the first row. Future rows will be added via Pin functionality.
#'
#' @param results Analysis results object
#' @param plots Plot objects
#' @param user_config Reactive containing user configuration
#' @param param_manager Parameter manager instance
#' @return Shiny UI tagList with table
#' @noRd
create_solutions_table <- function(results, plots, user_config = reactive(NULL), param_manager = NULL) {
  if (is.null(results$optimal_design) || is.null(results$workflow_info)) {
    return(create_empty_solutions_table())
  }
  
  optimal <- results$optimal_design
  workflow_info <- results$workflow_info
  
  # Extract current solution data with access to slider values
  solution_row <- extract_solution_data(optimal, workflow_info, user_config, param_manager, index = 1)
  
  # Create table structure
  create_solutions_table_ui(list(solution_row), workflow_info)
}

#' Enhanced solutions table with pinned solutions support
#'
#' @description Creates solutions table showing pinned solutions + current solution
#' with visual distinction between pinned and current rows.
#'
#' @param results Analysis results object
#' @param plots Plot objects
#' @param user_config Reactive containing user configuration
#' @param param_manager Parameter manager instance
#' @param pinned_solutions ReactiveValues containing pinned solutions
#' @return Shiny UI tagList with enhanced table
#' @noRd
create_enhanced_solutions_table <- function(results, plots, user_config = reactive(NULL), param_manager = NULL, pinned_solutions) {
  if (is.null(results$optimal_design) || is.null(results$workflow_info)) {
    return(create_empty_solutions_table())
  }
  
  optimal <- results$optimal_design
  workflow_info <- results$workflow_info
  all_solution_rows <- list()
  
  # Add pinned solutions first
  if (!is.null(pinned_solutions) && length(pinned_solutions$solutions) > 0) {
    for (i in seq_along(pinned_solutions$solutions)) {
      pinned_solution <- pinned_solutions$solutions[[i]]
      
      # Extract solution data for pinned solution using its stored results
      if (!is.null(pinned_solution$results) && !is.null(pinned_solution$results$optimal_design)) {
        # Create a mock param_manager with pinned values instead of live values
        pinned_param_manager <- list(parameters = pinned_solution$parameters)
        
        solution_row <- extract_solution_data(
          pinned_solution$results$optimal_design, 
          pinned_solution$results$workflow_info, 
          reactive(pinned_solution$results$user_config),
          pinned_param_manager, 
          index = pinned_solution$index
        )
        # Mark as pinned for visual distinction
        solution_row$is_pinned <- TRUE
        all_solution_rows[[length(all_solution_rows) + 1]] <- solution_row
      }
    }
  }
  
  # Add current solution as the last row (if there are pinned solutions)
  if (length(all_solution_rows) > 0) {
    current_row <- extract_solution_data(optimal, workflow_info, user_config, param_manager, index = "Current")
    current_row$is_pinned <- FALSE
    all_solution_rows[[length(all_solution_rows) + 1]] <- current_row
  } else {
    # No pinned solutions - show just current solution with index 1
    current_row <- extract_solution_data(optimal, workflow_info, user_config, param_manager, index = 1)
    current_row$is_pinned <- FALSE
    all_solution_rows[[1]] <- current_row
  }
  
  # Create enhanced table structure
  create_enhanced_solutions_table_ui(all_solution_rows, workflow_info)
}

#' Extract solution data for table row
#'
#' @param optimal Optimal design results
#' @param workflow_info Workflow information
#' @param user_config Reactive containing user configuration
#' @param param_manager Parameter manager instance
#' @param index Solution index number
#' @return List with solution data
#' @noRd
extract_solution_data <- function(optimal, workflow_info, user_config = reactive(NULL), param_manager = NULL, index = 1) {
  list(
    index = index,
    achieved_power = extract_achieved_power(optimal),
    optimal_design = extract_optimal_design_value(optimal, workflow_info),
    experimental_choices = extract_experimental_choices(optimal, workflow_info, user_config, param_manager),
    analysis_choices = extract_analysis_choices(optimal, workflow_info, user_config, param_manager),
    effect_sizes = extract_effect_sizes(optimal, workflow_info, user_config, param_manager)
  )
}

#' Get optimal design column name based on minimizing parameter
#'
#' @param workflow_info Workflow information containing minimizing parameter
#' @return Column title string
#' @noRd
get_optimal_design_column_name <- function(workflow_info) {
  if (is.null(workflow_info) || is.null(workflow_info$minimizing_parameter)) {
    return("Optimal Design")
  }
  
  # Map minimizing parameters to readable column names
  param_name_mapping <- list(
    "cells_per_target" = "Optimal Cells per Target",
    "reads_per_cell" = "Optimal Reads per Cell", 
    "sequenced_reads_per_cell" = "Optimal Reads per Cell",
    "TPM_threshold" = "Optimal TPM Threshold",
    "minimum_fold_change" = "Optimal Fold Change",
    "cost" = "Optimal Cost"
  )
  
  column_name <- param_name_mapping[[workflow_info$minimizing_parameter]]
  if (is.null(column_name)) {
    # Fallback for unknown parameters
    return(paste("Optimal", tools::toTitleCase(gsub("_", " ", workflow_info$minimizing_parameter))))
  }
  
  return(column_name)
}

#' Determine which experimental parameter subcolumns to show
#'
#' @param solution_rows List of solution row data
#' @return List of experimental parameter column information
#' @noRd
determine_experimental_subcolumns <- function(solution_rows) {
  # Check which experimental parameters appear across all solutions
  all_exp_params <- list()
  for (row in solution_rows) {
    if (!is.null(row$experimental_choices) && length(row$experimental_choices) > 0) {
      all_exp_params <- c(all_exp_params, names(row$experimental_choices))
    }
  }
  
  # Get unique parameter names and create ordered list
  unique_params <- unique(all_exp_params)
  
  # Define parameter order and column headers
  param_order <- list(
    "MOI" = list(header = "MOI", width = "6%"),
    "Number of targets" = list(header = "# Targets", width = "6%"),
    "gRNAs per target" = list(header = "gRNAs/Target", width = "6%"),
    "Cells per target" = list(header = "Cells/Target", width = "6%"),
    "Reads per cell" = list(header = "Reads/Cell", width = "6%")
  )
  
  # Return only the parameters that actually appear
  visible_params <- list()
  for (param_name in names(param_order)) {
    if (param_name %in% unique_params) {
      visible_params[[param_name]] <- param_order[[param_name]]
    }
  }
  
  return(visible_params)
}

#' Determine which effect sizes parameter subcolumns to show
#'
#' @param solution_rows List of solution row data
#' @return List of effect sizes parameter column information
#' @noRd
determine_effect_sizes_subcolumns <- function(solution_rows) {
  # Check which effect sizes parameters appear across all solutions
  all_effect_params <- list()
  for (row in solution_rows) {
    if (!is.null(row$effect_sizes) && length(row$effect_sizes) > 0) {
      all_effect_params <- c(all_effect_params, names(row$effect_sizes))
    }
  }
  
  # Get unique parameter names and create ordered list
  unique_params <- unique(all_effect_params)
  
  # Define parameter order and column headers for effect sizes
  param_order <- list(
    "Fold change" = list(header = "Fold Change", width = "8%"),
    "Proportion non-null pairs" = list(header = "Non-null Prop", width = "8%")
  )
  
  # Return only the parameters that actually appear
  visible_params <- list()
  for (param_name in names(param_order)) {
    if (param_name %in% unique_params) {
      visible_params[[param_name]] <- param_order[[param_name]]
    }
  }
  
  return(visible_params)
}

#' Create the actual table UI
#'
#' @param solution_rows List of solution row data
#' @param workflow_info Workflow information containing minimizing parameter
#' @return Shiny UI tags
#' @noRd
create_solutions_table_ui <- function(solution_rows, workflow_info = NULL) {
  # Determine which columns to show based on whether they have content
  visible_columns <- determine_visible_columns(solution_rows)
  experimental_subcolumns <- determine_experimental_subcolumns(solution_rows)
  effect_sizes_subcolumns <- determine_effect_sizes_subcolumns(solution_rows)
  
  # Calculate experimental parameters total width
  exp_param_count <- length(experimental_subcolumns)
  exp_total_width <- if (exp_param_count > 0) 30 else 0  # 30% total for experimental params
  exp_subcolumn_width <- if (exp_param_count > 0) paste0(round(exp_total_width / exp_param_count, 1), "%") else "0%"
  
  # Calculate effect sizes total width
  effect_param_count <- length(effect_sizes_subcolumns)
  effect_total_width <- if (effect_param_count > 0) 16 else 0  # 16% total for effect sizes
  effect_subcolumn_width <- if (effect_param_count > 0) paste0(round(effect_total_width / effect_param_count, 1), "%") else "0%"
  
  # Create two-row header structure
  optimal_design_column_name <- get_optimal_design_column_name(workflow_info)
  
  # First header row (main column headers with colspan)
  header_row_1 <- list(
    tags$th("Solution ID", rowspan = "2", style = "width: 8%; text-align: center; font-weight: bold; background-color: #f8f9fa; vertical-align: middle;"),
    tags$th("Power", rowspan = "2", style = "width: 8%; text-align: center; font-weight: bold; background-color: #f8f9fa; vertical-align: middle;"),
    tags$th(optimal_design_column_name, rowspan = "2", style = "width: 15%; text-align: center; font-weight: bold; background-color: #f8f9fa; vertical-align: middle;")
  )
  
  # Add experimental parameters main header if needed
  if (visible_columns$experimental_choices && exp_param_count > 0) {
    header_row_1 <- append(header_row_1, list(
      tags$th("Experimental Parameters", colspan = as.character(exp_param_count), 
              style = paste0("width: ", exp_total_width, "%; text-align: center; font-weight: bold; background-color: #f8f9fa;"))
    ))
  }
  
  # Add other single-row headers
  if (visible_columns$analysis_choices) {
    header_row_1 <- append(header_row_1, list(
      tags$th("TPM threshold", rowspan = "2", style = "width: 10%; text-align: center; font-weight: bold; background-color: #f8f9fa; vertical-align: middle;")
    ))
  }
  
  # Add effect sizes main header if subcolumns are needed
  if (visible_columns$effect_sizes && effect_param_count > 0) {
    header_row_1 <- append(header_row_1, list(
      tags$th("Effect Sizes", colspan = as.character(effect_param_count), 
              style = paste0("width: ", effect_total_width, "%; text-align: center; font-weight: bold; background-color: #f8f9fa;"))
    ))
  } else if (visible_columns$effect_sizes) {
    # Single effect sizes column if no subcolumns
    header_row_1 <- append(header_row_1, list(
      tags$th("Effect Sizes", rowspan = "2", style = "width: 16%; text-align: center; font-weight: bold; background-color: #f8f9fa; vertical-align: middle;")
    ))
  }
  
  # Second header row (subcolumn headers for experimental parameters and effect sizes)
  header_row_2 <- list()
  if (visible_columns$experimental_choices && exp_param_count > 0) {
    for (param_name in names(experimental_subcolumns)) {
      param_info <- experimental_subcolumns[[param_name]]
      header_row_2 <- append(header_row_2, list(
        tags$th(param_info$header, style = paste0("width: ", exp_subcolumn_width, "; text-align: center; font-weight: bold; background-color: #e9ecef; font-size: 12px;"))
      ))
    }
  }
  
  # Add effect sizes subcolumn headers if needed
  if (visible_columns$effect_sizes && effect_param_count > 0) {
    for (param_name in names(effect_sizes_subcolumns)) {
      param_info <- effect_sizes_subcolumns[[param_name]]
      header_row_2 <- append(header_row_2, list(
        tags$th(param_info$header, style = paste0("width: ", effect_subcolumn_width, "; text-align: center; font-weight: bold; background-color: #e9ecef; font-size: 12px;"))
      ))
    }
  }
  
  # Create table header (two rows if experimental parameters or effect sizes have subcolumns)
  table_header <- if (length(header_row_2) > 0) {
    list(
      do.call(tags$tr, header_row_1),
      do.call(tags$tr, header_row_2)
    )
  } else {
    list(do.call(tags$tr, header_row_1))
  }
  
  # Create table rows with dynamic columns
  table_rows <- lapply(solution_rows, function(row_data) {
    row_cells <- list(
      # Index column
      tags$td(
        style = "text-align: center; padding: 12px; vertical-align: top; border-right: 1px solid #dee2e6;",
        tags$span(row_data$index, style = "font-size: 18px; font-weight: bold;")
      ),
      
      # Achieved Power column
      tags$td(
        style = "text-align: center; padding: 12px; vertical-align: top; border-right: 1px solid #dee2e6;",
        if (!is.null(row_data$achieved_power)) {
          tags$span(paste0(round(row_data$achieved_power * 100, 1), "%"), 
                   style = "font-size: 16px; font-weight: bold;")
        } else {
          tags$span("N/A", style = "color: #6c757d; font-style: italic;")
        }
      ),
      
      # Optimal Design column
      tags$td(
        style = "text-align: center; padding: 12px; vertical-align: top; border-right: 1px solid #dee2e6;",
        if (!is.null(row_data$optimal_design)) {
          if (grepl("<br>", row_data$optimal_design$value)) {
            # Multi-line content - render as HTML
            tags$div(
              HTML(row_data$optimal_design$value), 
              style = "font-size: 13px; font-weight: bold; line-height: 1.4;"
            )
          } else {
            # Single line content
            tags$span(row_data$optimal_design$value, style = "font-size: 15px; font-weight: bold;")
          }
        } else {
          tags$span("N/A", style = "color: #6c757d; font-style: italic;")
        }
      )
    )
    
    # Add experimental parameter subcolumns
    if (visible_columns$experimental_choices && exp_param_count > 0) {
      for (param_name in names(experimental_subcolumns)) {
        param_value <- if (!is.null(row_data$experimental_choices[[param_name]])) {
          row_data$experimental_choices[[param_name]]
        } else {
          "-"
        }
        
        row_cells <- append(row_cells, list(
          tags$td(
            style = "text-align: center; padding: 8px; vertical-align: top; border-right: 1px solid #dee2e6;",
            tags$span(param_value, style = "font-size: 13px; font-weight: 500;")
          )
        ))
      }
    }
    
    if (visible_columns$analysis_choices) {
      row_cells <- append(row_cells, list(
        tags$td(
          style = "padding: 12px; vertical-align: top; border-right: 1px solid #dee2e6;",
          create_parameter_section_display(row_data$analysis_choices, "Analysis")
        )
      ))
    }
    
    # Effect sizes - use subcolumns if available, otherwise single column
    if (visible_columns$effect_sizes && effect_param_count > 0) {
      # Generate individual cells for each effect size parameter
      for (param_name in names(effect_sizes_subcolumns)) {
        param_info <- effect_sizes_subcolumns[[param_name]]
        param_value <- if (!is.null(row_data$effect_sizes[[param_name]])) {
          if (param_name == "Fold change") {
            format(round(as.numeric(row_data$effect_sizes[[param_name]]), 2), nsmall = 2)
          } else if (param_name == "Proportion non-null pairs") {
            paste0(round(as.numeric(row_data$effect_sizes[[param_name]]) * 100, 1), "%")
          } else {
            row_data$effect_sizes[[param_name]]
          }
        } else {
          "-"
        }
        
        row_cells <- append(row_cells, list(
          tags$td(
            style = "padding: 8px; text-align: center; vertical-align: top; border-right: 1px solid #dee2e6;",
            tags$span(param_value, style = "font-size: 13px;")
          )
        ))
      }
    } else if (visible_columns$effect_sizes) {
      # Single effect sizes column
      row_cells <- append(row_cells, list(
        tags$td(
          style = "padding: 12px; vertical-align: top;",
          create_parameter_section_display(row_data$effect_sizes, "Effect")
        )
      ))
    }
    
    do.call(tags$tr, row_cells)
  })
  
  # Return complete table
  tags$div(
    style = "overflow-x: auto;",
    tags$table(
      class = "table table-bordered",
      style = "width: 100%; margin: 0; border-collapse: collapse; font-size: 13px;",
      tags$thead(table_header),
      tags$tbody(table_rows)
    )
  )
}

#' Create enhanced solutions table UI with visual distinction for pinned vs current
#'
#' @description Enhanced version of create_solutions_table_ui that adds visual
#' distinction between pinned solutions (solid background) and current solution
#' (highlighted background with "Current" styling).
#'
#' @param solution_rows List of solution row data (with is_pinned property)
#' @param workflow_info Workflow information containing minimizing parameter
#' @return Shiny UI tags
#' @noRd
create_enhanced_solutions_table_ui <- function(solution_rows, workflow_info = NULL) {
  # Determine which columns to show based on whether they have content
  visible_columns <- determine_visible_columns(solution_rows)
  experimental_subcolumns <- determine_experimental_subcolumns(solution_rows)
  effect_sizes_subcolumns <- determine_effect_sizes_subcolumns(solution_rows)
  
  # Calculate experimental parameters total width
  exp_param_count <- length(experimental_subcolumns)
  exp_total_width <- if (exp_param_count > 0) 30 else 0  # 30% total for experimental params
  exp_subcolumn_width <- if (exp_param_count > 0) paste0(round(exp_total_width / exp_param_count, 1), "%") else "0%"
  
  # Calculate effect sizes total width
  effect_param_count <- length(effect_sizes_subcolumns)
  effect_total_width <- if (effect_param_count > 0) 16 else 16  # 16% total for effect sizes
  effect_subcolumn_width <- if (effect_param_count > 0) paste0(round(effect_total_width / effect_param_count, 1), "%") else "16%"
  
  # Create two-row header structure
  optimal_design_column_name <- get_optimal_design_column_name(workflow_info)
  
  # First header row (main column headers with colspan)
  header_row_1 <- list(
    tags$th("Solution ID", rowspan = "2", style = "width: 8%; text-align: center; font-weight: bold; background-color: #f8f9fa; vertical-align: middle;"),
    tags$th("Power", rowspan = "2", style = "width: 8%; text-align: center; font-weight: bold; background-color: #f8f9fa; vertical-align: middle;"),
    tags$th(optimal_design_column_name, rowspan = "2", style = "width: 15%; text-align: center; font-weight: bold; background-color: #f8f9fa; vertical-align: middle;")
  )
  
  # Add experimental parameters main header if needed
  if (visible_columns$experimental_choices && exp_param_count > 0) {
    header_row_1 <- append(header_row_1, list(
      tags$th("Experimental Parameters", colspan = as.character(exp_param_count), 
              style = paste0("width: ", exp_total_width, "%; text-align: center; font-weight: bold; background-color: #f8f9fa;"))
    ))
  }
  
  # Add other single-row headers
  if (visible_columns$analysis_choices) {
    header_row_1 <- append(header_row_1, list(
      tags$th("TPM threshold", rowspan = "2", style = "width: 10%; text-align: center; font-weight: bold; background-color: #f8f9fa; vertical-align: middle;")
    ))
  }
  
  # Add effect sizes main header if subcolumns are needed
  if (visible_columns$effect_sizes && effect_param_count > 0) {
    header_row_1 <- append(header_row_1, list(
      tags$th("Effect Sizes", colspan = as.character(effect_param_count), 
              style = paste0("width: ", effect_total_width, "%; text-align: center; font-weight: bold; background-color: #f8f9fa;"))
    ))
  } else if (visible_columns$effect_sizes) {
    # Single effect sizes column if no subcolumns
    header_row_1 <- append(header_row_1, list(
      tags$th("Effect Sizes", rowspan = "2", style = "width: 16%; text-align: center; font-weight: bold; background-color: #f8f9fa; vertical-align: middle;")
    ))
  }
  
  # Second header row (subcolumn headers for experimental parameters and effect sizes)
  header_row_2 <- list()
  if (visible_columns$experimental_choices && exp_param_count > 0) {
    for (param_name in names(experimental_subcolumns)) {
      param_info <- experimental_subcolumns[[param_name]]
      header_row_2 <- append(header_row_2, list(
        tags$th(param_info$header, style = paste0("width: ", exp_subcolumn_width, "; text-align: center; font-weight: bold; background-color: #e9ecef; font-size: 12px;"))
      ))
    }
  }
  
  # Add effect sizes subcolumn headers if needed
  if (visible_columns$effect_sizes && effect_param_count > 0) {
    for (param_name in names(effect_sizes_subcolumns)) {
      param_info <- effect_sizes_subcolumns[[param_name]]
      header_row_2 <- append(header_row_2, list(
        tags$th(param_info$header, style = paste0("width: ", effect_subcolumn_width, "; text-align: center; font-weight: bold; background-color: #e9ecef; font-size: 12px;"))
      ))
    }
  }
  
  # Create table header (two rows if experimental parameters or effect sizes have subcolumns)
  table_header <- if (length(header_row_2) > 0) {
    list(
      do.call(tags$tr, header_row_1),
      do.call(tags$tr, header_row_2)
    )
  } else {
    list(do.call(tags$tr, header_row_1))
  }
  
  # Create table rows with visual distinction for pinned vs current
  table_rows <- lapply(solution_rows, function(row_data) {
    # Visual distinction: pinned solutions have normal background, current has highlighted background
    row_bg_color <- if (!is.null(row_data$is_pinned) && !row_data$is_pinned) {
      "#fff3cd"  # Light yellow background for current solution
    } else {
      "white"    # White background for pinned solutions
    }
    
    row_cells <- list(
      # Index column with visual distinction
      tags$td(
        style = paste0("text-align: center; padding: 12px; vertical-align: top; border-right: 1px solid #dee2e6; background-color: ", row_bg_color, ";"),
        if (!is.null(row_data$is_pinned) && !row_data$is_pinned) {
          tags$span(row_data$index, style = "font-size: 18px; font-weight: bold;")  # Current solution
        } else {
          tags$span(row_data$index, style = "font-size: 18px; font-weight: bold;")  # Pinned solution
        }
      ),
      
      # Achieved Power column
      tags$td(
        style = paste0("text-align: center; padding: 12px; vertical-align: top; border-right: 1px solid #dee2e6; background-color: ", row_bg_color, ";"),
        if (!is.null(row_data$achieved_power)) {
          tags$span(paste0(round(row_data$achieved_power * 100, 1), "%"), 
                   style = "font-size: 16px; font-weight: bold;")
        } else {
          tags$span("N/A", style = "color: #6c757d; font-style: italic;")
        }
      ),
      
      # Optimal Design column
      tags$td(
        style = paste0("text-align: center; padding: 12px; vertical-align: top; border-right: 1px solid #dee2e6; background-color: ", row_bg_color, ";"),
        if (!is.null(row_data$optimal_design)) {
          if (grepl("<br>", row_data$optimal_design$value)) {
            # Multi-line content - render as HTML
            tags$div(
              HTML(row_data$optimal_design$value), 
              style = "font-size: 13px; font-weight: bold; line-height: 1.4;"
            )
          } else {
            # Single line content
            tags$span(row_data$optimal_design$value, style = "font-size: 15px; font-weight: bold;")
          }
        } else {
          tags$span("N/A", style = "color: #6c757d; font-style: italic;")
        }
      )
    )
    
    # Add experimental parameter subcolumns with background color
    if (visible_columns$experimental_choices && exp_param_count > 0) {
      for (param_name in names(experimental_subcolumns)) {
        param_value <- if (!is.null(row_data$experimental_choices[[param_name]])) {
          row_data$experimental_choices[[param_name]]
        } else {
          "-"
        }
        
        row_cells <- append(row_cells, list(
          tags$td(
            style = paste0("text-align: center; padding: 8px; vertical-align: top; border-right: 1px solid #dee2e6; background-color: ", row_bg_color, ";"),
            tags$span(param_value, style = "font-size: 13px; font-weight: 500;")
          )
        ))
      }
    }
    
    if (visible_columns$analysis_choices) {
      row_cells <- append(row_cells, list(
        tags$td(
          style = paste0("padding: 12px; vertical-align: top; border-right: 1px solid #dee2e6; background-color: ", row_bg_color, ";"),
          create_parameter_section_display(row_data$analysis_choices, "Analysis")
        )
      ))
    }
    
    # Effect sizes - use subcolumns if available, otherwise single column
    if (visible_columns$effect_sizes && effect_param_count > 0) {
      # Generate individual cells for each effect size parameter
      for (param_name in names(effect_sizes_subcolumns)) {
        param_info <- effect_sizes_subcolumns[[param_name]]
        param_value <- if (!is.null(row_data$effect_sizes[[param_name]])) {
          if (param_name == "Fold change") {
            format(round(as.numeric(row_data$effect_sizes[[param_name]]), 2), nsmall = 2)
          } else if (param_name == "Proportion non-null pairs") {
            paste0(round(as.numeric(row_data$effect_sizes[[param_name]]) * 100, 1), "%")
          } else {
            row_data$effect_sizes[[param_name]]
          }
        } else {
          "-"
        }
        
        row_cells <- append(row_cells, list(
          tags$td(
            style = paste0("padding: 8px; text-align: center; vertical-align: top; border-right: 1px solid #dee2e6; background-color: ", row_bg_color, ";"),
            tags$span(param_value, style = "font-size: 13px;")
          )
        ))
      }
    } else if (visible_columns$effect_sizes) {
      # Single effect sizes column
      row_cells <- append(row_cells, list(
        tags$td(
          style = paste0("padding: 12px; vertical-align: top; background-color: ", row_bg_color, ";"),
          create_parameter_section_display(row_data$effect_sizes, "Effect")
        )
      ))
    }
    
    do.call(tags$tr, row_cells)
  })
  
  # Return complete table
  tags$div(
    style = "overflow-x: auto;",
    tags$table(
      class = "table table-bordered",
      style = "width: 100%; margin: 0; border-collapse: collapse; font-size: 13px;",
      tags$thead(table_header),
      tags$tbody(table_rows)
    )
  )
}

#' Determine which columns should be visible
#'
#' @param solution_rows List of solution row data
#' @return List with visibility flags for each column type
#' @noRd
determine_visible_columns <- function(solution_rows) {
  # Check if any row has content for each category
  has_experimental <- any(sapply(solution_rows, function(row) {
    !is.null(row$experimental_choices) && length(row$experimental_choices) > 0
  }))
  
  has_analysis <- any(sapply(solution_rows, function(row) {
    !is.null(row$analysis_choices) && length(row$analysis_choices) > 0
  }))
  
  has_effect <- any(sapply(solution_rows, function(row) {
    !is.null(row$effect_sizes) && length(row$effect_sizes) > 0
  }))
  
  list(
    experimental_choices = has_experimental,
    analysis_choices = has_analysis,
    effect_sizes = has_effect
  )
}

#' Create parameter section display for table cells
#'
#' @param params List of parameters
#' @param section_type Type hint for styling
#' @return Shiny UI tags
#' @noRd
create_parameter_section_display <- function(params, section_type = "default") {
  if (is.null(params) || length(params) == 0) {
    return(tags$span("-", style = "color: #6c757d; font-style: italic;"))
  }
  
  # Special handling for Analysis section (TPM threshold column) - show only values
  if (section_type == "Analysis") {
    tags$div(
      style = "line-height: 1.5;",
      lapply(names(params), function(param_name) {
        if (!is.null(params[[param_name]])) {
          tags$div(
            style = "margin-bottom: 4px; text-align: center;",
            tags$span(params[[param_name]], style = "color: #495057; font-weight: 500; font-size: 14px;")
          )
        }
      })
    )
  } else if (section_type == "Experimental") {
    # Special compact format for Experimental section - show all parameters in one row
    param_strings <- lapply(names(params), function(param_name) {
      if (!is.null(params[[param_name]])) {
        paste0(param_name, ": ", params[[param_name]])
      }
    })
    param_strings <- param_strings[!sapply(param_strings, is.null)]
    
    tags$div(
      style = "line-height: 1.3;",
      tags$span(
        paste(param_strings, collapse = " • "), 
        style = "color: #495057; font-weight: 500; font-size: 13px;"
      )
    )
  } else {
    # Default format for other sections - show parameter name and value
    tags$div(
      style = "line-height: 1.5;",
      lapply(names(params), function(param_name) {
        if (!is.null(params[[param_name]])) {
          tags$div(
            style = "margin-bottom: 4px;",
            tags$span(param_name, style = "color: #5A6B73; font-size: 12px; font-weight: 500;"),
            tags$span(": ", style = "color: #6c757d;"),
            tags$span(params[[param_name]], style = "color: #495057; font-weight: 500;")
          )
        }
      })
    )
  }
}

#' Extract achieved power value
#'
#' @param optimal Optimal design results
#' @return Numeric power value or NULL
#' @noRd
extract_achieved_power <- function(optimal) {
  if (!is.null(optimal$achieved_power) && !is.na(optimal$achieved_power)) {
    return(optimal$achieved_power)
  }
  return(NULL)
}

#' Extract optimal design value based on workflow
#'
#' @param optimal Optimal design results
#' @param workflow_info Workflow information
#' @return List with label and value
#' @noRd
extract_optimal_design_value <- function(optimal, workflow_info) {
  minimizing_param <- workflow_info$minimizing_parameter
  
  # Check if this is a workflow where cells and reads are varying
  # (cost minimization or TPM/FC minimization with cells+reads varying)
  cells_and_reads_varying <- workflow_info$workflow_id %in% c(
    "power_cost_minimization",           # Cost minimization (cells+reads vary)
    "power_cost_TPM_cells_reads",       # TPM minimization with cells+reads varying
    "power_cost_fc_cells_reads"         # FC minimization with cells+reads varying
  )
  
  # Check if this is a workflow where one of cells/reads is cost-constrained
  cells_or_reads_constrained <- workflow_info$workflow_id %in% c(
    "power_cost_TPM_cells",             # TPM min with cells fixed, reads cost-constrained
    "power_cost_TPM_reads",             # TPM min with reads fixed, cells cost-constrained
    "power_cost_fc_cells",              # FC min with cells fixed, reads cost-constrained
    "power_cost_fc_reads"               # FC min with reads fixed, cells cost-constrained
  )
  
  if (minimizing_param == "TPM_threshold") {
    if (cells_and_reads_varying) {
      # Show TPM + optimal cells and reads (both varying)
      return(list(
        label = "Optimal Design",
        value = create_multi_param_display(list(
          "TPM Threshold" = if (!is.null(optimal$TPM_threshold)) round(optimal$TPM_threshold) else "N/A",
          "Cells per target" = if (!is.null(optimal$cells_per_target)) scales::comma(round(optimal$cells_per_target)) else "N/A",
          "Reads per cell" = if (!is.null(optimal$sequenced_reads_per_cell)) scales::comma(round(optimal$sequenced_reads_per_cell)) else "N/A"
        ))
      ))
    } else if (cells_or_reads_constrained) {
      # Show TPM + cost-constrained parameter
      if (workflow_info$workflow_id == "power_cost_TPM_cells") {
        # Cells varying (cost-constrained), reads fixed
        return(list(
          label = "Optimal Design",
          value = create_multi_param_display(list(
            "TPM Threshold" = if (!is.null(optimal$TPM_threshold)) round(optimal$TPM_threshold) else "N/A",
            "Cost-constrained cells per target" = if (!is.null(optimal$cells_per_target)) scales::comma(round(optimal$cells_per_target)) else "N/A"
          ))
        ))
      } else if (workflow_info$workflow_id == "power_cost_TPM_reads") {
        # Reads varying (cost-constrained), cells fixed
        return(list(
          label = "Optimal Design",
          value = create_multi_param_display(list(
            "TPM Threshold" = if (!is.null(optimal$TPM_threshold)) round(optimal$TPM_threshold) else "N/A",
            "Cost-constrained reads per cell" = if (!is.null(optimal$sequenced_reads_per_cell)) scales::comma(round(optimal$sequenced_reads_per_cell)) else "N/A"
          ))
        ))
      }
    } else {
      return(list(
        label = "TPM Threshold",
        value = if (!is.null(optimal$TPM_threshold)) round(optimal$TPM_threshold) else "N/A"
      ))
    }
  } else if (minimizing_param == "minimum_fold_change") {
    if (cells_and_reads_varying) {
      # Show FC + optimal cells and reads (both varying)
      return(list(
        label = "Optimal Design",
        value = create_multi_param_display(list(
          "Fold Change" = if (!is.null(optimal$minimum_fold_change)) round(optimal$minimum_fold_change, 2) else "N/A",
          "Cells per target" = if (!is.null(optimal$cells_per_target)) scales::comma(round(optimal$cells_per_target)) else "N/A",
          "Reads per cell" = if (!is.null(optimal$sequenced_reads_per_cell)) scales::comma(round(optimal$sequenced_reads_per_cell)) else "N/A"
        ))
      ))
    } else if (cells_or_reads_constrained) {
      # Show FC + cost-constrained parameter
      if (workflow_info$workflow_id == "power_cost_fc_cells") {
        # Cells varying (cost-constrained), reads fixed
        return(list(
          label = "Optimal Design",
          value = create_multi_param_display(list(
            "Fold Change" = if (!is.null(optimal$minimum_fold_change)) round(optimal$minimum_fold_change, 2) else "N/A",
            "Cost-constrained cells per target" = if (!is.null(optimal$cells_per_target)) scales::comma(round(optimal$cells_per_target)) else "N/A"
          ))
        ))
      } else if (workflow_info$workflow_id == "power_cost_fc_reads") {
        # Reads varying (cost-constrained), cells fixed
        return(list(
          label = "Optimal Design",
          value = create_multi_param_display(list(
            "Fold Change" = if (!is.null(optimal$minimum_fold_change)) round(optimal$minimum_fold_change, 2) else "N/A",
            "Cost-constrained reads per cell" = if (!is.null(optimal$sequenced_reads_per_cell)) scales::comma(round(optimal$sequenced_reads_per_cell)) else "N/A"
          ))
        ))
      }
    } else {
      return(list(
        label = "Fold Change",
        value = if (!is.null(optimal$minimum_fold_change)) round(optimal$minimum_fold_change, 2) else "N/A"
      ))
    }
  } else if (minimizing_param == "cells_per_target") {
    return(list(
      label = "Cells per Target",
      value = if (!is.null(optimal$cells_per_target)) scales::comma(round(optimal$cells_per_target)) else "N/A"
    ))
  } else if (minimizing_param %in% c("reads_per_cell", "sequenced_reads_per_cell")) {
    return(list(
      label = "Reads per Cell",
      value = if (!is.null(optimal$sequenced_reads_per_cell)) scales::comma(round(optimal$sequenced_reads_per_cell)) else "N/A"
    ))
  } else if (minimizing_param == "cost") {
    # Cost minimization: Show total cost + optimal cells and reads
    return(list(
      label = "Optimal Design",
      value = create_multi_param_display(list(
        "Total Cost" = if (!is.null(optimal$total_cost)) paste0("$", scales::comma(round(optimal$total_cost))) else "N/A",
        "Cells per target" = if (!is.null(optimal$cells_per_target)) scales::comma(round(optimal$cells_per_target)) else "N/A",
        "Reads per cell" = if (!is.null(optimal$sequenced_reads_per_cell)) scales::comma(round(optimal$sequenced_reads_per_cell)) else "N/A"
      ))
    ))
  }
  
  return(list(label = "Unknown", value = "N/A"))
}

#' Create formatted display for multiple parameters in optimal design
#'
#' @param param_list Named list of parameters and their values
#' @return HTML formatted string
#' @noRd
create_multi_param_display <- function(param_list) {
  param_strings <- lapply(names(param_list), function(name) {
    paste0(name, ": ", param_list[[name]])
  })
  paste(param_strings, collapse = "<br>")
}

#' Extract experimental choices for display
#'
#' @param optimal Optimal design results
#' @param user_config Reactive containing user configuration
#' @param param_manager Parameter manager instance
#' @return Named list of experimental choice parameters (only those with sliders)
#' @noRd
extract_experimental_choices <- function(optimal, workflow_info = NULL, user_config = reactive(NULL), param_manager = NULL) {
  params <- list()
  
  # Get the parameter being minimized and workflow type
  minimizing_param <- NULL
  is_cost_minimization <- FALSE
  cells_reads_varying <- FALSE
  if (!is.null(workflow_info)) {
    minimizing_param <- workflow_info$minimizing_parameter
    is_cost_minimization <- workflow_info$workflow_id == "power_cost_minimization"
    # Also check for TPM/FC minimization workflows where cells and reads are both varying
    cells_reads_varying <- workflow_info$workflow_id %in% c(
      "power_cost_minimization",           # Cost minimization (cells+reads vary)
      "power_cost_TPM_cells_reads",       # TPM minimization with cells+reads varying
      "power_cost_fc_cells_reads"         # FC minimization with cells+reads varying
    )
  }
  
  # Show experimental parameters that appear in sliders AND are not being minimized
  # This includes both Row 1 (MOI, targets, gRNAs) and Row 2 experimental parameters (cells, reads)
  if (!is.null(param_manager) && !is.null(param_manager$parameters)) {
    
    # Row 1 parameters (always experimental choices when present and not minimized)
    if (!is.null(param_manager$parameters$MOI)) {
      params[["MOI"]] <- param_manager$parameters$MOI
    }
    if (!is.null(param_manager$parameters$num_targets)) {
      params[["Number of targets"]] <- param_manager$parameters$num_targets
    }
    if (!is.null(param_manager$parameters$gRNAs_per_target)) {
      params[["gRNAs per target"]] <- param_manager$parameters$gRNAs_per_target
    }
    
    # Row 2 experimental parameters (cells and reads)
    # In workflows where cells and reads are varying parameters (cost minimization or TPM/FC min with both varying),
    # they don't have sliders, so they should NOT appear in slider columns
    
    # For cells per target:
    # Show ONLY if cells is fixed (not cost-constrained or varying)
    # Exclude if: 1) both cells+reads varying, 2) cells is being minimized, 3) cells is cost-constrained
    cells_should_exclude <- cells_reads_varying ||  # Both varying
                           (!is.null(minimizing_param) && minimizing_param == "cells_per_target") ||  # Cells minimized
                           (!is.null(workflow_info$workflow_id) && workflow_info$workflow_id %in% c("power_cost_TPM_cells", "power_cost_fc_cells"))  # Cells cost-constrained (reads is fixed)
    
    if (!cells_should_exclude && !is.null(param_manager$parameters$cells_per_target)) {
      params[["Cells per target"]] <- param_manager$parameters$cells_per_target
    }
    
    # For reads per cell:
    # Show ONLY if reads is fixed (not cost-constrained or varying)
    # Exclude if: 1) both cells+reads varying, 2) reads is being minimized, 3) reads is cost-constrained  
    reads_should_exclude <- cells_reads_varying ||  # Both varying
                           (!is.null(minimizing_param) && minimizing_param %in% c("reads_per_cell", "sequenced_reads_per_cell")) ||  # Reads minimized
                           (!is.null(workflow_info$workflow_id) && workflow_info$workflow_id %in% c("power_cost_TPM_reads", "power_cost_fc_reads"))  # Reads cost-constrained (cells is fixed)
    
    if (!reads_should_exclude && !is.null(param_manager$parameters$reads_per_cell)) {
      params[["Reads per cell"]] <- param_manager$parameters$reads_per_cell
    }
  }
  
  return(params)
}

#' Extract analysis choices for display
#'
#' @param optimal Optimal design results
#' @param workflow_info Workflow information
#' @param user_config Reactive containing user configuration
#' @param param_manager Parameter manager instance
#' @return Named list of analysis choice parameters (only those with sliders, excluding minimized parameter)
#' @noRd
extract_analysis_choices <- function(optimal, workflow_info, user_config = reactive(NULL), param_manager = NULL) {
  minimizing_param <- workflow_info$minimizing_parameter
  params <- list()
  
  # Only show parameters that appear in sliders AND are not being minimized
  if (!is.null(param_manager) && !is.null(param_manager$parameters)) {
    
    # Analysis choices should only include analysis-specific parameters (TPM threshold)
    # Cells and reads are now handled in experimental choices
    
    # Check if TPM threshold slider is visible (Row 2) and not being minimized
    if ((is.null(minimizing_param) || minimizing_param != "TPM_threshold") && 
        parameter_has_slider("TPM_threshold", user_config, workflow_info) &&
        !is.null(param_manager$parameters$TPM_threshold)) {
      params[["TPM threshold"]] <- param_manager$parameters$TPM_threshold
    }
  }
  
  return(params)
}

#' Extract effect sizes for display
#'
#' @param optimal Optimal design results
#' @param workflow_info Workflow information
#' @param user_config Reactive containing user configuration
#' @param param_manager Parameter manager instance
#' @return Named list of effect size parameters (only those with sliders, excluding minimized parameter)
#' @noRd
extract_effect_sizes <- function(optimal, workflow_info, user_config = reactive(NULL), param_manager = NULL) {
  minimizing_param <- workflow_info$minimizing_parameter
  params <- list()
  
  # Only show parameters that appear in sliders AND are not being minimized
  if (!is.null(param_manager) && !is.null(param_manager$parameters)) {
    
    # Check if fold change slider is visible (Row 2) and not being minimized
    if ((is.null(minimizing_param) || minimizing_param != "minimum_fold_change") && 
        parameter_has_slider("minimum_fold_change", user_config, workflow_info) &&
        !is.null(param_manager$parameters$minimum_fold_change)) {
      params[["Fold change"]] <- param_manager$parameters$minimum_fold_change
    }
  }
  
  # Add proportion of non-null pairs as a fixed parameter (always shown)
  # This comes from the optimal design results or user config
  prop_non_null_value <- NULL
  if (!is.null(optimal$prop_non_null)) {
    prop_non_null_value <- optimal$prop_non_null
  } else if (!is.null(user_config)) {
    config <- user_config()
    if (!is.null(config$effect_sizes) && !is.null(config$effect_sizes$prop_non_null)) {
      prop_non_null_value <- config$effect_sizes$prop_non_null
    }
  }
  
  # Default value if not found elsewhere
  if (is.null(prop_non_null_value)) {
    prop_non_null_value <- 0.1  # Default proportion commonly used
  }
  
  params[["Proportion non-null pairs"]] <- prop_non_null_value
  
  return(params)
}

#' Create empty solutions table when no data available
#'
#' @return Shiny UI tags
#' @noRd
create_empty_solutions_table <- function() {
  tags$div(
    style = "text-align: center; padding: 40px; color: #7A8B93;",
    tags$p("No solutions available yet. Run an analysis to see results.", style = "font-style: italic;")
  )
}

#' Check if a parameter has a slider in the current workflow
#'
#' @param param_name Parameter name to check
#' @param user_config Reactive containing user configuration
#' @param workflow_info Workflow information
#' @return Boolean indicating if parameter has a slider
#' @noRd
parameter_has_slider <- function(param_name, user_config = reactive(NULL), workflow_info) {
  # This function determines if a parameter appears in Row 2 of the parameter sliders
  # based on the slider logic from mod_parameter_sliders.R
  
  if (is.null(workflow_info) || is.null(workflow_info$workflow_id)) {
    return(FALSE)
  }
  
  # Get the minimized parameter for this workflow
  minimized_param <- get_minimized_parameter_for_display(workflow_info$workflow_id)
  
  # Parameters in Row 2 are: cells_per_target, reads_per_cell, TPM_threshold, minimum_fold_change
  # But exclude the minimized parameter
  row2_params <- c("cells_per_target", "reads_per_cell", "TPM_threshold", "minimum_fold_change")
  
  # Map parameter names
  param_mapping <- list(
    "cells_per_target" = "cells_per_target",
    "reads_per_cell" = "reads_per_cell", 
    "TPM_threshold" = "TPM_threshold",
    "minimum_fold_change" = "minimum_fold_change"
  )
  
  # Check if parameter is in Row 2 and not minimized
  if (param_name %in% names(param_mapping)) {
    mapped_param <- param_mapping[[param_name]]
    is_in_row2 <- mapped_param %in% row2_params
    is_not_minimized <- is.null(minimized_param) || length(minimized_param) == 0 || mapped_param != minimized_param
    
    # For cost minimization workflow, also check power+cost filtering logic
    if (!is.null(user_config)) {
      config <- user_config()
      if (!is.null(config) && !is.null(config$design_options)) {
        design_config <- config$design_options
        param_controls <- design_config$parameter_controls
        optimization_type <- design_config$optimization_type
        
        # Apply power+cost mode filtering (show only "fixed" parameters)
        if (!is.null(optimization_type) && optimization_type == "power_cost" && !is.null(param_controls)) {
          control_name_mapping <- list(
            "cells_per_target" = "cells_per_target",
            "reads_per_cell" = "sequenced_reads_per_cell",
            "TPM_threshold" = "TPM_threshold", 
            "minimum_fold_change" = "minimum_fold_change"
          )
          
          control_name <- control_name_mapping[[param_name]]
          if (!is.null(control_name) && !is.null(param_controls[[control_name]])) {
            param_type <- param_controls[[control_name]]$type
            return(is_in_row2 && is_not_minimized && param_type == "fixed")
          }
        }
        
        # For cost minimization, check if it's TPM or FC (they are shown as sliders)
        if (!is.null(minimized_param) && length(minimized_param) > 0 && minimized_param == "cost") {
          return(param_name %in% c("TPM_threshold", "minimum_fold_change"))
        }
      }
    }
    
    return(is_in_row2 && is_not_minimized)
  }
  
  return(FALSE)
}

#' Get minimized parameter for display checking (same logic as mod_parameter_sliders.R)
#'
#' @param workflow_id Workflow ID
#' @return Minimized parameter name
#' @noRd
get_minimized_parameter_for_display <- function(workflow_id) {
  minimized_map <- list(
    # Power-only workflows
    "power_single_cells_per_target" = "cells_per_target",
    "power_single_reads_per_cell" = "reads_per_cell", 
    "power_single_TPM_threshold" = "TPM_threshold",
    "power_single_minimum_fold_change" = "minimum_fold_change",
    
    # Cost minimization workflow
    "power_cost_minimization" = "cost",
    
    # Power+cost workflows
    "power_cost_TPM_cells" = "TPM_threshold",
    "power_cost_TPM_reads" = "TPM_threshold",
    "power_cost_fc_cells" = "minimum_fold_change",
    "power_cost_fc_reads" = "minimum_fold_change"
  )
  
  return(minimized_map[[workflow_id]] %||% character(0))
}

# ============================================================================
# SOLUTION DISPLAY HELPER FUNCTIONS (Legacy - for backward compatibility)
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
  } else if (minimizing_param %in% c("reads_per_cell", "sequenced_reads_per_cell")) {
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
  excluded_params <- c("reads_per_cell", "sequenced_reads_per_cell", "cost")
  
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
