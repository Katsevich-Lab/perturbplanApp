#' The application server-side
#'
#' Clean 3-module architecture: Input → Analysis → Plotting → Display
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @importFrom shiny bindCache downloadHandler renderUI req
#' @importFrom magrittr %>%
#' @importFrom openxlsx write.xlsx
#' @importFrom ggplot2 ggsave ggplot annotate theme_void
#' @importFrom digest digest
#' @noRd
app_server <- function(input, output, session) {
  
  # ========================================================================
  # MODULE 0: CENTRAL PARAMETER MANAGEMENT
  # ========================================================================
  # Initialize the central parameter manager (single source of truth)
  param_manager <- mod_parameter_manager_server("param_manager")
  
  # ========================================================================
  # PLAN STATE MANAGEMENT - Real-time Analysis & UI Control
  # ========================================================================
  # Comprehensive state management for analysis workflow and UI interactions
  # Tracks design problem structure, plan button clicks, slider visibility, and sidebar collapse
  plan_state <- reactiveValues(
    real_time_enabled = FALSE,       # Is real-time analysis active
    sliders_visible = FALSE,         # Should sliders be visible in UI (set when Plan clicked)
    current_design_signature = NULL, # Signature of current design problem structure
    last_analysis_completed = NULL,  # Timestamp when last analysis completed
    
    # Sidebar collapse state management
    sidebar_collapsed = FALSE,       # Current sidebar visibility state (TRUE = collapsed/hidden)
    
    # Track actual user Plan button clicks for auto-collapse
    waiting_for_plan_result = FALSE,  # Whether we're waiting for Plan-triggered analysis
    has_plan_been_clicked = FALSE     # Whether Plan has been clicked for current config
  )
  
  # Helper functions: Sidebar collapse state management
  
  # Toggle sidebar collapsed state
  # @param collapse: NULL (toggle), TRUE (collapse), FALSE (expand)
  # @return: New collapse state (TRUE/FALSE)
  toggle_sidebar_collapse <- function(collapse = NULL) {
    if (is.null(collapse)) {
      collapse <- !plan_state$sidebar_collapsed
    }
    plan_state$sidebar_collapsed <- collapse
    return(collapse)
  }
  
  # Handle automatic sidebar collapse after successful Plan execution
  handle_auto_collapse <- function() {
    # Update internal state
    toggle_sidebar_collapse(TRUE)
    
    # Send collapse command to client with delay for smooth UX
    session$sendCustomMessage(
      type = "plan_success_collapse",
      message = list(
        delay = 500,
        showProgress = TRUE
      )
    )
  }
  
  # Helper function: Create design problem signature from ALL sidebar parameters
  create_design_problem_signature <- function(user_config) {
    if (is.null(user_config)) return(NULL)
    
    # Include ALL sidebar parameters that should trigger complete clearing
    all_sidebar_elements <- list(
      # Design options (Steps 1/2/3) - Structure AND shared parameter values
      design_options = if (!is.null(user_config$design_options)) {
        list(
          optimization_type = user_config$design_options$optimization_type,
          minimization_target = user_config$design_options$minimization_target,
          parameter_controls = if (!is.null(user_config$design_options$parameter_controls)) {
            lapply(user_config$design_options$parameter_controls, function(param) param$type)
          } else NULL,
          # Include shared parameter value
          cost_budget = user_config$design_options$cost_budget,
          # Include parameters that should trigger slider reset when changed
          # Handle NULL values to ensure consistent signature comparison
          target_power = user_config$design_options$target_power %||% 0.8,
          cost_per_cell = user_config$design_options$cost_per_cell %||% "not_applicable",
          cost_per_million_reads = user_config$design_options$cost_per_million_reads %||% "not_applicable"
        )
      } else NULL,
      
      # Experimental setup - ALL parameters (shared + non-shared)
      experimental_setup = if (!is.null(user_config$experimental_setup)) {
        list(
          # Non-shared parameters
          biological_system = user_config$experimental_setup$biological_system,
          pilot_data_choice = user_config$experimental_setup$pilot_data_choice,
          non_targeting_gRNAs = user_config$experimental_setup$non_targeting_gRNAs,
          
          # Shared parameters
          MOI = user_config$experimental_setup$MOI,
          num_targets = user_config$experimental_setup$num_targets,
          gRNAs_per_target = user_config$experimental_setup$gRNAs_per_target,
          cells_fixed = user_config$experimental_setup$cells_fixed,
          sequenced_reads_fixed = user_config$experimental_setup$sequenced_reads_fixed
        )
      } else NULL,
      
      # Analysis choices - ALL parameters (shared + non-shared)
      analysis_choices = if (!is.null(user_config$analysis_choices)) {
        list(
          # Non-shared parameters
          side = user_config$analysis_choices$side,
          gene_list_mode = user_config$analysis_choices$gene_list_mode,
          
          # Shared parameter
          TPM_threshold_fixed = user_config$analysis_choices$TPM_threshold_fixed
        )
      } else NULL,
      
      # Effect sizes - ALL parameters (shared + non-shared)
      effect_sizes = if (!is.null(user_config$effect_sizes)) {
        list(
          # Non-shared parameter
          prop_non_null = user_config$effect_sizes$prop_non_null,
          
          # Shared parameter
          minimum_fold_change_fixed = user_config$effect_sizes$minimum_fold_change_fixed
        )
      } else NULL,
      
      # Advanced choices - ALL parameters (none are shared with sliders)
      advanced_choices = if (!is.null(user_config$advanced_choices)) {
        list(
          gRNA_variability = user_config$advanced_choices$gRNA_variability,
          mapping_efficiency = user_config$advanced_choices$mapping_efficiency,
          control_group = user_config$advanced_choices$control_group,
          fdr_target = user_config$advanced_choices$fdr_target
        )
      } else NULL
    )
    
    # Create signature hash of ALL sidebar elements
    return(digest::digest(all_sidebar_elements, algo = "md5"))
  }
  
  # ========================================================================
  # MODULE 1: INPUT COLLECTION 
  # ========================================================================
  # Collect all user inputs through sidebar with parameter manager and plan state integration
  user_workflow_config <- mod_sidebar_server("sidebar", param_manager, plan_state)
  
  # ========================================================================  
  # MODULE 2: ANALYSIS ENGINE (Perturbplan Integration)
  # ========================================================================
  # Generate real analysis results using perturbplan package functions with real-time triggers
  analysis_results_raw <- mod_analysis_engine_server("analysis", user_workflow_config, param_manager, plan_state)
  
  # Track when Plan analysis completes
  observeEvent(analysis_results_raw(), {
    
    # CRITICAL: Check plan_state flags BEFORE accessing results to prevent race condition
    # If waiting_for_plan_result is FALSE, this is likely a spurious trigger from mode change
    if (!plan_state$waiting_for_plan_result) {
      return()  # Exit early - no Plan button click is pending
    }
    
    results <- analysis_results_raw()
    
    # Only mark completion for successful Plan-triggered analysis
    if (!is.null(results) && is.null(results$error)) {
      config <- user_workflow_config()
      
      
      # Check if this was a user-initiated Plan button click (not configuration change)
      if (plan_state$waiting_for_plan_result) {
        
        plan_state$last_analysis_completed <- Sys.time()
        plan_state$waiting_for_plan_result <- FALSE  # Reset flag
        
        # Trigger auto-collapse after successful Plan analysis ONLY
        handle_auto_collapse()
      }
    }
  })
  
  # ========================================================================
  # MODULE 3: PLOTTING ENGINE (Always Same)
  # ========================================================================
  # Convert analysis data into plot objects
  plot_objects <- mod_plotting_engine_server("plotting", analysis_results_raw)
  
  # ========================================================================
  # MODULE 4: RESULTS DISPLAY (Always Same)  
  # ========================================================================
  # Handle UI presentation of plots and tables with parameter manager and plan state integration
  display_outputs <- mod_results_display_server("display", plot_objects, analysis_results_raw, user_workflow_config, param_manager, NULL, plan_state)
  
  # ========================================================================
  # HEADER EXPORT FUNCTIONALITY
  # ========================================================================
  
  # Header export buttons UI
  output$header_export_buttons <- renderUI({
    # Show export buttons only when results are available
    req(plot_objects(), analysis_results_raw())
    
    plots <- plot_objects()
    results <- analysis_results_raw()
    
    if (!is.null(plots) && !is.null(results) && 
        is.null(plots$error) && is.null(results$error)) {
      
      tags$div(
        style = "display: flex; gap: 8px; align-items: center;",
        downloadButton(
          "header_export_excel",
          "",
          icon = icon("file-excel"),
          class = "btn btn-success btn-sm",
          style = "padding: 4px 8px;",
          title = "Export to Excel"
        ),
        downloadButton(
          "header_export_plot", 
          "",
          icon = icon("image"),
          class = "btn btn-info btn-sm", 
          style = "padding: 4px 8px;",
          title = "Download Plot"
        )
      )
    }
  })
  
  # Header export handlers (reuse logic from results display module)
  output$header_export_excel <- downloadHandler(
    filename = function() {
      paste0("perturbplan_analysis_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".xlsx")
    },
    content = function(file) {
      req(analysis_results_raw(), plot_objects())
      
      results <- analysis_results_raw()
      plots <- plot_objects()
      
      tryCatch({
        # Reuse Excel export logic
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
        
        if (!is.null(results$user_config$cost_info)) {
          excel_data[["Cost_Information"]] <- create_excel_cost_info(results$user_config$cost_info)
        }
        
        openxlsx::write.xlsx(excel_data, file = file)
        
      }, error = function(e) {
        stop(e$message)
      })
    },
    contentType = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
  )
  
  output$header_export_plot <- downloadHandler(
    filename = function() {
      paste0("perturbplan_plot_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")
    },
    content = function(file) {
      req(plot_objects())
      
      plots <- plot_objects()
      
      tryCatch({
        if (!is.null(plots$plots$main_plot)) {
          ggplot2::ggsave(
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
        # Create error plot fallback
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
  
  # ========================================================================
  # APP STATE MANAGEMENT
  # ========================================================================
  
  # Monitor for ANY sidebar configuration changes (complete clearing trigger)
  observe({
    current_config <- user_workflow_config()
    new_signature <- create_design_problem_signature(current_config)
    
    # Check the source of the change before resetting state
    recent_slider_change <- if (!is.null(current_config) && 
                               !is.null(current_config$last_parameter_source) &&
                               !is.null(current_config$last_parameter_timestamp) &&
                               current_config$last_parameter_source == "slider") {
      difftime(Sys.time(), current_config$last_parameter_timestamp, units = "secs") < 1
    } else {
      FALSE
    }
    
    # Check if core design structure changed
    if (!is.null(new_signature) && 
        !identical(plan_state$current_design_signature, new_signature) &&
        !recent_slider_change) {  # Don't reset for recent slider changes
      
      # Sidebar configuration changed - reset state completely
      old_signature <- plan_state$current_design_signature
      plan_state$current_design_signature <- new_signature
      plan_state$real_time_enabled <- FALSE
      plan_state$sliders_visible <- FALSE
      
      # Reset sidebar collapse state when configuration changes
      plan_state$sidebar_collapsed <- FALSE
      
      # Reset Plan button tracking to prevent inappropriate auto-collapse
      plan_state$waiting_for_plan_result <- FALSE
      plan_state$has_plan_been_clicked <- FALSE
      plan_state$reset_plan_state <- TRUE  # Signal analysis engine to reset tracking
      
      # Send sidebar state update to client
      session$sendCustomMessage(
        type = "sidebar_collapse_state",
        message = list(
          collapsed = FALSE,
          animate = FALSE
        )
      )
      
      # Clear pinned solutions from previous design problem
      # Note: pinned_solutions will be handled by results_display module
      
      # Notify user only if we had a previous valid design problem
      if (!is.null(old_signature)) {
      }
    }
  })
  
  # ========================================================================
  # SIDEBAR COLLAPSE SYSTEM
  # ========================================================================
  
  # Handle sidebar state changes from client
  observeEvent(input$sidebar_state_changed, {
    if (!is.null(input$sidebar_state_changed)) {
      plan_state$sidebar_collapsed <- input$sidebar_state_changed$collapsed
    }
  })
  
  # Handle sidebar error reports from client
  observeEvent(input$sidebar_error, {
    if (!is.null(input$sidebar_error)) {
      warning("Sidebar error: ", input$sidebar_error$message, 
              " Context: ", input$sidebar_error$context)
    }
  })
  
  # Handle floating sidebar toggle button (handled by JavaScript, but track state)
  observeEvent(input$`floating-sidebar-toggle`, {
    # This will be handled primarily by JavaScript
    plan_state$sidebar_collapsed <- FALSE
    
    session$sendCustomMessage(
      type = "sidebar_collapse_state", 
      message = list(
        collapsed = FALSE,
        animate = TRUE
      )
    )
  })
  
  # Combined observer for loading states and error handling
  observe({
    config <- user_workflow_config()
    analysis <- analysis_results_raw()
    
    # Handle loading states
    if (plan_state$waiting_for_plan_result && is.null(analysis)) {
      # Show loading notification when Plan clicked but analysis not ready
    }
    
    # Handle analysis errors  
    if (!is.null(analysis) && !is.null(analysis$error)) {
    }
  })
  
  # Development debug output disabled to reduce console output
}
