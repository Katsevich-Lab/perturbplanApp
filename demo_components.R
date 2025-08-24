# Phase 2 Component Library Demo
# This file demonstrates the new UI components that replace inline styles
# with semantic CSS classes from the Phase 1 foundation

library(shiny)
library(perturbplanApp)

# Example usage of the new components
demo_ui <- function() {
  ns <- NS("demo")
  
  fluidPage(
    h1("Phase 2: Component Library Demo"),
    
    # 1. Collapsible Section Component
    h3("1. Collapsible Section"),
    collapsible_section(
      ns, "section1", "Design Options",
      tags$div(
        p("This replaces the complex nested div structure with onclick handlers."),
        p("Instead of:"),
        tags$code('tags$div(onclick = paste0("toggleSection(...)"), ...)'),
        p("We now use:"),
        tags$code('collapsible_section(ns, "id", "Title", content)')
      )
    ),
    
    # 2. Parameter Input Groups
    h3("2. Parameter Input Groups"),
    tags$div(
      class = "input-row",  # Uses our CSS foundation
      parameter_input_group(
        ns, "power", "Target Power:",
        numericInput(ns("power"), NULL, value = 0.8, min = 0.1, max = 0.99),
        help_text = "Power between 0.1 and 0.99"
      ),
      parameter_input_group(
        ns, "budget", "Budget:",
        numericInput(ns("budget"), NULL, value = 10000),
        help_text = "Total budget in USD"
      )
    ),
    
    # 3. Currency Inputs
    h3("3. Currency Inputs"),
    tags$div(
      class = "input-row",
      currency_input(ns, "cost_cell", "Cost per cell:", value = 0.05),
      currency_input(ns, "cost_read", "Cost per read:", value = 0.001)
    ),
    
    # 4. Step Container
    h3("4. Step Containers"),
    step_container(
      1, "Optimization Constraints",
      selectInput(ns("opt_type"), "Constraint type:",
        choices = list("Power only" = "power", "Power + cost" = "both")
      )
    ),
    
    step_container(
      2, "Parameter Selection", 
      p("This creates consistent multi-step forms with proper dividers."),
      show_divider = FALSE
    ),
    
    # 5. Parameter Matrix
    h3("5. Parameter Matrix"),
    p("Complex parameter control matrix with business logic:"),
    parameter_matrix(
      ns,
      list(
        cells_per_target = "varying",
        mapped_reads_per_cell = "fixed", 
        TPM_threshold = "minimizing"
      )
    ),
    
    # 6. File Upload Zone
    h3("6. File Upload Zone"),
    file_upload_zone(
      ns, "data_file",
      accept = c(".rds", ".RDS"),
      info_text = "Combined RDS file with baseline expression and library parameters",
      placeholder = "Choose reference expression data..."
    ),
    
    hr(),
    
    h2("Benefits of Component Library"),
    tags$ul(
      tags$li("Replaces 283+ inline style instances with semantic CSS classes"),
      tags$li("Consistent behavior across all modules"),
      tags$li("Reusable components reduce code duplication"),
      tags$li("Easy maintenance and styling updates"),
      tags$li("Proper accessibility and responsive design")
    )
  )
}

# To test this demo:
# shinyApp(ui = demo_ui(), server = function(input, output, session) {})