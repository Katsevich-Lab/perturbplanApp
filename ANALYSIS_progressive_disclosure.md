# Progressive Disclosure Observer Chain Analysis

## Phase 1.1: Current Observer Architecture Analysis

### **Observer Block 1: Basic Progressive Disclosure (Lines 135-151)**

**Purpose:** Initial step progression and cascade hiding

**Triggers:**
- `input$optimization_type` changes

**Conditions:**
```r
if (!is.null(input$optimization_type) && input$optimization_type != "")
```

**Side Effects:**
- `shinyjs::show("power_cost_inputs")` - Show power/cost section
- If `optimization_type == "power_cost"`:
  - `shinyjs::show("cost_budget_div")` - Show cost budget input
- Else:
  - `shinyjs::hide("cost_budget_div")` - Hide cost budget input
- If condition fails (no optimization type):
  - `shinyjs::hide("power_cost_inputs")` - Hide power/cost section
  - `shinyjs::hide("step2")` - Cascade hide Step 2
  - `shinyjs::hide("step3")` - Cascade hide Step 3

### **Observer Block 2: Input Reset (Lines 154-160)**

**Purpose:** Reset inputs when optimization type changes

**Triggers:**
- `input$optimization_type` changes (implicit dependency)

**Side Effects:**
- `updateNumericInput(session, "target_power", value = 0.8)`
- `updateNumericInput(session, "cost_budget", value = 10000)`

### **Observer Block 3: Step 2 Visibility (Lines 187-205)**

**Purpose:** Show Step 2 when Step 1 complete and inputs ready

**Triggers:**
- `input$optimization_type`
- `input$target_power`
- `input$cost_budget`

**Conditions:**
```r
step1_complete = !is.null(input$optimization_type) && input$optimization_type != ""
power_ready = !is.null(input$target_power) && is.numeric(input$target_power) && input$target_power > 0
cost_ready = TRUE (default)
if (optimization_type == "power_cost"):
  cost_ready = !is.null(input$cost_budget) && is.numeric(input$cost_budget) && input$cost_budget > 0
```

**Side Effects:**
- If `step1_complete && power_ready && cost_ready`:
  - `shinyjs::show("step2")`
- Else:
  - `shinyjs::hide("step2")`
  - `shinyjs::hide("step3")` - Cascade hide Step 3

### **Observer Block 4: Step 3 Visibility (Lines 207-244)**

**Purpose:** Show Step 3 when minimization target selected and has controls

**Triggers:**
- `input$minimization_target`
- `input$optimization_type` (indirectly via param_configs)

**Conditions:**
```r
target_selected = !is.null(input$minimization_target) && input$minimization_target != ""
valid_types = !is.null(opt_type) && opt_type != "" && !is.null(target) && target != ""
has_controls = any varying parameters in param_configs
```

**Side Effects:**
- If conditions met and `has_controls`:
  - `shinyjs::show("step3")`
- Else:
  - `shinyjs::hide("step3")`
- If `minimization_target == "cost"`:
  - `shinyjs::show("cost_minimization_params")`
- Else:
  - `shinyjs::hide("cost_minimization_params")`
- If target not selected:
  - `shinyjs::hide("step3")`
  - `shinyjs::hide("cost_minimization_params")`

### **Observer Block 5: Design Summary (Lines 247-289)**

**Purpose:** Show summary when all steps complete

**Triggers:**
- `input$cells_per_target_control`
- `input$reads_per_cell_control`
- `input$TPM_control`
- `input$fc_control`
- `input$optimization_type`
- `input$minimization_target`
- `input$target_power`

**Conditions:**
```r
all_inputs_ready = !is.null(input$optimization_type) && input$optimization_type != "" &&
                   !is.null(input$minimization_target) && input$minimization_target != "" &&
                   !is.null(input$target_power) && input$target_power > 0
step3_visible = !is.null(input$minimization_target) && input$minimization_target != ""
```

**Side Effects:**
- If `all_inputs_ready && step3_visible`:
  - Generate summary text with `generate_design_summary()`
  - `shinyjs::html("summary_text", summary_text)`
  - `shinyjs::show("design_summary")`
- Else:
  - `shinyjs::hide("design_summary")`

## Phase 1.1: Dependencies & Side Effects Summary

### **All Reactive Input Dependencies:**
1. `input$optimization_type` - Used in blocks 1, 2, 3, 4, 5
2. `input$target_power` - Used in blocks 3, 5
3. `input$cost_budget` - Used in blocks 3
4. `input$minimization_target` - Used in blocks 4, 5
5. `input$cells_per_target_control` - Used in block 5
6. `input$reads_per_cell_control` - Used in block 5
7. `input$TPM_control` - Used in block 5
8. `input$fc_control` - Used in block 5

### **All UI Elements Controlled:**
1. `"power_cost_inputs"` - Show/hide
2. `"cost_budget_div"` - Show/hide
3. `"step2"` - Show/hide
4. `"step3"` - Show/hide
5. `"cost_minimization_params"` - Show/hide
6. `"design_summary"` - Show/hide
7. `"summary_text"` - HTML content update
8. `"target_power"` - Input value reset
9. `"cost_budget"` - Input value reset

### **Complex Dependencies:**
- **Cascade hiding:** When Step 1 fails, hide Steps 2 & 3
- **Cost budget conditional:** Only required for power_cost optimization
- **Parameter controls:** Step 3 visibility depends on varying parameters
- **Summary generation:** Requires all parameter control inputs

### **Redundant Conditions:**
- `input$minimization_target` null check repeated in blocks 4 & 5
- `input$optimization_type` validation repeated across multiple blocks
- Step 3 visibility logic duplicated between block 4 and summary conditions

### **Performance Issues:**
- Block 5 triggers on every parameter control change
- Multiple calls to `get_param_configs()` with same inputs
- No debouncing for rapid input changes

## Phase 1.1: Proposed State Structure

Based on analysis, the unified state should track:

```r
progressive_state <- reactive({
  list(
    # Basic validations
    has_optimization_type = !is.null(input$optimization_type) && input$optimization_type != "",
    has_target_power = !is.null(input$target_power) && is.numeric(input$target_power) && input$target_power > 0,
    has_cost_budget = !is.null(input$cost_budget) && is.numeric(input$cost_budget) && input$cost_budget > 0,
    has_minimization_target = !is.null(input$minimization_target) && input$minimization_target != "",

    # Computed states
    cost_budget_required = input$optimization_type == "power_cost",
    cost_inputs_ready = if (cost_budget_required) has_cost_budget else TRUE,
    step1_complete = has_optimization_type,
    step2_ready = has_optimization_type && has_target_power && cost_inputs_ready,
    step3_ready = has_minimization_target && has_varying_parameters(),
    summary_ready = step2_ready && step3_ready && all_parameter_controls_set(),

    # Special conditions
    show_cost_budget = cost_budget_required,
    show_cost_minimization = input$minimization_target == "cost",

    # Current workflow
    optimization_type = input$optimization_type,
    minimization_target = input$minimization_target
  )
})
```

## Phase 1.2: Helper Function Contracts

### **Validation Functions (Pure Functions)**
| Function | Input Types | Return Type | Purpose |
|----------|-------------|-------------|---------|
| `is_step1_complete(optimization_type)` | Character | Logical | Step 1 validation |
| `is_power_input_ready(target_power)` | Numeric | Logical | Power input validation |
| `is_cost_budget_ready(cost_budget, optimization_type)` | Numeric, Character | Logical | Cost budget validation |
| `is_step2_complete(minimization_target)` | Character | Logical | Step 2 validation |
| `workflow_has_varying_parameters(opt_type, target)` | Character, Character | Logical | Step 3 needs check |
| `are_all_parameter_controls_set(opt_type, target, controls)` | Character, Character, List | Logical | Summary readiness |

### **UI Control Functions (Side Effects)**
| Function | Parameters | Return | Purpose |
|----------|------------|---------|---------|
| `toggle_power_cost_inputs(session, show, opt_type)` | Session, Logical, Character | NULL | Show/hide power section |
| `toggle_step2_section(session, show)` | Session, Logical | NULL | Show/hide Step 2 |
| `toggle_step3_section(session, show)` | Session, Logical | NULL | Show/hide Step 3 |
| `toggle_cost_minimization_params(session, show)` | Session, Logical | NULL | Show/hide cost params |
| `toggle_design_summary(session, show, text)` | Session, Logical, Character | NULL | Show/hide summary |
| `reset_input_values(session)` | Session | NULL | Reset to defaults |

### **Function Dependencies**
- **`workflow_has_varying_parameters()`** → calls `get_param_configs()`
- **`toggle_power_cost_inputs()`** → calls `shinyjs::show/hide` on multiple elements
- **`toggle_step2_section()`** → cascades to hide Step 3
- **All toggle functions** → require `shinyjs` imports

### **State Transition Logic**
```r
# Proposed unified controller structure:
progressive_state <- reactive({
  list(
    step1_complete = is_step1_complete(input$optimization_type),
    power_ready = is_power_input_ready(input$target_power),
    cost_ready = is_cost_budget_ready(input$cost_budget, input$optimization_type),
    step2_complete = is_step2_complete(input$minimization_target),
    step3_has_controls = workflow_has_varying_parameters(input$optimization_type, input$minimization_target),
    all_controls_set = are_all_parameter_controls_set(input$optimization_type, input$minimization_target, input),
    show_cost_budget = input$optimization_type == "power_cost",
    show_cost_minimization = input$minimization_target == "cost"
  )
})

observe({
  state <- progressive_state()

  # Step 1 → Power/Cost section
  toggle_power_cost_inputs(session, state$step1_complete, input$optimization_type)

  # Step 1 + inputs → Step 2
  toggle_step2_section(session, state$step1_complete && state$power_ready && state$cost_ready)

  # Step 2 + has controls → Step 3
  toggle_step3_section(session, state$step2_complete && state$step3_has_controls)

  # Cost minimization parameters
  toggle_cost_minimization_params(session, state$show_cost_minimization)

  # All complete → Summary
  if (state$step2_complete && state$step3_has_controls && state$all_controls_set) {
    summary_text <- generate_design_summary(...)
    toggle_design_summary(session, TRUE, summary_text)
  } else {
    toggle_design_summary(session, FALSE)
  }
})

# Input reset observer (separate)
observe({
  input$optimization_type  # dependency
  reset_input_values(session)
})
```