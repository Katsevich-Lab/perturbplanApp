# Real-Time Analysis Feature - Manual Testing Checklist

## Pre-Testing Setup
- [ ] Start app with `golem::run_dev()`
- [ ] Verify app loads without errors
- [ ] Check console for any warnings

## 1. Workflow Scenario Testing (8 workflows)

### Power-Only Workflows (4 tests)

#### 1.1 Minimize Cells per Target
- [ ] Select "Constrain power only"
- [ ] Select "Cells per target" as minimization target
- [ ] Verify sliders are **hidden**
- [ ] Click "Plan" button
- [ ] Verify "Optimization mode activated!" notification appears
- [ ] Verify sliders + pinning buttons appear
- [ ] Drag any slider (e.g., MOI)
- [ ] Verify "Updating analysis..." notification appears
- [ ] Verify plot updates automatically (no manual Plan click needed)
- [ ] Verify pinning buttons work (pin current solution)

#### 1.2 Minimize Reads per Cell  
- [ ] Repeat above steps for "Reads per cell" target
- [ ] Verify different sliders are shown based on workflow

#### 1.3 Minimize TPM Threshold
- [ ] Repeat above steps for "TPM analysis threshold" target
- [ ] Verify TPM-specific parameters in sliders

#### 1.4 Minimize Fold Change
- [ ] Repeat above steps for "Fold change" target
- [ ] Verify fold change range adjusts based on side parameter

### Power+Cost Workflows (4 tests)

#### 1.5 Minimize TPM with Cells Varying
- [ ] Select "Constrain power and cost"
- [ ] Enter cost budget (e.g., $10,000)
- [ ] Select "TPM analysis threshold" as target
- [ ] Set cells to "Varying", reads to "Fixed" in Step 3
- [ ] Follow same plan/slider/real-time testing as above

#### 1.6 Minimize TPM with Reads Varying
- [ ] Same as 1.5 but set reads to "Varying", cells to "Fixed"

#### 1.7 Minimize FC with Cells Varying
- [ ] Select "Fold change" as target  
- [ ] Set cells to "Varying", reads to "Fixed"
- [ ] Follow same testing pattern

#### 1.8 Minimize FC with Reads Varying
- [ ] Select "Fold change" as target
- [ ] Set reads to "Varying", cells to "Fixed"
- [ ] Follow same testing pattern

## 2. Design Problem Change Testing

### 2.1 Optimization Type Changes
- [ ] Start with "Power only" + configured workflow
- [ ] Click Plan, verify sliders appear
- [ ] Change to "Power + cost"
- [ ] Verify "Design problem changed" notification
- [ ] Verify sliders disappear
- [ ] Verify real-time mode resets

### 2.2 Minimization Target Changes
- [ ] Configure "Power only" + "Cells per target"
- [ ] Click Plan, verify sliders appear
- [ ] Change target to "TPM threshold"
- [ ] Verify state resets correctly

### 2.3 Parameter Control Changes
- [ ] Configure power+cost workflow
- [ ] Set specific varying/fixed parameters
- [ ] Click Plan, verify sliders appear
- [ ] Change parameter controls (varying ↔ fixed)
- [ ] Verify state resets

## 3. Performance Testing

### 3.1 Debouncing Effectiveness
- [ ] Enter real-time mode (Plan clicked, sliders visible)
- [ ] Rapidly drag a slider back and forth
- [ ] Verify analysis doesn't trigger on every movement
- [ ] Verify analysis triggers ~500ms after stopping
- [ ] Check console for excessive trigger messages

### 3.2 Rapid Parameter Changes  
- [ ] Quickly change multiple sliders in sequence
- [ ] Verify UI remains responsive
- [ ] Verify analysis triggers appropriately
- [ ] Check for any lag or freezing

### 3.3 Memory and Performance
- [ ] Leave app in real-time mode for extended period
- [ ] Make periodic slider changes
- [ ] Monitor browser memory usage (dev tools)
- [ ] Verify no memory leaks or excessive resource usage

## 4. User Experience Testing

### 4.1 Notification System
- [ ] Verify "Design problem changed" notifications appear correctly
- [ ] Verify "Optimization mode activated!" on first plan click
- [ ] Verify "Updating analysis..." during slider changes
- [ ] Check notification timing and visibility

### 4.2 Loading States
- [ ] Verify smooth transitions between states
- [ ] Check for any flickering or jarring changes
- [ ] Verify loading indicators appear during analysis

### 4.3 Visual Feedback
- [ ] Verify slider values update in real-time
- [ ] Verify plots update smoothly (no abrupt changes)
- [ ] Verify pinning functionality works correctly

## 5. Error Handling Testing

### 5.1 Invalid Parameter Combinations
- [ ] Try to set invalid parameter ranges
- [ ] Verify graceful error handling
- [ ] Verify recovery from error states

### 5.2 Analysis Failures
- [ ] Simulate analysis errors (if possible)
- [ ] Verify error messages appear appropriately
- [ ] Verify app doesn't crash

### 5.3 Edge Cases
- [ ] Test with minimum parameter values
- [ ] Test with maximum parameter values  
- [ ] Test rapid mode switching
- [ ] Test browser refresh during real-time mode

## 6. Integration Testing

### 6.1 Multi-Curve Functionality
- [ ] Enter real-time mode
- [ ] Pin multiple solutions with different parameters
- [ ] Verify multi-curve plots work correctly
- [ ] Verify pinned solutions persist during real-time changes

### 6.2 Export Functionality
- [ ] Generate real-time analysis results
- [ ] Test Excel export functionality
- [ ] Test plot download functionality
- [ ] Verify exported data matches current state

## Testing Results Summary

### Passed Tests: ___/40+

### Failed Tests (if any):
- Test: _______________
  Issue: _______________
  Severity: ____________

### Performance Notes:
- Debouncing effectiveness: _______________
- UI responsiveness: _______________
- Memory usage: _______________

### User Experience Notes:
- Notification clarity: _______________
- Loading states: _______________
- Overall smoothness: _______________

### Recommendations:
- [ ] Ready for production use
- [ ] Minor adjustments needed
- [ ] Major issues require fixes

## Sign-off
- [ ] All critical workflows tested
- [ ] Performance acceptable
- [ ] User experience smooth
- [ ] Error handling robust
- [ ] Feature ready for release