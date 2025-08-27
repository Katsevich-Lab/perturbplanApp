// PerturbPlan JavaScript - Collapsible sections functionality

function toggleSection(contentId, chevronId) {
  console.log('toggleSection called with:', contentId, chevronId);
  
  // Get all sections that could be in the sidebar (handle namespaced IDs)
  var allContentElements = document.querySelectorAll('[id$="-content"]');
  var allChevronElements = document.querySelectorAll('[id$="-chevron"]');
  
  var targetContent = document.getElementById(contentId);
  var targetChevron = document.getElementById(chevronId);
  
  if (!targetContent || !targetChevron) {
    console.log('Target elements not found:', contentId, chevronId);
    return;
  }
  
  // Close all other sections
  allContentElements.forEach(function(section) {
    if (section.id !== contentId) {
      section.style.display = 'none';
    }
  });
  
  allChevronElements.forEach(function(chevron) {
    if (chevron.id !== chevronId) {
      chevron.className = 'fa fa-chevron-right';
    }
  });
  
  // Toggle the clicked section
  if (targetContent.style.display === 'none' || targetContent.style.display === '') {
    targetContent.style.display = 'block';
    targetChevron.className = 'fa fa-chevron-down';
  } else {
    targetContent.style.display = 'none';
    targetChevron.className = 'fa fa-chevron-right';
  }
}

// Shiny message handlers for optimization mode persistence
Shiny.addCustomMessageHandler('storeOptimizationMode', function(data) {
  console.log('Storing optimization mode:', data.mode);
  sessionStorage.setItem('perturbplan_optimization_mode', data.mode);
});

Shiny.addCustomMessageHandler('reloadAfterDelay', function(data) {
  console.log('Reloading page after delay:', data.delay);
  setTimeout(function() {
    window.location.reload();
  }, data.delay);
});

// Initialize on page load
$(document).ready(function() {
  console.log('PerturbPlan JavaScript loaded');
  
  // Restore optimization mode if stored
  var storedMode = sessionStorage.getItem('perturbplan_optimization_mode');
  if (storedMode) {
    console.log('Restoring optimization mode:', storedMode);
    // Clear the stored mode after use
    sessionStorage.removeItem('perturbplan_optimization_mode');
    
    // Restore both the Shiny input value AND the visual select dropdown selection
    setTimeout(function() {
      // Update the select dropdown selection in the DOM
      var selectElement = document.getElementById('sidebar-design_options-optimization_type');
      if (selectElement) {
        selectElement.value = storedMode;
        console.log('Visually selected dropdown option for:', storedMode);
        
        // Trigger the change event to ensure Shiny processes the selection
        var event = new Event('change', { bubbles: true });
        selectElement.dispatchEvent(event);
      }
      
      // Also send to Shiny to ensure server-side state is updated
      Shiny.setInputValue('sidebar-design_options-optimization_type', storedMode, {priority: 'event'});
    }, 500);
  }
  
  // Set initial states after a delay to ensure Shiny is loaded
  setTimeout(function() {
    // Find the first section (Design Options) and open it by default
    var firstContent = document.querySelector('[id*="design-content"]');
    var firstChevron = document.querySelector('[id*="design-chevron"]');
    
    if (firstContent && firstChevron) {
      firstContent.style.display = 'block';
      firstChevron.className = 'fa fa-chevron-down';
      console.log('Opened Design Options section by default');
    }
    
    // Close all other sections
    var allOtherContent = document.querySelectorAll('[id$="-content"]:not([id*="design-content"])');
    var allOtherChevrons = document.querySelectorAll('[id$="-chevron"]:not([id*="design-chevron"])');
    
    allOtherContent.forEach(function(section) {
      section.style.display = 'none';
    });
    
    allOtherChevrons.forEach(function(chevron) {
      chevron.className = 'fa fa-chevron-right';
    });
    
    console.log('Initial states set for PerturbPlan');
  }, 1000);
});