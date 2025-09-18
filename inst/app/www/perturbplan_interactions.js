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

function collapseAllSections() {
  // Get all content sections and chevrons
  var allContentElements = document.querySelectorAll('[id$="-content"]');
  var allChevronElements = document.querySelectorAll('[id$="-chevron"]');

  // Close all sections
  allContentElements.forEach(function(section) {
    section.style.display = 'none';
  });

  // Reset all chevrons to right-pointing
  allChevronElements.forEach(function(chevron) {
    chevron.className = 'fa fa-chevron-right';
  });
}

// ========================================================================
// SIDEBAR COLLAPSE SYSTEM - Manual Only
// ========================================================================

window.SidebarCollapse = {
  isCollapsed: false,
  isAnimating: false,

  init: function() {
    this.bindEvents();
    console.log('Simple sidebar toggle initialized');
  },

  bindEvents: function() {
    var self = this;

    // Triangle button toggles sidebar
    $(document).on('click', '#simple-toggle', function(e) {
      e.preventDefault();
      console.log('Triangle button clicked');
      self.toggle();
    });
  },

  toggle: function() {
    if (this.isAnimating) return;
    this.setCollapsed(!this.isCollapsed);
  },

  setCollapsed: function(shouldCollapse) {
    if (this.isAnimating || this.isCollapsed === shouldCollapse) return;

    this.isCollapsed = shouldCollapse;
    this.isAnimating = true;

    var sidebar = $('.main-sidebar');
    var contentWrapper = $('.content-wrapper');
    var toggleButton = $('#simple-toggle');

    // Add transition classes
    sidebar.addClass('collapse-transition');
    contentWrapper.addClass('collapse-transition');

    if (shouldCollapse) {
      // Collapse sidebar
      sidebar.addClass('collapsed');
      contentWrapper.addClass('sidebar-collapsed');
      toggleButton.text('▶'); // Point right when collapsed
      console.log('Sidebar collapsed');
    } else {
      // Expand sidebar
      sidebar.removeClass('collapsed');
      contentWrapper.removeClass('sidebar-collapsed');
      toggleButton.text('◀'); // Point left when expanded
      console.log('Sidebar expanded');
    }

    // Clear animation state after transition completes
    setTimeout(() => {
      sidebar.removeClass('collapse-transition');
      contentWrapper.removeClass('collapse-transition');
      this.isAnimating = false;
    }, 300);
  }
};

// Test basic JavaScript execution
console.log('=== BASIC TEST: JavaScript file is loading ===');

// Initialize on page load
$(document).ready(function() {
  console.log('=== BASIC TEST: jQuery document ready fired ===');
  console.log('PerturbPlan JavaScript loaded');

  // Set initial states after a delay to ensure Shiny is loaded
  setTimeout(function() {
    console.log('=== BASIC TEST: setTimeout fired after 1000ms ===');
    // Initialize sidebar collapse system after DOM is fully ready
    window.SidebarCollapse.init();
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