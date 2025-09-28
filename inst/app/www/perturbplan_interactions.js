// ========================================================================
// SIDEBAR SECTION COLLAPSE FUNCTIONALITY
// ========================================================================

function toggleSection(contentId, chevronId) {
  var allContentElements = document.querySelectorAll('[id$="-content"]');
  var allChevronElements = document.querySelectorAll('[id$="-chevron"]');
  var targetContent = document.getElementById(contentId);
  var targetChevron = document.getElementById(chevronId);

  if (!targetContent || !targetChevron) return;

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
  var allContentElements = document.querySelectorAll('[id$="-content"]');
  var allChevronElements = document.querySelectorAll('[id$="-chevron"]');

  allContentElements.forEach(function(section) {
    section.style.display = 'none';
  });

  allChevronElements.forEach(function(chevron) {
    chevron.className = 'fa fa-chevron-right';
  });
}

// ========================================================================
// SIDEBAR COLLAPSE SYSTEM
// ========================================================================

window.SidebarCollapse = {
  isCollapsed: false,
  isAnimating: false,

  init: function() {
    this.bindEvents();
    this.ensureExpandedState();
  },

  ensureExpandedState: function() {
    var sidebar = $('.main-sidebar');
    var contentWrapper = $('.content-wrapper');
    var toggleButton = $('#simple-toggle');

    sidebar.removeClass('collapsed');
    contentWrapper.removeClass('sidebar-collapsed');
    toggleButton.text('◀');
  },

  bindEvents: function() {
    var self = this;
    $(document).on('click', '#simple-toggle', function(e) {
      e.preventDefault();
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

    sidebar.addClass('collapse-transition');
    contentWrapper.addClass('collapse-transition');

    if (shouldCollapse) {
      sidebar.addClass('collapsed');
      contentWrapper.addClass('sidebar-collapsed');
      toggleButton.text('▶');
    } else {
      sidebar.removeClass('collapsed');
      contentWrapper.removeClass('sidebar-collapsed');
      toggleButton.text('◀');
    }

    setTimeout(() => {
      sidebar.removeClass('collapse-transition');
      contentWrapper.removeClass('collapse-transition');
      this.isAnimating = false;
    }, 300);
  }
};

// ========================================================================
// PIN BUTTON FOCUS MANAGEMENT
// ========================================================================

function handlePinButtonClicks() {
  // Multiple targeting strategies with debugging
  console.log('Setting up pin button handlers...');

  // Strategy 1: Target by button text
  $(document).on('click', 'button:contains("Pin Setting"), button:contains("Clear Pins")', function() {
    console.log('Pin button clicked (by text):', this);
    var button = this;
    setTimeout(function() {
      button.blur();
      console.log('Blurred button (by text)');
    }, 10);
  });

  // Strategy 2: Target by class in pin section
  $(document).on('click', '.pin-buttons-section button', function() {
    console.log('Pin button clicked (by section):', this);
    var button = this;
    setTimeout(function() {
      button.blur();
      console.log('Blurred button (by section)');
    }, 10);
  });

  // Strategy 3: Target any button with specific IDs
  $(document).on('click', '[id*="pin_solution"], [id*="clear_pins"]', function() {
    console.log('Pin button clicked (by ID):', this);
    var button = this;
    setTimeout(function() {
      button.blur();
      console.log('Blurred button (by ID)');
    }, 10);
  });

  // Strategy 4: Use longer delay in case Shiny is slow
  $(document).on('click', '.pin-buttons-section .btn', function() {
    console.log('Pin button clicked (main handler):', this);
    var button = this;
    setTimeout(function() {
      button.blur();
      console.log('Blurred button (main handler)');
    }, 100); // Longer delay
  });
}

// ========================================================================
// INITIALIZATION
// ========================================================================

$(document).ready(function() {
  setTimeout(function() {
    // Initialize sidebar collapse system
    window.SidebarCollapse.init();

    // Initialize pin button focus management
    handlePinButtonClicks();

    // Open Design Options section by default
    var firstContent = document.querySelector('[id*="design-content"]');
    var firstChevron = document.querySelector('[id*="design-chevron"]');

    if (firstContent && firstChevron) {
      firstContent.style.display = 'block';
      firstChevron.className = 'fa fa-chevron-down';
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
  }, 1000);
});