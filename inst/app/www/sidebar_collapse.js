/**
 * Sidebar Collapse System - JavaScript Integration
 * Handles smooth sidebar collapse/expand animations and Shiny communication
 */

$(document).ready(function() {
  
  // ========================================================================
  // SIDEBAR COLLAPSE CORE FUNCTIONALITY
  // ========================================================================
  
  /**
   * Global sidebar collapse state management
   */
  window.SidebarCollapse = {
    isCollapsed: false,
    isAnimating: false,
    autoCollapseEnabled: true,
    
    // DOM element references
    sidebar: null,
    contentWrapper: null,
    toggleButton: null,
    
    // Initialize the sidebar collapse system
    init: function() {
      try {
        this.cacheDOMElements();
        
        // Validate DOM elements exist
        if (!this.validateDOM()) {
          return; // Error already handled in validateDOM
        }
        
        this.bindEvents();
        this.setupInitialState();
        console.log('Sidebar collapse system initialized');
      } catch (error) {
        this.handleError(error, 'initialization');
      }
    },
    
    // Cache DOM elements for performance
    cacheDOMElements: function() {
      this.sidebar = $('.main-sidebar');
      this.contentWrapper = $('.content-wrapper');
      this.toggleButton = $('.sidebar-toggle-btn, .floating-sidebar-toggle');
      
      // Create floating toggle button if it doesn't exist
      if ($('.floating-sidebar-toggle').length === 0) {
        this.createFloatingToggleButton();
      }
    },
    
    // Bind event handlers
    bindEvents: function() {
      var self = this;
      
      // Toggle button click handler
      $(document).on('click', '.sidebar-toggle-btn, .floating-sidebar-toggle, .sidebar-collapse-control, #floating-sidebar-toggle, #simple-toggle', function(e) {
        e.preventDefault();
        console.log('Toggle button clicked!');
        self.toggle();
      });
      
      // Handle clicks on the CSS pseudo-element toggle (left edge area when collapsed)
      $(document).on('click', function(e) {
        // Check if click is in the left edge area when sidebar is collapsed
        if (self.isCollapsed && e.pageX <= 25 && e.pageY >= ($(window).height() / 2 - 30) && e.pageY <= ($(window).height() / 2 + 30)) {
          e.preventDefault();
          self.expand();
        }
      });
      
      // Listen for Shiny-triggered collapse events
      $(document).on('sidebar:collapse', function(e, shouldCollapse) {
        self.setCollapsed(shouldCollapse);
      });
      
      // Window resize handler for responsive behavior
      $(window).on('resize', function() {
        self.handleResize();
      });
      
      // ESC key to expand collapsed sidebar
      $(document).on('keydown', function(e) {
        if (e.key === 'Escape' && self.isCollapsed) {
          self.expand();
        }
      });
      
      // Listen for Shiny DOM changes that might affect sidebar functionality
      $(document).on('shiny:inputchanged', function(event) {
        // Reinitialize sidebar after mode changes that affect DOM
        if (event.name === 'sidebar-design_options-optimization_type') {
          setTimeout(() => {
            self.reinitialize();
          }, 200); // Wait for DOM changes to complete
        }
      });
    },
    
    // Setup initial state based on screen size and preferences
    setupInitialState: function() {
      // Check if mobile device
      if (window.innerWidth <= 768) {
        this.setCollapsed(true, false); // Collapse on mobile, no animation
      } else {
        this.setCollapsed(false, false); // Expand on desktop, no animation
      }
    },
    
    // Reinitialize sidebar system after DOM changes
    reinitialize: function() {
      try {
        console.log('Reinitializing sidebar collapse system after DOM changes');
        
        // Recache DOM elements in case they've changed
        this.cacheDOMElements();
        
        // Validate DOM elements still exist
        if (!this.validateDOM()) {
          console.warn('DOM validation failed during reinitialize');
          return;
        }
        
        // Preserve current state but refresh DOM references
        var currentlyCollapsed = this.isCollapsed;
        
        // Re-apply current state to refresh DOM classes
        if (currentlyCollapsed) {
          this.performCollapse();
        } else {
          this.performExpand();
        }
        
        console.log('Sidebar collapse system reinitialized successfully');
      } catch (error) {
        this.handleError(error, 'reinitialize');
      }
    },
    
    // Main toggle function
    toggle: function() {
      if (this.isAnimating) return;
      this.setCollapsed(!this.isCollapsed);
    },
    
    // Collapse sidebar
    collapse: function() {
      this.setCollapsed(true);
    },
    
    // Expand sidebar
    expand: function() {
      this.setCollapsed(false);
    },
    
    // Set collapsed state with animation
    setCollapsed: function(shouldCollapse, animate = true) {
      try {
        if (this.isAnimating) return;
        if (this.isCollapsed === shouldCollapse) return;
        
        // Validate DOM elements before operation
        if (!this.validateDOM()) {
          return;
        }
        
        this.isCollapsed = shouldCollapse;
        this.isAnimating = animate;
        
        if (animate) {
          this.sidebar.addClass('collapse-transition');
          this.contentWrapper.addClass('collapse-transition');
        }
        
        if (shouldCollapse) {
          this.performCollapse();
        } else {
          this.performExpand();
        }
        
        // Update toggle button visibility
        this.updateToggleButtonVisibility();
        
        // Notify Shiny of state change
        this.notifyShiny(shouldCollapse);
        
        // Clear animation state after transition completes
        if (animate) {
          setTimeout(() => {
            try {
              this.sidebar.removeClass('collapse-transition');
              this.contentWrapper.removeClass('collapse-transition');
              this.isAnimating = false;
            } catch (error) {
              this.handleError(error, 'animation_cleanup');
            }
          }, 300); // Match CSS transition duration
        } else {
          this.isAnimating = false;
        }
      } catch (error) {
        this.handleError(error, 'toggle');
      }
    },
    
    // Perform collapse animation
    performCollapse: function() {
      this.sidebar.addClass('collapsed');
      this.contentWrapper.addClass('sidebar-collapsed');
      
      // Fade out sidebar content
      this.sidebar.find('.sidebar-content, .scrollable-sidebar').css('opacity', '0');
      
      // Show simple toggle button
      $('#simple-toggle').show().css('display', 'flex');
      console.log('Sidebar collapsed - showing toggle button');
    },
    
    // Perform expand animation
    performExpand: function() {
      this.sidebar.removeClass('collapsed');
      this.contentWrapper.removeClass('sidebar-collapsed');
      
      // Fade in sidebar content with slight delay
      setTimeout(() => {
        this.sidebar.find('.sidebar-content, .scrollable-sidebar').css('opacity', '1');
      }, 150);
      
      // Hide simple toggle button
      $('#simple-toggle').hide();
      console.log('Sidebar expanded - hiding toggle button');
    },
    
    // Update toggle button visibility and icons
    updateToggleButtonVisibility: function() {
      var toggleIcon = $('.sidebar-toggle-icon');
      
      if (this.isCollapsed) {
        toggleIcon.addClass('collapsed');
        $('.sidebar-toggle-btn').addClass('visible');
      } else {
        toggleIcon.removeClass('collapsed');
        $('.sidebar-toggle-btn').removeClass('visible');
      }
    },
    
    // Create floating toggle button dynamically
    createFloatingToggleButton: function() {
      var floatingButton = $('<button>')
        .addClass('floating-sidebar-toggle')
        .attr('title', 'Show Sidebar')
        .html('<i class="fa fa-bars"></i>');
      
      $('body').append(floatingButton);
      this.toggleButton = this.toggleButton.add(floatingButton);
    },
    
    // Handle window resize for responsive behavior
    handleResize: function() {
      try {
        var isMobile = window.innerWidth <= 768;
        
        if (isMobile && !this.isCollapsed) {
          // Auto-collapse on mobile
          this.setCollapsed(true, false);
        } else if (!isMobile && this.isCollapsed && window.innerWidth > 1024) {
          // Auto-expand on large screens if user preference allows
          if (this.autoCollapseEnabled) {
            this.setCollapsed(false, false);
          }
        }
      } catch (error) {
        this.handleError(error, 'resize');
      }
    },
    
    // Notify Shiny of collapse state changes
    notifyShiny: function(isCollapsed) {
      if (window.Shiny && window.Shiny.setInputValue) {
        Shiny.setInputValue('sidebar_collapsed', isCollapsed, {priority: 'event'});
        
        // Also trigger custom event for other components
        $(document).trigger('sidebar:stateChanged', [isCollapsed]);
      }
    },
    
    // Set auto-collapse preference
    setAutoCollapse: function(enabled) {
      this.autoCollapseEnabled = enabled;
      if (window.Shiny && window.Shiny.setInputValue) {
        Shiny.setInputValue('auto_collapse_enabled', enabled, {priority: 'event'});
      }
    },
    
    // Get current state
    getState: function() {
      return {
        isCollapsed: this.isCollapsed,
        isAnimating: this.isAnimating,
        autoCollapseEnabled: this.autoCollapseEnabled
      };
    },
    
    // Error handling for sidebar operations
    handleError: function(error, context) {
      console.error('Sidebar collapse error:', error, context);
      
      // Reset animation state if stuck
      if (this.isAnimating) {
        this.isAnimating = false;
        this.sidebar.removeClass('collapse-transition');
        this.contentWrapper.removeClass('collapse-transition');
      }
      
      // Notify Shiny if available
      if (window.Shiny && window.Shiny.setInputValue) {
        try {
          Shiny.setInputValue('sidebar_error', {
            message: error.message || error,
            context: context,
            timestamp: Date.now()
          }, {priority: 'event'});
        } catch (e) {
          console.error('Failed to notify Shiny of sidebar error:', e);
        }
      }
      
      // Show user notification
      this.showErrorNotification(error, context);
    },
    
    // Show error notification to user
    showErrorNotification: function(error, context) {
      var message = 'Sidebar operation failed';
      if (context === 'resize') {
        message = 'Sidebar resize failed - please refresh page';
      } else if (context === 'toggle') {
        message = 'Sidebar toggle failed - trying to reset';
      }
      
      // Create temporary notification
      var notification = $('<div class="sidebar-error-notification">')
        .text(message)
        .css({
          position: 'fixed',
          top: '20px',
          right: '20px',
          background: '#C85A5A',
          color: 'white',
          padding: '10px 15px',
          borderRadius: '4px',
          zIndex: '9999',
          fontSize: '14px'
        });
      
      $('body').append(notification);
      
      // Auto-remove after 5 seconds
      setTimeout(() => {
        notification.fadeOut(300, () => notification.remove());
      }, 5000);
    },
    
    // Validate DOM elements exist
    validateDOM: function() {
      var issues = [];
      
      if (this.sidebar.length === 0) {
        issues.push('Main sidebar element not found');
      }
      
      if (this.contentWrapper.length === 0) {
        issues.push('Content wrapper element not found');
      }
      
      if (issues.length > 0) {
        this.handleError(new Error('DOM validation failed: ' + issues.join(', ')), 'initialization');
        return false;
      }
      
      return true;
    }
  };
  
  // ========================================================================
  // SHINY INTEGRATION & MESSAGE HANDLERS
  // ========================================================================
  
  /**
   * Listen for messages from Shiny server
   */
  if (window.Shiny) {
    
    // Handle collapse commands from Shiny
    Shiny.addCustomMessageHandler('toggleSidebar', function(shouldCollapse) {
      window.SidebarCollapse.setCollapsed(shouldCollapse);
    });
    
    // Handle auto-collapse setting updates
    Shiny.addCustomMessageHandler('setAutoCollapse', function(enabled) {
      window.SidebarCollapse.setAutoCollapse(enabled);
    });
    
    // Handle Plan button success - auto collapse if enabled
    Shiny.addCustomMessageHandler('planSuccess', function(data) {
      if (window.SidebarCollapse.autoCollapseEnabled) {
        setTimeout(() => {
          window.SidebarCollapse.collapse();
        }, 500); // Small delay after Plan success
      }
    });
  }
  
  // ========================================================================
  // GLOBAL FUNCTIONS & LEGACY SUPPORT
  // ========================================================================
  
  /**
   * Global toggle function for backward compatibility
   */
  window.toggleSidebar = function(collapse = null) {
    if (collapse === null) {
      window.SidebarCollapse.toggle();
    } else {
      window.SidebarCollapse.setCollapsed(collapse);
    }
    return window.SidebarCollapse.isCollapsed;
  };
  
  /**
   * Initialize the system when DOM is ready
   */
  window.SidebarCollapse.init();
});