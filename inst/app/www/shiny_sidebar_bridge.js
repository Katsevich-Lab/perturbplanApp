/**
 * Shiny-JavaScript Communication Bridge for Sidebar Collapse
 * Handles bidirectional communication between Shiny server and client-side animations
 */

$(document).ready(function() {
  
  // ========================================================================
  // SHINY COMMUNICATION BRIDGE
  // ========================================================================
  
  /**
   * Bridge between Shiny reactive system and client-side animations
   */
  window.ShinySidebarBridge = {
    
    // State synchronization
    serverState: {
      collapsed: false,
      autoCollapse: true,
      toggleAvailable: true
    },
    
    // Message queue for reliable communication
    messageQueue: [],
    isProcessing: false,
    
    // Initialize bridge
    init: function() {
      this.setupShinyHandlers();
      this.setupClientHandlers();
      this.setupStateSync();
      console.log('Shiny-Sidebar communication bridge initialized');
    },
    
    // ========================================================================
    // SHINY MESSAGE HANDLERS (Server -> Client)
    // ========================================================================
    
    setupShinyHandlers: function() {
      var self = this;
      
      if (!window.Shiny) {
        console.warn('Shiny not available - running in standalone mode');
        return;
      }
      
      // Handle collapse state changes from server
      Shiny.addCustomMessageHandler('sidebar_collapse_state', function(data) {
        self.handleServerStateUpdate(data);
      });
      
      // Handle auto-collapse after Plan button success
      Shiny.addCustomMessageHandler('plan_success_collapse', function(data) {
        self.handlePlanSuccess(data);
      });
      
      // Handle configuration updates from server
      Shiny.addCustomMessageHandler('sidebar_config_update', function(config) {
        self.updateConfiguration(config);
      });
      
      // Handle forced sidebar state (e.g., during workflow changes)
      Shiny.addCustomMessageHandler('force_sidebar_state', function(data) {
        self.forceSidebarState(data.collapsed, data.animate);
      });
      
      // Handle responsive breakpoint changes
      Shiny.addCustomMessageHandler('responsive_breakpoint', function(data) {
        self.handleResponsiveChange(data);
      });
    },
    
    // ========================================================================
    // CLIENT EVENT HANDLERS (Client -> Server)
    // ========================================================================
    
    setupClientHandlers: function() {
      var self = this;
      
      // Listen for client-side collapse state changes
      $(document).on('sidebar:stateChanged', function(e, isCollapsed) {
        self.notifyServer('sidebar_state_changed', {
          collapsed: isCollapsed,
          timestamp: Date.now(),
          userInitiated: true
        });
      });
      
      // Listen for user preference changes
      $(document).on('sidebar:preferenceChanged', function(e, preferences) {
        self.notifyServer('sidebar_preferences', preferences);
      });
      
      // Listen for animation completion
      $(document).on('sidebar:animationComplete', function(e, details) {
        self.notifyServer('sidebar_animation_complete', details);
      });
      
      // Listen for error events
      $(document).on('sidebar:error', function(e, error) {
        self.notifyServer('sidebar_error', error);
      });
    },
    
    // ========================================================================
    // STATE SYNCHRONIZATION
    // ========================================================================
    
    setupStateSync: function() {
      var self = this;
      
      // Sync state every 5 seconds (fallback)
      setInterval(function() {
        self.syncState();
      }, 5000);
      
      // Sync on visibility change
      document.addEventListener('visibilitychange', function() {
        if (!document.hidden) {
          self.syncState();
        }
      });
      
      // Initial state sync
      setTimeout(() => {
        self.syncState();
      }, 1000);
    },
    
    // Synchronize state between client and server
    syncState: function() {
      if (!window.Shiny || !window.SidebarCollapse) return;
      
      var currentState = window.SidebarCollapse.getState();
      
      // Only sync if state has changed
      if (this.hasStateChanged(currentState)) {
        this.notifyServer('sidebar_state_sync', {
          client_state: currentState,
          server_state: this.serverState,
          sync_timestamp: Date.now()
        });
      }
    },
    
    // Check if state has changed since last sync
    hasStateChanged: function(currentState) {
      return (
        currentState.isCollapsed !== this.serverState.collapsed ||
        currentState.autoCollapseEnabled !== this.serverState.autoCollapse
      );
    },
    
    // ========================================================================
    // MESSAGE HANDLING METHODS
    // ========================================================================
    
    // Handle server state updates
    handleServerStateUpdate: function(data) {
      this.serverState = { ...this.serverState, ...data };
      
      // Apply state changes to client
      if (window.SidebarCollapse) {
        if (data.collapsed !== undefined) {
          window.SidebarCollapse.setCollapsed(data.collapsed, data.animate !== false);
          
          // Control toggle button visibility
          if (data.collapsed) {
            $('#simple-toggle').show().css('display', 'flex');
            console.log('Server state: showing toggle button');
          } else {
            $('#simple-toggle').hide();
            console.log('Server state: hiding toggle button');
          }
        }
        if (data.autoCollapse !== undefined) {
          window.SidebarCollapse.setAutoCollapse(data.autoCollapse);
        }
      }
    },
    
    // Handle Plan button success with auto-collapse
    handlePlanSuccess: function(data) {
      if (!window.SidebarCollapse) return;
      
      // Show progress animation if animations are enabled
      if (window.SidebarAnimations && data.showProgress) {
        window.SidebarAnimations.showPlanProgress();
      }
      
      // Auto-collapse after delay if enabled
      if (window.SidebarCollapse.autoCollapseEnabled) {
        setTimeout(() => {
          window.SidebarCollapse.collapse();
          
          // Show toggle button after collapse
          $('#simple-toggle').show().css('display', 'flex');
          console.log('Plan success: showing toggle button');
          
          // Notify server of auto-collapse completion
          this.notifyServer('auto_collapse_complete', {
            triggered_by: 'plan_success',
            delay: data.delay || 500
          });
        }, data.delay || 500);
      }
    },
    
    // Update client configuration
    updateConfiguration: function(config) {
      if (window.SidebarCollapse) {
        if (config.autoCollapseEnabled !== undefined) {
          window.SidebarCollapse.setAutoCollapse(config.autoCollapseEnabled);
        }
        if (config.toggleAvailable !== undefined) {
          this.setToggleAvailability(config.toggleAvailable);
        }
      }
      
      // Update server state cache
      this.serverState = { ...this.serverState, ...config };
    },
    
    // Force sidebar state (used during workflow transitions)
    forceSidebarState: function(collapsed, animate = true) {
      if (window.SidebarCollapse) {
        window.SidebarCollapse.setCollapsed(collapsed, animate);
        
        // Update server state immediately
        this.serverState.collapsed = collapsed;
        
        // Notify server of forced state completion
        this.notifyServer('forced_state_complete', {
          collapsed: collapsed,
          animated: animate
        });
      }
    },
    
    // Handle responsive breakpoint changes
    handleResponsiveChange: function(data) {
      if (window.SidebarCollapse) {
        // Adjust behavior based on screen size
        if (data.isMobile && !window.SidebarCollapse.isCollapsed) {
          window.SidebarCollapse.collapse();
        } else if (data.isDesktop && data.autoExpand && window.SidebarCollapse.isCollapsed) {
          window.SidebarCollapse.expand();
        }
      }
    },
    
    // ========================================================================
    // UTILITY METHODS
    // ========================================================================
    
    // Send message to Shiny server with queue management
    notifyServer: function(inputId, value) {
      if (!window.Shiny) return;
      
      // Add to message queue
      this.messageQueue.push({ inputId, value, timestamp: Date.now() });
      
      // Process queue
      this.processMessageQueue();
    },
    
    // Process message queue to prevent flooding
    processMessageQueue: function() {
      if (this.isProcessing || this.messageQueue.length === 0) return;
      
      this.isProcessing = true;
      
      // Process messages with small delay to batch them
      setTimeout(() => {
        while (this.messageQueue.length > 0) {
          const message = this.messageQueue.shift();
          try {
            Shiny.setInputValue(message.inputId, message.value, { priority: 'event' });
          } catch (error) {
            console.error('Error sending message to Shiny:', error);
          }
        }
        this.isProcessing = false;
      }, 50);
    },
    
    // Set toggle button availability
    setToggleAvailability: function(available) {
      const toggleButtons = $('.sidebar-toggle-btn, .floating-sidebar-toggle, .sidebar-collapse-control');
      
      if (available) {
        toggleButtons.prop('disabled', false).removeClass('disabled');
      } else {
        toggleButtons.prop('disabled', true).addClass('disabled');
      }
    },
    
    // Error handling
    handleError: function(error, context) {
      console.error('Sidebar bridge error:', error, context);
      
      this.notifyServer('sidebar_error', {
        message: error.message || error,
        context: context,
        timestamp: Date.now(),
        userAgent: navigator.userAgent
      });
    },
    
    // Get bridge status for debugging
    getStatus: function() {
      return {
        initialized: true,
        shinyAvailable: !!window.Shiny,
        sidebarAvailable: !!window.SidebarCollapse,
        animationsAvailable: !!window.SidebarAnimations,
        serverState: this.serverState,
        messageQueueLength: this.messageQueue.length,
        isProcessing: this.isProcessing
      };
    }
  };
  
  // ========================================================================
  // ERROR HANDLING & DEBUGGING
  // ========================================================================
  
  // Global error handler for sidebar-related errors
  window.addEventListener('error', function(e) {
    if (e.message && e.message.includes('sidebar')) {
      window.ShinySidebarBridge.handleError(e.error || e.message, 'global_error');
    }
  });
  
  // Console debugging helper
  window.debugSidebar = function() {
    console.log('Sidebar System Status:');
    console.log('- Bridge:', window.ShinySidebarBridge.getStatus());
    if (window.SidebarCollapse) {
      console.log('- Collapse:', window.SidebarCollapse.getState());
    }
    if (window.SidebarAnimations) {
      console.log('- Animations:', {
        reducedMotion: window.SidebarAnimations.isReducedMotion,
        rafId: window.SidebarAnimations.rafId
      });
    }
  };
  
  // Initialize bridge
  window.ShinySidebarBridge.init();
});