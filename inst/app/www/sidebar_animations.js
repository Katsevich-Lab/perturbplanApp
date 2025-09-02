/**
 * Sidebar Animation Utilities - Performance & Smooth Transitions
 * Advanced animation handling for sidebar collapse system
 */

$(document).ready(function() {
  
  // ========================================================================
  // ANIMATION PERFORMANCE UTILITIES
  // ========================================================================
  
  /**
   * Animation performance optimization utilities
   */
  window.SidebarAnimations = {
    
    // Animation frame management
    rafId: null,
    isReducedMotion: false,
    
    // Initialize animation system
    init: function() {
      this.detectReducedMotion();
      this.setupAnimationObserver();
      this.optimizePerformance();
    },
    
    // Detect user's reduced motion preference
    detectReducedMotion: function() {
      this.isReducedMotion = window.matchMedia('(prefers-reduced-motion: reduce)').matches;
      
      if (this.isReducedMotion) {
        console.log('Reduced motion detected - using simplified animations');
        this.enableReducedMotionMode();
      }
    },
    
    // Setup intersection observer for performance
    setupAnimationObserver: function() {
      if ('IntersectionObserver' in window) {
        this.observer = new IntersectionObserver((entries) => {
          entries.forEach(entry => {
            if (entry.isIntersecting) {
              entry.target.classList.add('animation-ready');
            }
          });
        }, {
          threshold: 0.1,
          rootMargin: '50px'
        });
        
        // Observe sidebar elements
        $('.main-sidebar, .content-wrapper').each((i, el) => {
          this.observer.observe(el);
        });
      }
    },
    
    // Performance optimizations
    optimizePerformance: function() {
      // Enable hardware acceleration
      $('.main-sidebar, .content-wrapper').css({
        'will-change': 'transform, margin-left',
        'transform': 'translateZ(0)', // Force GPU layer
        'backface-visibility': 'hidden' // Prevent flickering
      });
      
      // Optimize during animations
      this.setupAnimationFrameOptimization();
    },
    
    // Animation frame optimization
    setupAnimationFrameOptimization: function() {
      var self = this;
      
      // Throttle resize events
      var resizeTimer;
      $(window).on('resize', function() {
        clearTimeout(resizeTimer);
        resizeTimer = setTimeout(() => {
          self.handleOptimizedResize();
        }, 100);
      });
      
      // Optimize scroll during sidebar animations
      $('.main-sidebar').on('scroll', function() {
        if (window.SidebarCollapse.isAnimating) {
          // Disable scroll during animation for performance
          this.scrollTop = this.scrollTop;
        }
      });
    },
    
    // Optimized resize handler
    handleOptimizedResize: function() {
      if (this.rafId) {
        cancelAnimationFrame(this.rafId);
      }
      
      this.rafId = requestAnimationFrame(() => {
        window.SidebarCollapse.handleResize();
        this.rafId = null;
      });
    },
    
    // Reduced motion mode
    enableReducedMotionMode: function() {
      $('body').addClass('reduced-motion');
      
      // Override CSS transitions for reduced motion
      $('<style>')
        .prop('type', 'text/css')
        .html(`
          .reduced-motion .main-sidebar,
          .reduced-motion .content-wrapper {
            transition: none !important;
          }
          
          .reduced-motion .sidebar-toggle-btn {
            transition: background-color 0.1s ease !important;
          }
        `)
        .appendTo('head');
    },
    
    // ========================================================================
    // SMOOTH ANIMATION HELPERS
    // ========================================================================
    
    /**
     * Smooth collapse with staged animations
     */
    smoothCollapse: function(callback) {
      var sidebar = $('.main-sidebar');
      var content = $('.content-wrapper');
      
      // Stage 1: Fade out sidebar content (100ms)
      sidebar.find('.scrollable-sidebar').animate({
        opacity: 0
      }, 100, () => {
        
        // Stage 2: Slide sidebar (200ms)
        this.performSlideAnimation(sidebar, content, true, () => {
          
          // Stage 3: Show toggle button (100ms)
          $('.floating-sidebar-toggle').fadeIn(100, callback);
        });
      });
    },
    
    /**
     * Smooth expand with staged animations
     */
    smoothExpand: function(callback) {
      var sidebar = $('.main-sidebar');
      var content = $('.content-wrapper');
      
      // Stage 1: Hide toggle button (100ms)
      $('.floating-sidebar-toggle').fadeOut(100, () => {
        
        // Stage 2: Slide sidebar (200ms)
        this.performSlideAnimation(sidebar, content, false, () => {
          
          // Stage 3: Fade in sidebar content (100ms)
          sidebar.find('.scrollable-sidebar').animate({
            opacity: 1
          }, 100, callback);
        });
      });
    },
    
    /**
     * Core slide animation using CSS transitions
     */
    performSlideAnimation: function(sidebar, content, collapse, callback) {
      // Use CSS transitions for hardware acceleration
      if (collapse) {
        sidebar.addClass('collapsed');
        content.addClass('sidebar-collapsed');
      } else {
        sidebar.removeClass('collapsed');
        content.removeClass('sidebar-collapsed');
      }
      
      // Wait for CSS transition to complete
      setTimeout(callback, 200);
    },
    
    // ========================================================================
    // VISUAL FEEDBACK ENHANCEMENTS
    // ========================================================================
    
    /**
     * Add visual feedback during interactions
     */
    addVisualFeedback: function() {
      // Ripple effect on toggle button click
      $(document).on('click', '.sidebar-toggle-btn, .floating-sidebar-toggle', function(e) {
        if (window.SidebarAnimations.isReducedMotion) return;
        
        var $btn = $(this);
        var ripple = $('<span class="ripple-effect"></span>');
        
        $btn.append(ripple);
        
        setTimeout(() => {
          ripple.remove();
        }, 600);
      });
      
      // Hover animations for sidebar items during collapse
      $('.main-sidebar').on('mouseenter', '.sidebar-menu li', function() {
        if (window.SidebarCollapse.isCollapsed && !window.SidebarAnimations.isReducedMotion) {
          $(this).addClass('hover-highlight');
        }
      }).on('mouseleave', '.sidebar-menu li', function() {
        $(this).removeClass('hover-highlight');
      });
    },
    
    /**
     * Progress indicator during Plan button auto-collapse
     */
    showPlanProgress: function() {
      var progressBar = $('<div class="plan-progress-bar"><div class="progress-fill"></div></div>');
      $('body').append(progressBar);
      
      progressBar.find('.progress-fill').animate({
        width: '100%'
      }, 500, () => {
        setTimeout(() => {
          progressBar.fadeOut(200, () => progressBar.remove());
        }, 200);
      });
    },
    
    // Cleanup function
    destroy: function() {
      try {
        if (this.observer) {
          this.observer.disconnect();
        }
        if (this.rafId) {
          cancelAnimationFrame(this.rafId);
        }
      } catch (error) {
        console.error('Error during sidebar animations cleanup:', error);
      }
    },
    
    // Error handling for animation failures
    handleError: function(error, context) {
      console.error('Sidebar animation error:', error, context);
      
      // Reset animation state
      if (this.rafId) {
        cancelAnimationFrame(this.rafId);
        this.rafId = null;
      }
      
      // Remove stuck animation classes
      $('.main-sidebar, .content-wrapper').removeClass('collapse-transition animation-ready');
      
      // Notify global error handler if available
      if (window.SidebarCollapse && typeof window.SidebarCollapse.handleError === 'function') {
        window.SidebarCollapse.handleError(error, 'animation_' + context);
      }
    },
    
    // Safe animation wrapper
    safeAnimate: function(element, properties, duration, callback) {
      try {
        if (!element || element.length === 0) {
          console.warn('Cannot animate - element not found');
          if (callback) callback();
          return;
        }
        
        element.animate(properties, duration, callback);
      } catch (error) {
        this.handleError(error, 'safe_animate');
        if (callback) callback();
      }
    }
  };
  
  // ========================================================================
  // CSS CLASSES FOR ANIMATIONS
  // ========================================================================
  
  // Add dynamic CSS for animations
  $('<style>')
    .prop('type', 'text/css')
    .html(`
      .ripple-effect {
        position: absolute;
        top: 50%;
        left: 50%;
        width: 0;
        height: 0;
        border-radius: 50%;
        background: rgba(255, 255, 255, 0.6);
        transform: translate(-50%, -50%);
        animation: ripple 0.6s linear;
        pointer-events: none;
      }
      
      @keyframes ripple {
        to {
          width: 100px;
          height: 100px;
          opacity: 0;
        }
      }
      
      .hover-highlight {
        background-color: rgba(255, 255, 255, 0.1) !important;
        transition: background-color 0.2s ease !important;
      }
      
      .plan-progress-bar {
        position: fixed;
        top: 0;
        left: 0;
        width: 100%;
        height: 3px;
        background: rgba(0, 0, 0, 0.1);
        z-index: 9999;
      }
      
      .progress-fill {
        height: 100%;
        width: 0;
        background: linear-gradient(90deg, #5B90BF, #5BA672);
        transition: width 0.5s ease;
      }
      
      .animation-ready .main-sidebar,
      .animation-ready .content-wrapper {
        transition-delay: 0s;
      }
    `)
    .appendTo('head');
  
  // Initialize animation system
  window.SidebarAnimations.init();
  window.SidebarAnimations.addVisualFeedback();
});