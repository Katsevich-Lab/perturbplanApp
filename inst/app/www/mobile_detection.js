// ========================================================================
// MOBILE DEVICE DETECTION AND WARNING SYSTEM
// ========================================================================

/**
 * Mobile Detection System
 * Detects mobile phones (excluding tablets like iPad) and displays a warning
 * directing users to access the app from a desktop computer or tablet
 */

window.MobileDetection = {
  /**
   * Mobile phone detection (excludes tablets like iPad)
   * Checks user agent and screen size to determine if user is on a phone
   */
  isMobilePhone: function() {
    // Check 1: User Agent detection
    const userAgent = navigator.userAgent || navigator.vendor || window.opera;

    // Exclude iPads explicitly (they can handle the app)
    if (/iPad/i.test(userAgent)) {
      return false;
    }

    // Mobile phone patterns (excluding iPad which we checked above)
    const mobilePhonePatterns = [
      /Android/i,
      /webOS/i,
      /iPhone/i,
      /iPod/i,
      /BlackBerry/i,
      /Windows Phone/i,
      /IEMobile/i,
      /Opera Mini/i
    ];

    const isMobileUA = mobilePhonePatterns.some(pattern => pattern.test(userAgent));

    // Check 2: Screen size detection (phones typically < 768px)
    // Tablets like iPad are usually >= 768px
    const isPhoneScreen = window.innerWidth < 768 || window.screen.width < 768;

    // Check 3: Touch capability (additional indicator)
    const isTouchDevice = 'ontouchstart' in window || navigator.maxTouchPoints > 0;

    // Return true only if it's a mobile user agent AND small screen
    return isMobileUA && isPhoneScreen && isTouchDevice;
  },

  /**
   * Notify Shiny that mobile device was detected
   */
  notifyShiny: function() {
    if (typeof Shiny !== 'undefined') {
      Shiny.setInputValue('mobile_device_detected', true, {priority: 'event'});
    }
  },

  /**
   * Initialize mobile detection on page load
   */
  init: function() {
    // Wait for Shiny to be ready
    $(document).on('shiny:connected', () => {
      // Run detection after a short delay to ensure app is initialized
      setTimeout(() => {
        if (this.isMobilePhone()) {
          this.notifyShiny();
        }
      }, 500);
    });

    // Also check on window resize (for device orientation changes)
    let resizeTimer;
    window.addEventListener('resize', () => {
      clearTimeout(resizeTimer);
      resizeTimer = setTimeout(() => {
        if (this.isMobilePhone()) {
          this.notifyShiny();
        }
      }, 250);
    });
  }
};

// ========================================================================
// INITIALIZATION
// ========================================================================

// Initialize mobile detection as soon as DOM is ready
$(document).ready(function() {
  window.MobileDetection.init();
});
