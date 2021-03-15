/*var isMobileBinding = new Shiny.InputBinding();
$.extend(isMobileBinding, {
  find: function(scope) {
    return $(scope).find(".mobile-element");
    callback();
  },
  getValue: function(el) {
    return /((iPhone)|(iPod)|(iPad)|(Android)|(BlackBerry))/.test(navigator.userAgent)
  },
  setValue: function(el, value) {
  },
  subscribe: function(el, callback) {
  },
  unsubscribe: function(el) {
  }
});

Shiny.inputBindings.register(isMobileBinding);


var width = 0;
$(document).on("shiny:connected", function(e) {
  width = window.innerWidth;
  Shiny.onInputChange("width", width);
});
$(window).resize(function(e) {
  width = window.innerWidth;
  Shiny.onInputChange("width", width);
})
*/


$(document).ready(function(){
  // Mark columns we want to toggle
  $('body').find('div [class=col-sm-4]').addClass('sidebarPanel');
  $('body').find('div [class=col-sm-8]').addClass('mainPanel');
})


Shiny.addCustomMessageHandler ('resize',function (message) {
  $('.sidebarPanel').toggle();
  $('.mainPanel').toggleClass('col-sm-8 col-sm-12');
  $(window).trigger('resize')
});
