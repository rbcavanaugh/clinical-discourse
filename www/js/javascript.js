var isMobileBinding = new Shiny.InputBinding();
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