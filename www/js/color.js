

var colorBinding = new Shiny.InputBinding();
$.extend(colorBinding, {
  find: function(scope) {
    return $(scope).find("input[type=color]");
  },
  getValue: function(el) {
    return $(el).val();
  },
  setValue: function(el, value) {
    $(el).val(value);
  },
  subscribe: function(el, callback) {
    $(el).on("change.colorBinding", function(e) {
      callback(true);
    });
  },
  unsubscribe: function(el) {
    $(el).off(".colorBinding");
  },
  getRatePolicy: function() {
    return {policy: 'debounce', delay: 250};
  }
});

Shiny.inputBindings.register(colorBinding);