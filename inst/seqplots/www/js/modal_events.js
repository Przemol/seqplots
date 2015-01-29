var modalBinding = new Shiny.InputBinding();
$.extend(modalBinding, {
  find: function(scope) {
    return $(scope).find("#calcModal");
  },
  getValue: function(el) {
	// no-impl
  },
  setValue: function(el, value) {
	// no-impl
  },
  subscribe: function(el, callback) {
    $(el).on("shown", function(e) {
    	//$('#publicRdata').val(' ').change(); 
    	
		//$('#plot_type').change(); 
    	callback();
    });
  },
  unsubscribe: function(el) {
    $(el).off("#calcModal");
  }
});

Shiny.inputBindings.register(modalBinding);