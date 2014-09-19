if( !Modernizr.inputtypes.color ) { jscolor.bind() };

$('#htmltab th b').click(function() { 
  $('#htmltab input[value*="["]').click()
});

$('#htmltab th span span span').click(function() { 
	var th = $(this).parent().parent().parent();
	$('input[value*="['+th .parent().children().index(th)+',"]').click()
});

$("#htmltab tr td:first-child").click(function() { 
	var num = $(this).parent().parent().children().index($(this).parent()) + 1;
	$('input[value*=",'+num+']"]').click()

});
