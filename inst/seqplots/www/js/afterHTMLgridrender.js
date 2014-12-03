if( !Modernizr.inputtypes.color ) { jscolor.bind() };

$('.grid-all-toggle-btn').click(function() { 
  $('#htmltab input[value*="["]').click()
});

$('.grid-all-select-btn').click(function() { 
  $('#htmltab input[value*="["]').prop('checked', true).change()
});


$('.grid-all-remove-btn').click(function() { 
  $('#htmltab input[value*="["]').prop('checked', false).change()
});


$('.grid-col-select-btn').click(function() { 
	var th = $(this).parent();
	$('input[value*="['+th .parent().children().index(th)+',"]').click()
});

$(".grid-row-select-btn").click(function() { 
	var num = $(this).parent().parent().children().index($(this).parent()) + 1;
	$('input[value*=",'+num+']"]').click()

});

$(".toogle-sel-btn-vis").click(function() { 
    $( "a[class*='grid-']" ).toggle();
});

$('a[data-toggle=tooltip]').tooltip();

$(".rhdr").click(function() { 
    var text = $(this).siblings().val();
    var who = $(this).attr('data-who');
    if( text == '' ) text = $(this).attr('data-fname');
    if( who == 'inc' ) {
        $(this).parents('tr').find('select[id*='+who+']').val(text).change();
    } else {
        $(this).parents('tr').find('input[id*='+who+']').val(text).change();
    }

});

$('.hhdr').click(function() { 
    var num = $(this).parents('th').index()+1;
    var text = $(this).siblings().val();
    var who = $(this).attr('data-who');
    if( text == '' ) text = $(this).attr('data-fname');
    if( who == 'inc' ) {
        $('#plotTable tr > :nth-child('+num+') select[id*='+who+']').val(text).change();
    } else {
        $('#plotTable tr > :nth-child('+num+') input[id*='+who+']').val(text).change();
    }
     

    
});

$('.hdr-numeric-auto-input').change(function() { 
    var num = $(this).parents('th').index()+1;
    var text = $(this).val();
    var who = $(this).attr('data-who');
    $('#plotTable tr > :nth-child('+num+') input[id*='+who+']').val(text).change();
    
});


    // $('#plotTable tr > :nth-child('+num+') input[id*=label]')
    //$('input[id*=",'+num+']"]').click();
