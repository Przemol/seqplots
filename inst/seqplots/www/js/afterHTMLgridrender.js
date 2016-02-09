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
	var num = $(this).parents('tbody').children().index($(this).parents('tr')) + 1;
	$('input[value*=",'+num+']"]').click()

});

$(".toogle-sel-btn-vis").click(function() { 
    $( "a[class*='grid-']" ).toggle();
});
$(".toogle-col-btn-vis").click(function() { 
    $('.rhdrs').toggle();
});
$(".toogle-row-btn-vis").click(function() { 
    $('.hhdrs').toggle()
});

$(".rhdr").click(function() { 
    var text = $(this).parents('.input-group-btn').siblings().val();
    var who = $(this).attr('data-who');
    if( text == '' ) text = $(this).attr('data-fname');
    if( who == 'inc' ) {
        $(this).parents('tr').find('select[id*='+who+']').val(text).change();
    } else {
        $(this).parents('tr').find('input[id*='+who+']').val(text).change();
    }

});

$('.rdr-numeric-auto-input').change(function() { 
    var text = $(this).val();
    var who = $(this).attr('data-who');
    $(this).parents('tr').find('input[id*='+who+']').val(text).change();
});

$('.hhdr').click(function() { 
    var num = $(this).parents('th').index()+1;
    var text = $(this).parents('.input-group-btn').siblings().val();
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

$('.chdr').click(function() { 
   
    var text = $(this).parents('.input-group-btn').siblings().val();
    var who = $(this).attr('data-who');
    if( text == '' ) text = $(this).attr('data-fname');
    if( who == 'inc' ) {
        $('#plotTable select[id*='+who+']').val(text).change();
    } else {
        $('#plotTable input[id*='+who+']').val(text).change();
    }
       
});

$('.chdrat').click(function() { 
    $('#plotTable input[id*=label_]').each( function() { $(this).val($(this).attr('data-at')).change() } )   
});

$('.chdr-numeric-auto-input').change(function() { 
    var num = $(this).parents('th').index()+1;
    var text = $(this).val();
    var who = $(this).attr('data-who');
    $('#plotTable input[id*='+who+']').val(text).change();
    
});

$('.chdrrowcol').click(function() { 
    $('#plotTable input[id*=label_]').each( function(x) { 
        var z = $(this).attr('id'); 
        console.log(this);
        var x = z.match('_(.+)x(.+)$')[1];
        var y = z.match('_(.+)x(.+)$')[2];
        var v = $('#hhdr_' + x).val() + ' @ ' + $('#rhdrs_' + y).val();
        $(this).val(v).change();
    })
});

$('.chdrrm').click(function() { 
    $('#plotTable input[id*=label_]').each( function() { $(this).val('').change() } )   
});

$('.havettp').tooltip();
    



    // $('#plotTable tr > :nth-child('+num+') input[id*=label]')
    //$('input[id*=",'+num+']"]').click();
