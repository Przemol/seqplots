//JS alert handle
Shiny.addCustomMessageHandler("jsAlert", function(message) {
    alert(message)
});
    
//JS exec handle
Shiny.addCustomMessageHandler("jsExec", function(message) {
    eval(message)
});
    
//JS meaages handle
Shiny.addCustomMessageHandler("jsAssign", function(message) {
    M = message;
});
Shiny.addCustomMessageHandler("jsDots", function(message) {
    $("#summary3").text( $("#summary3").text().length < 50 ? $("#summary3").text()+"." : "." );
});

var selected = [];
$('#trackDT').on( 'click', 'tbody tr', function () {
  if($(this).hasClass('selected')) {
    jQuery(this).find(".select_indicator").removeClass( "icon-check-empty" ).addClass( "icon-check" );
  } else {
     jQuery(this).find(".select_indicator").removeClass( "icon-check" ).addClass( "icon-check-empty" );
  }
} );
    

animateTitle = function() {
  $("#letter-container h2 a").lettering();
	$.each($('.letter-container h2 a span'), function(index, value) { $(this).css({
		'-webkit-animation-delay': Math.random()*5+'s',
		'-moz-animation-delay': Math.random()*5+'s',
		'-ms-animation-delay': Math.random()*5+'s',
		'animation-delay': Math.random()*5+'s',
		'color': '#'+Math.floor(Math.random()*16777215).toString(16)		 
	}) });
	$('.letter-container h2 a span').hover( function () { $(this).css("color", '#'+Math.floor(Math.random()*16777215).toString(16) ); });
}


sendToCalc = function() {
    var t_sel = [];
    var f_sel = [];
    var genomes = [];
    if($('#trackDT table').length)  $.each(TableTools.fnGetInstance( $($('#trackDT table')[0]).attr('id')   ).fnGetSelectedData(), function(i, v) {  t_sel[i]=v[0];  genomes.push(v[3]); } );
    if($('#featureDT table').length) $.each(TableTools.fnGetInstance( $($('#featureDT table')[0]).attr('id') ).fnGetSelectedData(), function(i, v) {  f_sel[i]=v[0];  genomes.push(v[3]); } );
    
	if( !(f_sel.length >= 1 & (t_sel.length >= 1 | $('#SFsetup').text().length > 10 )) ) {
		return( alert("Select >=1 track(s) or pattern and >=1 feature(s)!") )
	}
	if ( $.unique(genomes).length > 1 ) {
		var msg = "More than one genome or genome version selected: " + $.unique(genomes) + ". \n\n" +
			"All tracks and feature files should be in the same genome version. " +
			"This setup is most likely to produce an error or non reliable plot. " + 
			"If you want to continue anyway press OK.";
		if( !confirm(msg) ) { return(0) }	
	}
  Shiny.shinyapp.sendInput({"f_tracks":t_sel});
  Shiny.shinyapp.sendInput({"f_features":f_sel});
	Shiny.shinyapp.sendInput({"TR_calculate":new Date().getTime()}); 
	$("#calcModal").modal("hide"); 
	$("#progressModal").modal("show");

}

jsRmFile = function(x) {
	if ( confirm("Are you sure you want to delete " + x + "?") ){
		Shiny.shinyapp.sendInput({"delFileVar":x}); 
	}
}

rmSelctedFiles = function() {

    if ( confirm("Delete all selected files?" ) ){
        Shiny.shinyapp.sendInput({"TR_delate":new Date().getTime()}); 
	}

}

$(function() {
//  $("#calcModal").on("shown", function(e) { 
//    if($.fn.dataTable.fnTables().length > 0) {
//      $.each($.fn.dataTable.fnTables(), function(i, v) { $(v).dataTable().fnAdjustColumnSizing(); } )
//    }
//  })
  

  
	
	$('#downloadLegend').tooltip({title:'Legend'});
	$('#downloadClusters').tooltip({title:'Clusters indicates'});
	$('#replot').tooltip({title:'Refresh the plot and applying new settings. Keyboard binding: [ENTER]'});
	$('[for="reactive"]').tooltip({title:'Keyboard binding: [ctrl+R] or [cmd+R]'});
  $('#hsccoef').parents('.row-fluid').tooltip({title:'0.01 (default on slider) calculates color key limits using data range from 1-99 percentile.  0.1 uses data range from 10-90 percentile.', placement:"right"});
	$('#spawn').addClass('btn-warning');
  $('i[data-toggle=tooltip]').tooltip();
  
	
  $('#stopapp').addClass('btn-warning');
  
	$(document).keydown(function(e){
    	if ( (e.keyCode == 13) || e.keyCode == 32 && (e.ctrlKey || e.metaKey) ) {
        if( $(e.target).prop('id') == "debug_cmd" ) return
       		$('#replot').click();
       			return true;
    		} else if (e.keyCode == 82 && (e.ctrlKey || e.metaKey)) { 
       		$('#reactive').click();
       			return false;
    		} else if (e.keyCode == 72 && (e.ctrlKey || e.metaKey)) { 
         	$('#img_heatmap').click();
          $('#replot').click();
       			return false;
    		}
	});
  
  //$('#plotTable th').click(function(e) {
   // var i = $(this).index()+1;
   // $('#plotTable tr > :nth-child('+i+') input[type=checkbox]').prop('checked', !$('#plotTable tr > :nth-child('+i+') input[type=checkbox]').prop('checked'));
  //});

  function closeEditorWarning() { 
    if ( (Shiny.shinyapp.$socket.readyState == 1) && !($.cookie('warn')=='false') ) {
      return "If you leave unsaved changes will be lost."  
    }
  } 
  window.onbeforeunload = closeEditorWarning;
  	
});
