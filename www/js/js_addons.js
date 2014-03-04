Shiny.addCustomMessageHandler("jsCreatedDT", function(message) {
    //$('#tracktable').width(1155); 
    $('#'+message.id).width(1170);
    var html = '<table cellpadding="0" cellspacing="0" border="0" class="table table-striped table-bordered table-condensed table-inside-modal" id="dt'+message.id+'"><tfoot><tr> <td></td> <th>-</th><th>-</th><th>-</th><th>-</th> <td></td><td></td><td></td><td></td><td></td> </tr></tfoot></table>'
    $('#'+message.id).html(html);
    var example = $('#dt'+message.id).dataTable( {
        "aaData": message.tab,
    		//"bPaginate": true,
        //"iDisplayLength" : 100,
        //"bScrollInfinite": true,
  	    //"bScrollCollapse": true,
				"sScrollY": "350px",
				//"sPaginationType": "bootstrap",
				"sDom": 'TfirtS',
  	    "bDeferRender": true,
        //"bAutoWidth": false,
				"oLanguage": {
					"sLengthMenu": "_MENU_ records per page"
				},
				"aaSorting": [[ 1, "desc" ]],
        "oTableTools": {
			     "sRowSelect": "multi",
               "fnPreRowSelect": function ( e, nodes ) {
                if(!e) return true;
                if ( e.target.className.indexOf('no_select') != -1 ) {
                    return false;
                }
                return true;
            },
           "fnRowSelected": function ( node, oConfig, nRow ) {
                         jQuery(node).find(".select_indicator").removeClass( "icon-check-empty" ).addClass( "icon-check icon-spin" );
                        //var a = TableTools.fnGetInstance( 'example' ).fnGetSelectedData();
                        //var sel = [];
                        //for(var i=0; i< a.length; i++) {
                        //    sel.push(a[i][0]);
                        //}
                        //alert( JSON.stringify(sel) );
                        //Shiny.shinyapp.sendInput({"f_tracks":sel});
                    },
            "fnRowDeselected": function ( node ) {
              jQuery(node).find(".select_indicator").removeClass( "icon-check icon-spin" ).addClass( "icon-check-empty" );
            },
			     "aButtons": [ 
             {"sExtends": "text", "sButtonText": "Select filtered", "fnClick": function ( node, conf ) {
               var zz = this.s.dt.oInstance.$('tr', {"filter":"applied"});
               if ( zz.length < 100 ) {
                 this.fnSelect( zz )
               } else {
                 alert('Select less than 100 rows!')
               }
               
               //.s.dt.oInstance
               //TableTools.fnGetInstance( 'dttracktable'   ).fnSelect( $('#dttracktable tbody tr') ) 
               } }, "select_none" ]
		    },
				"aoColumns": [
									{ "sTitle": '<i class="icon-file-alt"></i> File name',
                    "mRender": function ( data, type, row ) {
                      var short = data.length>60 ? data.substr(0,37)+'[...]'+data.substr(data.length-20,data.length) : data;
                      //return '<a href="files/' + data + '" class="no_select">'+ short +'</a>';
                      if( data.length>60 ) { return '<abbr title="'+data+'">'+short+'</abbr>' } else { return short }
                    }
  								},
									{ "sTitle": '<i class="icon-calendar"></i> Date created' },
									{ "sTitle": '<i class="icon-beaker"></i> Format' },
									{ "sTitle": '<i class="icon-info-sign"></i> Genome' },
									{ "sTitle": '<i class="icon-user"></i> User'},
                  
                  
									{ "sTitle": "Comment", "bVisible": false},
                  
                  { "sTitle": '<i class="icon-check icon-large"></i>',
                    "sClass": "table-center",
                    "sWidth": "29px",
                    "mData": null,
                    "bSortable": false,
                    "mRender": function ( data, type, row ) {
                      return '<i class="icon-check-empty select_indicator icon-large"></i>'
                    }
                  },
                     { "sTitle": '<i class="icon-comments-alt icon-large"></i>',
                    "mData": null,
                    "sClass": "table-center",
                    "bSortable": false,
                    "mRender": function ( data, type, row ) {
                      if (row[5].length) {
                        return '<a class="btn btn-mini no_select" rel="popover" data-content="'+ row[5] +'" data-original-title="Comment"'+ 
                          'onClick="$(this).popover({trigger: \'hover\', placement: \'left\'}).popover(\'show\')">'+'<i class="icon-comment icon-large no_select"></i></a>'
                      } else {
                        return '<div style="width: 29px"></div>'
                      }
                    }
                  },
                  { "sTitle": '<i class="icon-hdd icon-large"></i>',
                    "sClass": "table-center",
                    "mData": null,
                    "bSortable": false,
                    "mRender": function ( data, type, row ) {
                      return '<a class="btn btn-mini no_select" href="files/' + row[0] + '" target="_blank">'+'<i class="icon-download-alt icon-large no_select"></i></a>'
                    }
                  },
                  { "sTitle": '<i class="icon-trash icon-large""></i>',
                    "sClass": "table-center",
                    "mData": null,
                    "bSortable": false,
                    "mRender": function ( data, type, row ) {
                      return '<button class="btn btn-mini btn-danger no_select" onClick="jsRmFile(&#39;'+row[0]+'&#39;)"><i class="icon-remove icon-large no_select"></i></button>'
                    }
                  }
				]
			} );
			$('#dt'+message.id).parent().next().find("tfoot th").each( function ( i ) {
        		this.innerHTML = fnCreateSelect( example.fnGetColumnData(i+1) );
        		$("select", this).change( function () {
            	example.fnFilter( $(this).val(), i+1 );
        });
      });
});

animateTitle = function() {
  $("#letter-container h2 a").lettering();
	$.each($('.letter-container h2 a span'), function(index, value) { $(this).css({
		'-webkit-animation-delay': Math.random()*5+'s',
		'-moz-animation-delay': Math.random()*5+'s',
		'-ms-animation-delay': Math.random()*5+'s',
		'animation-delay': Math.random()*5+'s',
		//"background-color": '#'+Math.floor(Math.random()*16777215).toString(16), 
		'color': '#'+Math.floor(Math.random()*16777215).toString(16)		 
	}) });
//	$.each($('.letter-container h2 a span'), function(index, value) { $(this).css('-moz-animation-delay', Math.random()*3) });
//	$.each($('.letter-container h2 a span'), function(index, value) { $(this).css('-ms-animation-delay', Math.random()*3) });
//	$.each($('.letter-container h2 a span'), function(index, value) { $(this).css('animation-delay', Math.random()*3) });
//	$.each($('.letter-container h2 a span'), function(index, value) { $(this).css('color', '#'+Math.floor(Math.random()*16777215).toString(16)) });
	$('.letter-container h2 a span').hover( function () { $(this).css("color", '#'+Math.floor(Math.random()*16777215).toString(16) ); });
}


sendToCalc = function() {
    var t_sel = [];
    var f_sel = [];
    var genomes = [];
    $.each(TableTools.fnGetInstance( 'dttracktable'   ).fnGetSelectedData(), function(i, v) {  t_sel[i]=v[0];  genomes.push(v[3]); } );
    $.each(TableTools.fnGetInstance( 'dtfeaturetable' ).fnGetSelectedData(), function(i, v) {  f_sel[i]=v[0];  genomes.push(v[3]); } );
    
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
	//Shiny.shinyapp.sendInput({"TR_calculate":null});
	$("#myModal").modal("hide"); 
	$("#progressModal").modal("show");

}

jsRmFile = function(x) {
	if ( confirm("Are you sure you want to delete " + x + "?") ){
		Shiny.shinyapp.sendInput({"delFileVar":x}); 
	}
}

rmSelctedFiles = function() {
      var rm = [];
      $.each(TableTools.fnGetInstance( 'dttracktable'   ).fnGetSelectedData(), function(i, v) { rm.push(v[0]); } );
      $.each(TableTools.fnGetInstance( 'dtfeaturetable' ).fnGetSelectedData(), function(i, v) { rm.push(v[0]); } );
	if(rm.length > 0) {
    if ( confirm("Delate following files:\n " + rm.toString() + "?" ) ){
        Shiny.shinyapp.sendInput({"f_delate":rm});
        Shiny.shinyapp.sendInput({"TR_delate":new Date().getTime()}); 
		}
	}	else { alert("Select >=1 track(s) or >=1 feature(s)!") }
}

$(function() {
  $("#myModal").on("shown", function(e) { 
    if($.fn.dataTable.fnTables().length > 0) {
      $.each($.fn.dataTable.fnTables(), function(i, v) { $(v).dataTable().fnAdjustColumnSizing(); } )
    }
  })
  
  $('div.img').click(function(e){
    $(this).find('div > img').toggleClass('zoom_image');
    e.preventDefault();
  });
  
	$('#anchoredHidabeDiv').hide();
	
	$('#downloadLegend').tooltip({title:'Legend'});
	$('#downloadClusters').tooltip({title:'Clusters indicates'});
	$('#replot').tooltip({title:'Keyboard binding: [ENTER]'});
	$('[for="reactive"]').tooltip({title:'Keyboard binding: [ctrl+R] or [cmd+R]'});
	$('#spawn').addClass('btn-warning');
  $('i[data-toggle=tooltip]').tooltip();
	
	$(document).keydown(function(e){
    	if ( (e.keyCode == 13) || e.keyCode == 32 && (e.ctrlKey || e.metaKey) ) { 
       		$('#replot').click();
       			return false;
    		} else if (e.keyCode == 82 && (e.ctrlKey || e.metaKey)) { 
       		$('#reactive').click();
       			return false;
    		} else if (e.keyCode == 72 && (e.ctrlKey || e.metaKey)) { 
         	$('#img_heatmap').click();
          $('#replot').click();
       			return false;
    		}
	});
  
  $('#plotTable th').click(function(e) {
    var i = $(this).index()+1;
    $('#plotTable tr > :nth-child('+i+') input[type=checkbox]').prop('checked', !$('#plotTable tr > :nth-child('+i+') input[type=checkbox]').prop('checked'));
  });

  function closeEditorWarning() { 
    if ( (Shiny.shinyapp.$socket.readyState == 1) && !($.cookie('warn')=='false') ) {
      return "If you leave unsaved changes will be lost."  
    }
  } 
  window.onbeforeunload = closeEditorWarning;
  	
});