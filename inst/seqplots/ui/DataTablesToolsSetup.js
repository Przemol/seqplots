{
  		     "sRowSelect": "multi",
           "fnPreRowSelect": function ( e, nodes ) {
                if(!e) return true;
                if ( e.target.className.indexOf('no_select') != -1 ) {
                    return false;
                }
                return true;
            },
           "fnRowSelected": function ( node, oConfig, nRow ) {
              jQuery(node).find(".select_indicator").removeClass( "icon-check-empty" ).addClass( "icon-check" );
              jQuery(node).parents('.dataTables_wrapper').find(".selectionsInfo").html(this.fnGetSelectedData().length +' selected');
              
            },
            "fnRowDeselected": function ( node ) {
              jQuery(node).find(".select_indicator").removeClass( "icon-check" ).addClass( "icon-check-empty" );
              jQuery(node).parents('.dataTables_wrapper').find(".selectionsInfo").html(this.fnGetSelectedData().length +' selected');

            },
			     "aButtons": [ 
             {"sExtends": "text", "sButtonText": "Select filtered", "fnClick": function ( node, conf ) {
                 this.fnSelectAll( true )
                 alert('Total selections: '+ this.fnGetSelectedData().length +' rows!')
               } }, "select_none" ]
}
