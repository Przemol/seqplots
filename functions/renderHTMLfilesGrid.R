# TODO: Add comment
# 
# Author: przemol
###############################################################################


renderFiles <- function(df, id=paste(sample(c(LETTERS, letters, 1:10), 10), collapse=''), grid=TRUE, rd=1) {
	
	#cltab <- suppressWarnings(matrix(rgb(t(col2rgb(colors()[grep("dark",colors())])) , maxColorValue=255), length(grfile), length(grfile[[1]])))
	html.text <- capture.output( {
				cat('<table cellpadding="0" cellspacing="0" border="0" class="table table-striped table-bordered table-condensed" id="',id,'">', sep='')
				cat('<thead>')
				cat('<tr><th>')
				cat(colnames(df), sep='</th><th>')
				cat("</th></tr>")
				cat('</thead>')
				
				cat('<tbody>')
				for ( i in 1:nrow(df) ) {
					cat("<tr>")
					#cat("<td>", names(grfile)[j], "</td>")
					for ( j in 1:ncol(df) ) {
						cat('<td>')
							if (j == 1) {
								cat(paste0('<a href="files/', df[i,j],'">'))
									if ( nchar(df[i,j]) <= 65 ) {
										cat(df[i,j])
									} else {
										cat( paste0(substr(df[i,j], 1, 50), '[...]', substring(df[i,j], nchar(df[i,j])-10) ))
									}
									
								cat('</a>')
							} else {
								cat(df[i,j])
							}
							
						cat('</td>')
					}	
					cat("</tr>")
				}
				cat('</tbody>')
				if(grid) {
					
					
					cat('<tfoot>')
					cat('<tr>  <td></td> <th>')
					cat(rep('-', ncol(df)-3), sep='</th><th>')
					cat("</th><td></td><td></td> </tr>")
					cat('</tfoot>')
					
				
				}
				cat('</table>')
			})
	
#	scr	<- '$(".fTable").dataTable({
#				"bJQueryUI": true,
#				"sPaginationType": "full_numbers",
#				"aoColumnDefs": [
#					{ "sWidth": "40%", "aTargets": [ 1 ] }
#				],
#				"aoColumns": [
#					{ "sTitle": "File name" },
#					{ "sTitle": "Time_Created" },
#					{ "sTitle": "Format" },
#					{ "sTitle": "Genome" },
#					{ "sTitle": "User" },
#					{ "sTitle": "Comment", "bVisible": false},
#					{ "sTitle": "+" }
#				]
#			})'
	if(grid) {
		scr <- paste0(
			'var ',id,' = $("#',id,'").dataTable( {
				"bPaginate": false,
				"sScrollY": "350px",
				"sPaginationType": "bootstrap",
				"sDom": \'firt\',
				"oLanguage": {
					"sLengthMenu": "_MENU_ records per page"
				},
				"aaSorting": [[ 1, "desc" ]],
				"aoColumns": [
									{ "sTitle": "File name" },
									{ "sTitle": "Time_Created" },
									{ "sTitle": "Format" },
									{ "sTitle": "Genome" },
									{ "sTitle": "User" },
									{ "sTitle": "Comment", "bVisible": false},
									{ "sTitle": "+" },
									{ "sTitle": "-" }
				]
			} );
			$("#',id,'").parent().next().find("tfoot th").each( function ( i ) {
        		this.innerHTML = fnCreateSelect( ',id,'.fnGetColumnData(i+1) );
        		$("select", this).change( function () {
            	',id,'.fnFilter( $(this).val(), i+1 );
        } );
    } );

				')
	} else scr <- ''
		
	return(div(HTML(html.text), tags$script(scr, id=rd)) )
}
