# TODO: Add comment
# 
# Author: przemol
###############################################################################


renderHTMLgrid <- function(grfile, CC, checked=NULL, addcls='') {
	
	cltab <- suppressWarnings(matrix(rgb(t(col2rgb(colors()[grep("dark",colors())])) , maxColorValue=255), length(grfile), length(grfile[[1]])))
	html.text <- capture.output( {
				cat('<table id="plotTable" class="',addcls,'" style="margin-left: auto; margin-right: auto; text-align: center;">')
				cat('<thead>')
				cat('<tr><th><b>Features:</b></th><th><span><span><span>')
				cat(names(grfile[[1]]), sep='</span></span></span></th><th><span><span><span>')
				cat("</span></span></span></th></tr>")
				cat('</thead>')
				
				cat('<tbody>')
				for ( j in 1:length(grfile) ) {
					cat("<tr>")
					cat("<td>", names(grfile)[j], "</td>")
					for (i in 1:length(grfile[[j]])) {
						cat('<td>')
						if ((i %in% checked[,1]) & (j %in% checked[,2]) ) {
							cat('<input type="checkbox" name="plot_this" value="',toJSON(c(i,j)) ,'" checked />')
						} else {
							cat('<input type="checkbox" name="plot_this" value="',toJSON(c(i,j)) ,'" />')
						}
						if (CC) cat('<br /><input type="color" name="plot_col" id="col_',i,'x', j,'" value="',cltab[j,i] ,'" style="width:25px; display:none;" />', sep='')
						cat('</td>')
					}	
					cat("</tr>")
				}
				cat('</tbody>')
				cat('</table>')
			})
	return(HTML(html.text))
}
