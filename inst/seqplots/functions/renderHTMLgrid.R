# TODO: Add comment
# 
# Author: przemol
###############################################################################


renderHTMLgrid <- function(grfile, CC, checked=NULL, addcls='') {
	
	#cltab <- suppressWarnings(matrix(rgb(t(col2rgb(colors()[grep("dark",colors())])) , maxColorValue=255), length(grfile), length(grfile[[1]])))
	cls <- c("darkblue", "darkgreen", "darkred", "darkmagenta", "darkgray", "darkorange", "darkcyan", "black", rainbow((length(grfile)*length(grfile[[1]]))-8))[1:(length(grfile)*length(grfile[[1]]))]
	cltab <-matrix( rgb(t(col2rgb(cls)), maxColorValue=255), length(grfile[[1]]), length(grfile)  )
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
            if ( if(!is.null(checked)) if( any( apply(checked, 1, identical, c(i,j)) ) ) TRUE else FALSE else FALSE ) {
							cat('<input type="checkbox" name="plot_this" value="[',i,',',j,']" checked />', sep='')
						} else {
							cat('<input type="checkbox" name="plot_this" value="[',i,',',j,']" />', sep='')
						}
						if (CC) {
             
						  cat('<div class="div_separator div_setup"  style="display:none; min-width:90px;"><hr /></div>')
              
              cat('<div class="div_color div_setup"  style="display:none">
                    <input type="color" name="plot_col" id="color_',i,'x', j,'" class="color {hash:true}", value="',cltab[i,j],'" style="width:60px;" title="Set a color here." data-placement="left"/>  </div> ', sep='')
              
              cat('<div class="div_label div_setup"  style="display:none">
                  <input type="text"   id="label_',i,'x', j,'" style="width:60px" placeholder="Label..." title="Set a label here." data-placement="left"/> </div> ', sep='')
              
              cat('<div class="div_prior div_setup" style="display:none">
                  <input type="number" id="prior_',i,'x', j,'" style="width:60px" value=0 title="Set priority here...<br />Line plots: Higher number first in legend, lower plot on top.<br />Heatmaps: Higher number left." data-placement="left"/> </div> ', sep='')
              
              cat('<div class="div_inc div_setup"  style="display:none; min-width:91px">
                  <select id="inc_',i,'x', j,'" style="width:85px" title="Include for sorting or clustering." data-placement="left"><option value="true" selected="selected">Include</option><option value="false">Exclude</option></select></div> ', sep='')
              
              cat('<div class="div_min div_max div_setup"   style="display:none; min-width:127px"">
                  <input type="number"     id="min_',i,'x', j,'" style="width:40px" title="Heatmap MIN limit."/> - <input type="number"     id="max_',i,'x', j,'" style="width:40px" title="Heatmap MAX limit.."/> </div> ', sep='')

              
              
						}
						cat('</td>')
					}	
					cat("</tr>")
				}
				cat('</tbody>')
				cat('</table>')
        #cat('<script> if( !Modernizr.inputtypes.color ) { jscolor.bind() } </script>')
				cat('<script>')
				  cat(readLines(file.path(Sys.getenv("web", '.'), 'www/js/afterHTMLgridrender.js'), warn = FALSE, encoding = "UTF-8"))
				cat('</script>')
			})
	return(HTML(html.text))
}
