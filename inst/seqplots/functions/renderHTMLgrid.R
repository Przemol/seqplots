# TODO: Add comment
# 
# Author: przemol
###############################################################################


renderHTMLgrid <- function(grfile, CC, checked=NULL, addcls='', controls=NULL) {
	
	#cltab <- suppressWarnings(matrix(rgb(t(col2rgb(colors()[grep("dark",colors())])) , maxColorValue=255), length(grfile), length(grfile[[1]])))
	cls <- c("darkblue", "darkgreen", "darkred", "darkmagenta", "darkgray", "darkorange", "darkcyan", "black", rainbow((length(grfile)*length(grfile[[1]]))-8))[1:(length(grfile)*length(grfile[[1]]))]
	cltab <-matrix( rgb(t(col2rgb(cls)), maxColorValue=255), length(grfile[[1]]), length(grfile)  )
  html.text <- capture.output( {
                
				cat('<table id="plotTable" class="',addcls,'" style="margin-left: auto; margin-right: auto; text-align: center;">')
				cat('<thead>')
				cat(
                    '<tr><th><b>
                    <a class="btn btn-mini pull-left grid-all-toggle-btn havettp" href="#" style="margin-right: 5px" title="Toggle all selections" data-placement=top>
				        <i class="icon-check icon-large"></i></a>
                    <a class="btn btn-mini pull-left grid-all-select-btn havettp" href="#" style="margin-right: 5px" title="Select all" data-placement=top>
    			        <i class="icon-ok-circle icon-large"></i></a>
                    <a class="btn btn-mini pull-left grid-all-remove-btn havettp" href="#" style="margin-right: 5px" title="Select none" data-placement=top>
    			        <i class="icon-remove-circle icon-large"></i></a>
                        Features: 
                    </b>'
				)
				cat('
                    <div class="hhdrs"  style="text-align: center;"> 
                        <div class="div_separator div_setup"  style="display:none;"><hr /></div>
                        <div class="div_color div_setup"  style="display:none">
                            <div class="input-append">
                                <input type="color" style="width:55px" name="plot_col" class="color {hash:true}" title="Set a color for whole row." data-placement="left"/>  
                                <a class="btn chdr havettp" data-who="color" href="#" title="Copy color to all" data-placement=top><i class="icon-fullscreen"></i></a>
                            </div>                        
                        </div> 
                        <div class="div_label div_setup" style="display:none">
                            <div class="input-append">
                                <input type="text" style="width:55px" placeholder="All labels" title="Set a header label here." data-placement="left"/> 
                                <a class="btn chdr havettp" data-who="label" href="#" title="Copy label to all" data-placement=top><i class="icon-fullscreen"></i></a>
                                <a class="btn chdrrowcol havettp" data-who="color" href="#" title="Construct labels using row and col fields" data-placement=top>@</a>
                                <a class="btn chdrat havettp" data-who="color" href="#" title="Set labels to default" data-placement=top>D</a>
                                <a class="btn chdrrm havettp" data-who="color" href="#" title="Clear all labels" data-placement=top>X</a>
                                
                            </div>
                        </div>
                        <div class="div_prior div_setup" style="display:none">
                            <div class="input-append">
                                <input type="number"  style="width:55px" value=0 title="Set priority here...<br />Line plots: Higher number first in legend, lower plot on top.<br />Heatmaps: Higher number left." data-placement="left"/> 
                                <a class="btn chdr havettp" data-who="prior" href="#" title="Copy priority to all"><i class="icon-fullscreen"></i></a>
                            </div> 
                        </div> 
              
                        <div class="div_inc div_setup"  style="display:none; min-width:91px">
                            <div class="input-append">
                                <select  style="width:70px" title="Include for sorting or clustering." data-placement="left"><option value="true" selected="selected">Include</option><option value="false">Exclude</option></select>
                                <a class="btn chdr havettp" data-who="inc" href="#" title="Copy value to all"><i class="icon-fullscreen"></i></a>
                            </div> 
                        </div>
              
                        <div class="div_min div_max div_setup"   style="display:none; min-width:127px"">
                            <input class="chdr-numeric-auto-input" data-who="min" type="number" style="width:40px" title="Heatmap MIN for all heatmaps"/> - 
                            <input class="chdr-numeric-auto-input" data-who="max" type="number" style="width:40px" title="Heatmap MAX for all heatmaps"/>
 
                        </div>
                        </div>'
                    )
                    cat('</th>')
                
                for(i in 1:length(names(grfile[[1]]))) {
                    cat('<th>')
                    cat('<a class="btn btn-mini grid-col-select-btn havettp" href="#" title="Toggle column selection" data-placement=bottom>')
                    cat('<i class="icon-check icon-large"></i></a>')
                    cat('<span><span><span>')
                    cat(names(grfile[[1]])[i])
                    cat('</span></span></span>')
                    cat('<div class="hhdrs"  style="text-align: center;"> 
                        <div class="div_separator div_setup"  style="display:none;"><hr /></div>
                        <div class="div_color div_setup"  style="display:none">
                            <div class="input-append">
                                <input type="color" style="width:55px" name="plot_col" class="color {hash:true}" title="Set a color for whole row." data-placement="left"/>  
                                <a class="btn hhdr havettp" data-who="color" href="#" title="Copy to whole column"><i class="icon-chevron-down"></i></a>
                            </div>                        
                        </div> 
                        <div class="div_label div_setup" style="display:none">
                            <div class="input-append">
                                <input id="hhdr_', i, '" type="text" style="width:55px" placeholder="', names(grfile[[1]])[i], '" title="Set a header label here." data-placement="left"/> 
                                <a class="btn hhdr havettp" data-who="label" data-fname="', names(grfile[[1]])[i], '" href="#" title="Copy to whole column"><i class="icon-chevron-down"></i></a>
                            </div>
                        </div>
                        <div class="div_prior div_setup" style="display:none">
                            <div class="input-append">
                                <input type="number"  style="width:55px" value=0 title="Set priority here...<br />Line plots: Higher number first in legend, lower plot on top.<br />Heatmaps: Higher number left." data-placement="left"/> 
                                <a class="btn hhdr havettp" data-who="prior" href="#" title="Copy to whole column"><i class="icon-chevron-down"></i></a>
                            </div> 
                        </div> 
              
                        <div class="div_inc div_setup"  style="display:none; min-width:91px">
                            <div class="input-append">
                                <select  style="width:70px" title="Include for sorting or clustering." data-placement="left"><option value="true" selected="selected">Include</option><option value="false">Exclude</option></select>
                                <a class="btn hhdr havettp" data-who="inc" href="#" title="Copy to whole column"><i class="icon-chevron-down"></i></a>
                            </div> 
                        </div>
              
                        <div class="div_min div_max div_setup"   style="display:none; min-width:127px"">
                            <input class="hdr-numeric-auto-input" data-who="min" type="number" style="width:40px" title="Heatmap MIN limit for whole column"/> - 
                            <input class="hdr-numeric-auto-input" data-who="max" type="number" style="width:40px" title="Heatmap MAX limit for whole column"/>
                        </div>
                        </div> ',
                        "</th>", sep='')
                }
	
				cat('</thead>')
				
				cat('<tbody>')
				for ( j in 1:length(grfile) ) {
					cat("<tr>")
					cat(
                        "<td>",
                        '<p style="white-space:nowrap; margin-right:27px;"><a class="btn btn-mini pull-left grid-row-select-btn havettp" href="#" style="margin-right: 5px" title="Toggle row selection" data-placement=left>
                        <i class="icon-check icon-large"></i></a>',
                        names(grfile)[j], '</p>'
					)
                     cat(
                        '<div class="rhdrs" style="text-align: right;">',
                        '<div class="div_separator div_setup"  style="display:none; min-width:90px;"><hr /></div>
                        <div class="div_color div_setup"  style="display:none">
                            <div class="input-append">
                                <input style="width:80px" type="color" name="plot_col" class="color {hash:true}" title="Set a color for whole row." data-placement="left"/>  
                                <a class="btn rhdr havettp" data-who="color" href="#" title="Copy to whole row"><i class="icon-chevron-right"></i></a>
                            </div>                        
                        </div> 
                        <div class="div_label div_setup" style="display:none">
                            <div class="input-append">
                                <input id="rhdrs_', j, '"style="width:80px" type="text" placeholder="', gsub('\\..+$', '', names(grfile)[j]), '" title="Set a header label here." data-placement="left"/> 
                                <a class="btn rhdr havettp" data-who="label" data-fname="', gsub('\\..+$', '', names(grfile)[j]), '" href="#" title="Copy to whole row"><i class="icon-chevron-right"></i></a>
                            </div>
                        </div>
                        <div class="div_prior div_setup" style="display:none">
                            <div class="input-append">
                                <input style="width:80px" type="number" value=0 title="Set priority here...<br />Line plots: Higher number first in legend, lower plot on top.<br />Heatmaps: Higher number left." data-placement="left"/> 
                                <a class="btn rhdr havettp" data-who="prior" href="#" title="Copy to whole row"><i class="icon-chevron-right"></i></a>
                            </div> 
                        </div> 
              
                        <div class="div_inc div_setup"  style="display:none; min-width:91px">
                            <div class="input-append">
                                <select style="width:90px" title="Include for sorting or clustering." data-placement="left"><option value="true" selected="selected">Include</option><option value="false">Exclude</option></select>
                                <a class="btn rhdr havettp" data-who="inc" href="#"  title="Copy to whole row"><i class="icon-chevron-right"></i></a>
                            </div> 
                        </div>

                        <div class="div_min div_max div_setup"   style="display:none; min-width:127px"">
                            <input class="rdr-numeric-auto-input" data-who="min" type="number" style="width:40px" title="Heatmap MIN limit for whole row"/> - 
                            <input class="rdr-numeric-auto-input" data-who="max" type="number" style="width:40px" title="Heatmap MAX limit for whole row"/>
                        </div> 
                        </div> ',
                        '</div>',
                        "</td>", sep=''
                    )
					for (i in 1:length(grfile[[j]])) {
					    at_txt <- paste0( gsub('\\..+$', '', names(grfile[[1]])[i]), ' @ ', gsub('\\..+$', '', names(grfile)[j]) )
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
                  <input type="text"   id="label_',i,'x', j,'" data-at="', at_txt, '" style="width:60px" placeholder="Label..." title="Set a label here." data-placement="left"/> </div> ', sep='')
              
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
				cat('<tr><td style="border: 0px"><br>
                    <a style="width:220px;" class="btn btn-small pull-left toogle-sel-btn-vis" href="#"><i class="icon-eye-close icon-large"></i> Show/hide selection buttons</a>
                    <br />
                    <a style="display:none; width:220px" class="btn btn-small pull-left toogle-row-btn-vis div_setup div_separator" href="#"><i class="icon-list icon-fixed-width icon-large"></i> Show/hide row setup tools</a>
                    <br />
                    <a style="display:none; width:220px" class="btn btn-small pull-left toogle-col-btn-vis div_setup div_separator" href="#"><i class="icon-list icon-fixed-width icon-rotate-90 icon-large"></i> Show/hide column setup tools</a>
                    </td></tr>')
				cat('</tbody>')
				cat('</table>')
				
        #cat('<script> if( !Modernizr.inputtypes.color ) { jscolor.bind() } </script>')
				cat('<script>')
				  cat(readLines(file.path(Sys.getenv("web", '.'), 'www/js/afterHTMLgridrender.js'), warn = FALSE, encoding = "UTF-8"))
				  
				cat('</script>')
				cat('<script>')
				if( length(controls) ) {
				    cat( "$('.div_separator').show().children().tooltip();")
                    cat( paste0("$('.div_",  controls , "').show().children().tooltip();"  ) )
				}
				cat('</script>')
                
			})
	return(HTML(html.text))
}
