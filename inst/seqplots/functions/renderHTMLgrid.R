# TODO: Add comment
# 
# Author: przemol
###############################################################################
renderTopLeft <- function() {
    cat(
        '<b>
        <button class="btn btn-default btn-xs grid-all-toggle-btn havettp" href="#" style="margin-right: 5px" title="Toggle all selections" data-placement=top>
        <i class="icon-check icon-large"></i></button>
        <button class="btn btn-default btn-xs grid-all-select-btn havettp" href="#" style="margin-right: 5px" title="Select all" data-placement=top>
        <i class="icon-ok-circle icon-large"></i></button>
        <button class="btn btn-default btn-xs grid-all-remove-btn havettp" href="#" style="margin-right: 5px" title="Select none" data-placement=top>
        <i class="icon-remove-circle icon-large"></i></button>
        Features: 
        </b>'
    )
    cat('
        <div class="hhdrs"  style="width: 200px; margin-left:auto; margin-right:auto;"> 
            <div class="div_separator div_setup"  style="display:none;"><hr /></div>
            <div class="div_color div_setup form-group form-group-sm"  style="display:none">

                <div class="input-group" style="width: 112px;">
                    <input class="form-control" type="color" name="plot_col" class="color {hash:true}" title="Set a color for whole row." data-placement="left"/>  
                    <span class="input-group-btn">
                        <button class="btn btn-sm btn-default chdr havettp" data-who="color" href="#" title="Copy color to all" data-placement=top><i class="icon-fullscreen"></i></button>
                    </span>
                </div>                        
            </div> 
            <div class="div_label div_setup form-group form-group-sm" style="display:none">
                <div class="input-group">
                    <input class="form-control"  type="text" placeholder="All labels" title="Set a header label here." data-placement="left"/> 
                    <span class="input-group-btn">
                        <a class="btn btn-sm btn-default chdr havettp" data-who="label" href="#" title="Copy label to all" data-placement=top><i class="icon-fullscreen"></i></a>
                        <a class="btn btn-sm btn-default chdrrowcol havettp" data-who="color" href="#" title="Construct labels using row and col fields" data-placement=top>@</a>
                        <a class="btn btn-sm btn-default chdrat havettp" data-who="color" href="#" title="Set labels to default" data-placement=top>D</a>
                        <a class="btn btn-sm btn-default chdrrm havettp" data-who="color" href="#" title="Clear all labels" data-placement=top>X</a>
                    </span>
                </div>
            </div>
            <div class="div_prior div_setup form-group form-group-sm" style="display:none">
                <div class="input-group" style="width: 112px;">
                    <input class="form-control"  type="number"  value=0 title="Set priority here...<br />Line plots: Higher number first in legend, lower plot on top.<br />Heatmaps: Higher number left." data-placement="left"/> 
                    <span class="input-group-btn">        
                        <a class="btn btn-sm btn-default chdr havettp" data-who="prior" href="#" title="Copy priority to all"><i class="icon-fullscreen"></i></a>
                    </span>
                </div> 
            </div> 
            
            <div class="div_inc div_setup form-group form-group-sm"  style="display:none; min-width:91px">
                <div class="input-group" style="width: 112px;">
                    <select title="Include for sorting or clustering." data-placement="left" class="form-control"><option value="true" selected="selected">Include</option><option value="false">Exclude</option></select>
                    <span class="input-group-btn">
                        <a class="btn btn-sm btn-default chdr havettp" data-who="inc" href="#" title="Copy value to all"><i class="icon-fullscreen"></i></a>
                    </span>
                </div> 
            </div>
            
            <div class="div_min div_max div_setup form-group form-group-sm"   style="display:none; width:200px">
                <input class="chdr-numeric-auto-input form-control" data-who="min" type="number" style="width:60px" title="Heatmap MIN for all heatmaps"/> - 
                <input class="chdr-numeric-auto-input form-control" data-who="max" type="number" style="width:60px" title="Heatmap MAX for all heatmaps"/>
     
                            </div>
        </div>'
    )
    
}

renderHEADER <- function(grfile, i, j) {
    cat('<button class="btn btn-default btn-xs grid-col-select-btn havettp" href="#" title="Toggle column selection" data-placement=bottom>')
    cat('<i class="icon-check icon-large"></i></button>')
    cat('<span><span><span>')
    cat(names(grfile[[1]])[i])
    cat('</span></span></span>')
    cat(sep='', '
        <div class="hhdrs" style="width: 100px; margin-left:auto; margin-right:auto;"> 
            <div class="div_separator div_setup"  style="display:none;"><hr /></div>
            
            <div class="div_color div_setup form-group form-group-sm"  style="display:none">
                <div class="input-group">
                    <input class="form-control"  type="color" name="plot_col" class="color {hash:true}" title="Set a color for whole row." data-placement="left"/>  
                    <span class="input-group-btn">
                        <a class="btn btn-sm btn-default hhdr havettp" data-who="color" href="#" title="Copy to whole column"><i class="icon-chevron-down"></i></a>
                    </span>
                </div>           
            </div> 

            <div class="div_label div_setup form-group form-group-sm" style="display:none">
                <div class="input-group">
                    <input class="form-control"  id="hhdr_', i, '" type="text" placeholder="', names(grfile[[1]])[i], '" title="Set a header label here." data-placement="left"/> 
                    <span class="input-group-btn">
                        <a class="btn btn-sm btn-default hhdr havettp" data-who="label" data-fname="', names(grfile[[1]])[i], '" href="#" title="Copy to whole column"><i class="icon-chevron-down"></i></a>
                    </span>
                </div>
            </div>

            <div class="div_prior div_setup form-group form-group-sm" style="display:none">
                <div class="input-group">
                    <input class="form-control"  type="number" value=0 title="Set priority here...<br />Line plots: Higher number first in legend, lower plot on top.<br />Heatmaps: Higher number left." data-placement="left"/> 
                    <span class="input-group-btn">
                        <a class="btn btn-sm btn-default hhdr havettp" data-who="prior" href="#" title="Copy to whole column"><i class="icon-chevron-down"></i></a>
                    </span>        
                </div> 
            </div> 
              
            <div class="div_inc div_setup form-group form-group-sm"  style="display:none;">
                <div class="input-group">
                    <select class="form-control" title="Include for sorting or clustering." data-placement="left"><option value="true" selected="selected">Include</option><option value="false">Exclude</option></select>
                    <span class="input-group-btn">
                        <a class="btn btn-sm btn-default hhdr havettp" data-who="inc" href="#" title="Copy to whole column"><i class="icon-chevron-down"></i></a>
                    </span> 
                </div> 
            </div>
              
            <div class="div_min div_max div_setup form-group form-group-sm"   style="display:none;">
                <input class="hdr-numeric-auto-input form-control" data-who="min" type="number" style="width:43px" title="Heatmap MIN limit for whole column"/> - 
                <input class="hdr-numeric-auto-input form-control" data-who="max" type="number" style="width:43px" title="Heatmap MAX limit for whole column"/>
            </div>
        </div>'
    )
}

renderROW <- function(grfile, i, j) {
    cat(sep='', '
    <p class="tabrowpar">
        <a class="btn btn-default btn-xs pull-left grid-row-select-btn havettp" href="#" style="margin-right: 5px" title="Toggle row selection" data-placement=left>
            <i class="icon-check icon-large"></i>
        </a>',
        names(grfile)[j], 
    '</p>')
    cat(sep='', '
    <div class="div_separator div_setup"  style="display:none; min-width:90px;"><hr /></div>
     <div class="rhdrs" style="width: 100px; margin-left:auto; margin-right:10px;">
    
        <div class="div_color div_setup form-group form-group-sm"  style="display:none">
            <div class="input-group">
                <input class="form-control" type="color" name="plot_col" class="color {hash:true}" title="Set a color for whole row." data-placement="left"/>  
                <span class="input-group-btn">
                    <a class="btn btn-sm btn-default rhdr havettp" data-who="color" href="#" title="Copy to whole row"><i class="icon-chevron-right"></i></a>
                </span>            
            </div>                        
        </div> 
        
        <div class="div_label div_setup form-group form-group-sm" style="display:none">
            <div class="input-group">
                <input class="form-control"  id="rhdrs_', j, '" type="text" placeholder="', gsub('\\..+$', '', names(grfile)[j]), '" title="Set a header label here." data-placement="left"/> 
                <span class="input-group-btn">
                    <a class="btn btn-sm btn-default rhdr havettp" data-who="label" data-fname="', gsub('\\..+$', '', names(grfile)[j]), '" href="#" title="Copy to whole row"><i class="icon-chevron-right"></i></a>
                </span>
            </div>
        </div>
        
        <div class="div_prior div_setup form-group form-group-sm" style="display:none">
            <div class="input-group">
                    <input class="form-control" type="number" value=0 title="Set priority here...<br />Line plots: Higher number first in legend, lower plot on top.<br />Heatmaps: Higher number left." data-placement="left"/> 
                    <span class="input-group-btn">
                        <a class="btn btn-sm btn-default rhdr havettp" data-who="prior" href="#" title="Copy to whole row"><i class="icon-chevron-right"></i></a>
                    </span>
            </div> 
        </div> 
              
        <div class="div_inc div_setup form-group form-group-sm"  style="display:none; min-width:91px">
            <div class="input-group">
                <select class="form-control" title="Include for sorting or clustering." data-placement="left"><option value="true" selected="selected">Include</option><option value="false">Exclude</option></select>
                <span class="input-group-btn">
                    <a class="btn btn-sm btn-default rhdr havettp" data-who="inc" href="#"  title="Copy to whole row"><i class="icon-chevron-right"></i></a>
                </span>    
            </div> 
        </div>

        <div class="div_min div_max div_setup form-group form-group-sm"   style="display:none;">
            <input class="rdr-numeric-auto-input form-control" data-who="min" type="number" style="width:43px" title="Heatmap MIN limit for whole row"/> - 
            <input class="rdr-numeric-auto-input form-control" data-who="max" type="number" style="width:43px" title="Heatmap MAX limit for whole row"/>
        </div> 
    </div>')
}

renderCellControlls <- function(grfile, i, j, cltab) {
    at_txt <- paste0( gsub('\\..+$', '', names(grfile[[1]])[i]), ' @ ', gsub('\\..+$', '', names(grfile)[j]) )
    cat('<div class="div_separator div_setup"  style="display:none; min-width:90px;"><hr /></div>')
    
    cat('<div class="div_color div_setup form-group form-group-sm"  style="display:none">
                        <input class="form-control"  type="color" name="plot_col" id="color_',i,'x', j,'" class="color {hash:true}", value="',cltab[i,j],'" style="width:60px;" title="Set a color here." data-placement="left"/>  </div> ', sep='')
    
    cat('<div class="div_label div_setup form-group form-group-sm"  style="display:none">
                      <input class="form-control"  type="text"   id="label_',i,'x', j,'" data-at="', at_txt, '" style="width:60px" placeholder="Label..." title="Set a label here." data-placement="left"/> </div> ', sep='')
    
    cat('<div class="div_prior div_setup form-group form-group-sm" style="display:none">
                      <input class="form-control"  type="number" id="prior_',i,'x', j,'" style="width:60px" value=0 title="Set priority here...<br />Line plots: Higher number first in legend, lower plot on top.<br />Heatmaps: Higher number left." data-placement="left"/> </div> ', sep='')
    
    cat('<div class="div_inc div_setup form-group form-group-sm"  style="display:none; min-width:91px">
                      <select class="form-control" id="inc_',i,'x', j,'" style="width:85px" title="Include for sorting or clustering." data-placement="left"><option value="true" selected="selected">Include</option><option value="false">Exclude</option></select></div> ', sep='')
    
    cat('<div class="div_min div_max div_setup form-group form-group-sm"   style="display:none; min-width:127px"">
                      <input class="form-control"  type="number"     id="min_',i,'x', j,'" style="width:40px" title="Heatmap MIN limit."/> - <input class="form-control"  type="number"     id="max_',i,'x', j,'" style="width:40px" title="Heatmap MAX limit.."/> </div> ', sep='')
}


renderHTMLgrid <- function(grfile, CC, checked=NULL, addcls='', controls=NULL) {
	
	#cltab <- suppressWarnings(matrix(rgb(t(col2rgb(colors()[grep("dark",colors())])) , maxColorValue=255), length(grfile), length(grfile[[1]])))
	cls <- c("darkblue", "darkgreen", "darkred", "darkmagenta", "darkgray", "darkorange", "darkcyan", "black", rainbow((length(grfile)*length(grfile[[1]]))-8))[1:(length(grfile)*length(grfile[[1]]))]
	cltab <-matrix( rgb(t(col2rgb(cls)), maxColorValue=255), length(grfile[[1]]), length(grfile)  )
	html.text <- capture.output( {
	    cat('<div id="plot_this" class="shiny-input-checkboxgroup shiny-input-container">')
	    cat('<table id="plotTable" class="',addcls,'" style="margin-left: auto; margin-right: auto; text-align: center;">')
	        cat('<thead>')
	            cat('<tr>')
	                cat('<th>')
	                renderTopLeft()
	            cat('</th>')
	    
	            for(i in 1:length(names(grfile[[1]]))) {
	                cat('<th>')
	                renderHEADER(grfile, i, 0) 
	             cat("</th>")
	    }
	    cat('</thead>')
	    
	    cat('<tbody>')
    	    for ( j in 1:length(grfile) ) {
    	        cat("<tr>")
    	        cat("<td>")
    	            renderROW(grfile, 0, j)
    	        cat("</td>")
    	        
    	        for (i in 1:length(grfile[[j]])) {
    	            cat('<td>')
    	            if ( if(!is.null(checked)) if( any( apply(checked, 1, identical, c(i,j)) ) ) TRUE else FALSE else FALSE ) {
    	                cat('<input type="checkbox" name="plot_this" value="[',i,',',j,']" checked />', sep='')
    	            } else {
    	                cat('<input type="checkbox" name="plot_this" value="[',i,',',j,']" />', sep='')
    	            }
    	            if (CC) {
    	                renderCellControlls(grfile, i, j, cltab)
    	            }
    	            cat('</td>')
    	        }	
    	        cat("</tr>")
    	    }
    	    cat('<tr>
                <td style="border: 0px"><br>
                <a style="width:220px;" class="btn btn-default btn-sm pull-left toogle-sel-btn-vis" href="#"><i class="icon-eye-close icon-large"></i> Show/hide selection buttons</a>
                <br />
                <a style="display:none; width:220px" class="btn btn-default btn-sm pull-left toogle-row-btn-vis div_setup div_separator" href="#"><i class="icon-list icon-fixed-width icon-large"></i> Show/hide row setup tools</a>
                <br />
                <a style="display:none; width:220px" class="btn btn-default btn-sm pull-left toogle-col-btn-vis div_setup div_separator" href="#"><i class="icon-list icon-fixed-width icon-rotate-90 icon-large"></i> Show/hide column setup tools</a>
                </td>
                </tr>')
    	    cat('</tbody>')
	    cat('</table>')
	    cat('</div>')
	    
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
