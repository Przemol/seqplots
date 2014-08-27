#' PlotSetList Reference Class
#'
#' @field data a nested list holding the data
#' @field annotations list of annotations
#' 
#' @include PlotSetPair-class.R
#' 
PlotSetList <- setRefClass("PlotSetList", fields = list( data = "list", annotations = "list")  )
PlotSetList$methods( npaires = function() length(data) )
PlotSetList$methods( info = function() as.data.frame(t(as.data.frame( 
    sapply(1:length(data), function(x) c(x, gsub('\n@', ' @ ', data[[x]]$desc))), 
    row.names=c('ID', 'Pair name') ))) ) 
PlotSetList$methods( show = function() {
    cat( 'PlotSetList with', npaires(), 'feature/tracks pairs.\nContain:\n' ) 
    print(info()); return(NULL);
})
PlotSetList$methods( get = function(i) PlotSetList(data=data[i]) )
PlotSetList$methods( plot = function(what='a', ...) {
    if (what=="a") plotAverage(data, ...) else if (what=="h") plotHeatmap(data, ...) 
    else message('Unknown type of the plot, use what="a" for average plot and what="h" for heatmap')
})

setMethod("[", c("PlotSetList", "ANY"), function(x, i, ...) x$get(i) )
setMethod("[[", c("PlotSetList", "ANY"), function(x, i, ...) {
    if(length(i) > 1 ) stop('recursive indexing not allowed')
    do.call(PlotSetPair, x$data[[i]])
})
setGeneric('plot')
setMethod(plot,   c("PlotSetList"), function(x, ...) x$plot(...) )
