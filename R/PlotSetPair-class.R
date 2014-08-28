#' PlotSetPair Reference Class
#'
#' @field data a list holding the plot data
#' @field annotations list of annotations
#' 
#' @export
#' 
PlotSetPair <- setRefClass("PlotSetPair", fields = list(means='numeric', stderror='numeric', conint='numeric', all_ind='numeric', e='ANY', desc='character', heatmap='matrix')  )
PlotSetPair$methods( show = function() {
    cat( 'PlotSetPair contatining:', gsub('\n@', ' @ ', desc) ) 
})
PlotSetPair$methods( as.list = function() {
    list(means=means, stderror=stderror, conint=conint, all_ind=all_ind, e=e, desc=desc, heatmap=heatmap)
})
PlotSetPair$methods( plot = function(what='a', ...) {
    if (what=="a") plotAverage(list(.self), ...) else if (what=="h") plotHeatmap(list(.self), ...) 
    else message('Unknown type of the plot, use what="a" for average plot and what="h" for heatmap')
})
setGeneric('plot')
setMethod(plot, c("PlotSetPair"), function(x, ...) x$plot(...) )
