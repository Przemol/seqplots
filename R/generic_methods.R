#' Generic plot function for SeqPlots package calsses
#'
#' 
#' @family PlotFunctions
#' @export
#' 
setGeneric('plot')

#' @describeIn plot Method plot for signature 'PlotSetPair'
#' @include PlotSetPair-class.R
setMethod(plot, c("PlotSetPair"), function(x, ...) x$plot(...) )

#' @describeIn plot Method plot for signature 'PlotSetList'
#' @include PlotSetList-class.R
setMethod(plot,   c("PlotSetList"), function(x, ...) x$plot(...) )




#' @rdname Extract
#' @include PlotSetList-class.R
setMethod("[", c("PlotSetList", "ANY"), function(x, i, ...) x$get(i) )

#' @rdname Extract
#' @include PlotSetList-class.R
setMethod("[[", c("PlotSetList", "ANY"), function(x, i, ...) {
    if(length(i) > 1 ) stop('recursive indexing not allowed')
    do.call(PlotSetPair, x$data[[i]])
})
