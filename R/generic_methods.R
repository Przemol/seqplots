#' Generic method for SeqPlots package
#'
#' @name seqplots-generic
#' @seealso Extract
NULL


#' Generic plot function for SeqPlots package calsses
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


#' @describeIn plot Method plot for signature 'PlotSetPair'
#' @include PlotSetList-class.R
setMethod(plot,   c("PlotSetArray"), function(x, ...) x$plot(...) )



#' @rdname seqplots-generic
#' @include PlotSetList-class.R
setMethod("[", c("PlotSetList", "ANY"), function(x, i, ...) x$get(i) )

#' @rdname seqplots-generic
#' @include PlotSetList-class.R
setMethod("[[", c("PlotSetList", "ANY"), function(x, i, ...) {
    if(length(i) > 1 ) stop('recursive indexing not allowed')
    do.call(PlotSetPair, x$data[[i]])
})

#' @rdname seqplots-generic
#' @include PlotSetList-class.R
setMethod("[", signature(x = "PlotSetArray", i = "ANY", j = "missing"),
    function (x, i, j, ...) {
        if((na <- nargs()) == 2)
              x$getByID(i)
          else if(na == 3)
              x$get(i, 1:x$ntracks())
          else stop("invalid nargs()= ",na)
    }
)

#' @rdname seqplots-generic
#' @include PlotSetArray-class.R
setMethod("[", c("PlotSetArray", "ANY", "vector"), function(x, i, j) x$get(i, j) )

#' @rdname seqplots-generic
#' @include PlotSetArray-class.R
setMethod("[[", c("PlotSetArray", "ANY"), function(x, i, ...) {
    if(length(i) > 1 ) stop('recursive indexing not allowed')
    do.call(PlotSetPair, x$getByID(i)$data[[1]])
})

#' @rdname seqplots-generic
#' @include PlotSetArray-class.R
setMethod(unlist, c("PlotSetArray"), function(x) x$unlist() )
