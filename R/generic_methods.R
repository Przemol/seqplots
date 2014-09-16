#' SeqPlots generic methods
#' 
#' Generic operators working with \code{\link{PlotSetArray}}, 
#' \code{\link{PlotSetList}} and \code{\link{PlotSetPair}} to subset 
#' or flatten the data structure.
#' 
#' @param x an object of class \code{\link{PlotSetArray}}, 
#'  \code{\link{PlotSetList}} or \code{\link{PlotSetPair}} 
#' @param j see description for \code{i}
#' @param ... see description for \code{i}
#' 
#' @return
#' 
#' If \code{x} is \strong{\code{\link{PlotSetArray}}} class:
#' \itemize{
#'  \item \code{x[1:2,1:2]} produces \code{\link{PlotSetArray}} with 2 feature(s) and 
#'  2 tracks. 
#'  \item \code{x[1:2]} produces \code{\link{PlotSetList}} with 2 feature/tracks 
#'  pairs. 
#'  \item \code{x[[1]]} produces single \code{\link{PlotSetPair}}. 
#'  \item \code{unlist(x)} produces \code{\link{PlotSetList}} with all
#'   feature/tracks pairs.
#'   \item \code{x$as.array()} produces the matrix of \code{\link{PlotSetPair}} 
#'   classes with all feature/tracks pairs.
#' }
#' 
#' If \code{x} is \strong{\code{\link{PlotSetList}}} class:
#' \itemize{
#'  \item \code{x[1:2]} produces \code{\link{PlotSetList}} with 2 feature/tracks 
#'  pairs. 
#'  \item \code{x[[1]]} produces single \code{\link{PlotSetPair}}.
#' }
#'
#' 
#' @inheritParams base::Extract
#' @name seqplots-generic
#' @seealso \code{\link[base]{Extract}}
NULL


#' Generic plot function for SeqPlots package calsses
#' 
#' @param x This argument should be one of SeqPlots classes:
#'  \code{\link{PlotSetArray}}, \code{\link{PlotSetList}} or
#'  \code{\link{PlotSetPair}}
#' @param y For plotting SeqPlots classes this argument is ignored, used for 
#'  default functionality of \code{\link[graphics]{plot}} function.
#' @param what This argument takes a character determining if avareange 
#'  plot (\code{"a"}, default) or heatmap (\code{"h"}) will be plotted.
#' @param ... Other parameters controlong the plot, see \code{\link{plotAverage}}
#'  for avareange plot and \code{\link{plotHeatmap}} for heatmaps.
#' 
#' @return
#' Returns \code{NULL} for avareange plot and cluster report \code{data.frame} 
#' for hetamap - see \code{\link{plotHeatmap}} for details.  
#'  
#' @seealso \code{\link{getPlotSetArray}}
#'  
#' @family plotting functions
#' @export
#' 
setGeneric('plot')

#' @describeIn plot Method plot for signature 'PlotSetPair'
#' @include PlotSetPair-class.R
setMethod(plot, c("PlotSetPair"), function(x, what='a', ...) x$plot(what=what, ...) )

#' @describeIn plot Method plot for signature 'PlotSetList'
#' @include PlotSetList-class.R
setMethod(plot,   c("PlotSetList"), function(x, what='a', ...) x$plot(what=what, ...) )

#' @describeIn plot Method plot for signature 'PlotSetPair'
#' @include PlotSetList-class.R
setMethod(plot,   c("PlotSetArray"), function(x, what='a', ...) x$plot(what=what, ...) )


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
