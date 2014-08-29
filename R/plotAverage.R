#' Create the average plot
#' 
#' @inheritParams plotMext
#'
#' @param plotset The dataset to plot - can be \code{\link{PlotSetArray}}, 
#' \code{\link{PlotSetLits}}, \code{\link{PlotSetPair}} or
#' properly formated \code{\link[base]{list}}
#' 
#' @param main an overall title for the plot: see title.
#' @param xlab a title for the x axis: see title.
#' @param ylab a title for the y axis: see title.
#' @param plotScale scale the avilable data before ploting, can be "linear" (do not scale), "log2" or "zscore"
#' @param xlim the x limits (x1, x2) of the plot. Note that x1 > x2 is allowed and leads to a ‘reversed axis’.
#' The default value, NULL, indicates that the whole range present in \code{plotset} will be plotted.
#' @param ylim the y limits (y1, y2) of the plot. Note that x1 > x2 is allowed and leads to a ‘reversed axis’.
#' The default value, NULL, indicates that the range will be auto calculated including space for error estimates. 
#' @param type If set to "legend" only the legend/key will be plotted. 
#' @param error.estimates Indicates if error estimates are plotted, default: TRUE
#' @param legend Indicates if key descibing the PlotSetPairs is shown, default: TRUE
#' @param legend_ex Indicates if key descibing error estimates is shown, default: FALSE
#' @param legend_pos The position of main key, default: 'topright'
#' @param legend_ext_pos The position of error estimates key,  default: 'topleft'
#' @param cex.axis Axis numbers font size in points, default: 14
#' @param cex.lab Axis labels labes font size in points, default: 16
#' @param cex.main Main title font size in points, default: 20
#' @param cex.legend Keys labels font size in points, default: 10
#' @param ln.v =TRUE, 
#' @param ln.h =NULL, 
#' @param colvec =NULL, 
#' @param poinsize =12, 
#' @param postproc =function(){}
#' 
#' 
#' 
#' 
#' @inheritParams plot
#' @keywords internal
#'    
#' 
#' @param keepratio
#' @param ord
#' @param labels
#' 
#' 
#' @return NULL
#' 
#' @details
#' Soem details.
#' 
#' 
#' 
#' 
#' @author Przemyslaw Stempor
#' @export
#' 
#' @examples
#' 
#' 
#' \dontrun{
#' ## Run BEADS for BSgenome package, the reference genome package have to be installed prior to running this example
#' # source("http://bioconductor.org/biocLite.R")
#' # biocLite("BSgenome.Celegans.UCSC.ce10")
#' # library(BSgenome.Celegans.UCSC.ce10)
#' beads(sample_bam, SummedInput_bw, map_bw, genome='ce10')
#' 
#' ## Run BEADS for all BAM files in the directory
#' #lapply(dir(pattern='bam$'), beads, control=input, mappability=map_bw, genome=ref_fa)
#' }
#' 
setGeneric("plotAverage",
           function(plotset, ...) standardGeneric("plotAverage")
)

#' @describeIn plotAverage Method for signature plotset='list'
setMethod("plotAverage", signature(plotset='list'),
    function(plotset, keepratio=FALSE, ord=NULL, labels=NULL, ...) {
      opar <- par(no.readonly = TRUE)['pty']
      if(keepratio) par(pty='s')
          if( length(labels) ) {
              labels <- labels[1:length(plotset)]
              plotset <- Map(function(x, y) {if(!is.na(y)) x[['desc']]<-y; return(x)}, plotset, labels)
          }
      if( length(ord) ) { plotset <- plotset[ ord ] }
      plotMext(plotset, ...) 
      par(opar)
      return(invisible(NULL))
    }
)

#' @describeIn plotAverage Method for signature plotset='PlotSetPair'
#' @include PlotSetPair-class.R
setMethod("plotAverage", signature(plotset='PlotSetPair'),
          function(plotset, ...) {
              plotAverage(list(plotset), ...)
          }
)

#' @describeIn plotAverage Method for signature plotset='PlotSetList'
#' @include PlotSetList-class.R
setMethod("plotAverage", signature(plotset='PlotSetList'),
          function(plotset, ...) {
              plotAverage(plotset$data, ...)
          }
)

#' @describeIn plotAverage Method for signature plotset='PlotSetArray'
#' @include PlotSetArray-class.R
setMethod("plotAverage", signature(plotset='PlotSetArray'),
          function(plotset, ...) {
              plotAverage(unlist(plotset)$data, ...)
          }
)


