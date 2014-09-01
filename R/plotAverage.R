#' Create the average plot
#' 
#' Draw an average plot from \code{\link{PlotSetArray}}, 
#' \code{\link{PlotSetLits}}, \code{\link{PlotSetPair}} or
#' properly formated \code{\link[base]{list}} in active graphics window.
#' Axes and titles, keys and other plot elements are controled by function parameters.
#' 
#' @param plotset The dataset to plot - can be \code{\link{PlotSetArray}}, 
#' \code{\link{PlotSetLits}}, \code{\link{PlotSetPair}} or
#' properly formated \code{\link[base]{list}}
#' @param main The main title of the plot, shown in top-center part of the figure; defaults to NULL (not visible)
#' @param xlab Label shown below horizontal axis; default to \code{""} (empty)
#' @param ylab Label shown below vertical axis; default to \code{""} (empty)
#' @param labels The character vector giving labes used in experiment key. The defaults NULL value
#'  indicates taht feature/track file names will be used to generate the labes.
#' @param ord The numeric vector determinin the plotting order of experiments. 
#'  Feature-track pair with the highest priority will be listed on the top of key.
#'  If NULL (default) the order established in \code{plotset} is used.
#' @param keepratio If TRUE keep 1:1 aspect ratio of the figure; defaults to FALSE
#' @param plotScale scale the avilable data before ploting, can be "linear" (do not scale, default), "log2" or "zscore"
#' @param xlim the x limits (x1, x2) of the plot. Note that x1 > x2 is allowed and leads to a ‘reversed axis’.
#'  The default value, NULL, indicates that the whole range present in \code{plotset} will be plotted.
#' @param ylim the y limits (y1, y2) of the plot. Note that x1 > x2 is allowed and leads to a ‘reversed axis’.
#'  The default value, NULL, indicates that the range will be auto calculated including space for error estimates. 
#' @param type If set to "legend" only the legend/key will be plotted. 
#' @param error.estimates Indicates if error estimates are plotted, defaults to TRUE
#' @param legend Indicates if key descibing the PlotSetPairs is shown, defaults to TRUE
#' @param legend_ex Indicates if key descibing error estimates is shown, defaults to FALSE
#' @param legend_pos The position of main key, defaults to 'topright'
#' @param legend_ext_pos The position of error estimates key, defaults to 'topleft'
#' @param cex.axis Axis numbers font size in points, defaults to 14
#' @param cex.lab Axis labels labes font size in points, Defaults to 16
#' @param cex.main Main title font size in points, defaults to 20
#' @param cex.legend Keys labels font size in points, defaults to 10
#' @param ln.v Determins if vertical gide line(s) should be plotted (TRUE) or ommitted (FALSE).
#'  For anchored plots 2 lines indicateing the start and end of anchored distane are plotted.
#' @param ln.h Determins if horzontal gide line should is plotted. Numeric value of the parametter
#'  indicates the Y-axis position of the lie, NULL (default) indicates to ommit 
#' @param colvec The vector of colors used to plot the lines and error estimate fields.
#'  If set value NULL (default) the automatically generated color vaues will be used.
#'  Accpeted values are: vector of any of the three kinds of R color specifications, i.e.,
#'  either a color name (as listed by colors()), a hexadecimal string of the form "#rrggbb" or "#rrggbbaa" (see rgb), 
#'  or a positive integer i meaning palette()[i]. See \code{\link[grDevices]{col2rgb}}. 
#' @param pointsize The default font point size to be used for plots. Defaults to 12 (1/72 inch).
#' @param ... other graphical parameters passed to plot.default (see \code{\link[graphics]{plot.default}}, \code{\link[graphics]{par}} and section ‘Details’ below)
#' 
#' @inheritParams graphics::plot.default
#' 
#' @return NULL
#' 
#' @details
#' The relavent parameters passed to \code{\link[graphics]{plot.default}} funtion:
#' @param log a character string which contains "x" if the x axis is to be logarithmic, "y" if the y axis is to be logarithmic and "xy" or "yx" if both axes are to be logarithmic.
#' @param ann a logical value indicating whether the default annotation (title and x and y axis labels) should appear on the plot.
#' @param axes a logical value indicating whether both axes should be drawn on the plot. Use graphical parameter "xaxt" or "yaxt" to suppress just one of the axes.
#' @param frame.plot a logical indicating whether a box should be drawn around the plot.
#' @param panel.first	an ‘expression’ to be evaluated after the plot axes are set up but before any plotting takes place. This can be useful for drawing background grids or scatterplot smooths. Note that this works by lazy evaluation: passing this argument from other plot methods may well not work since it may be evaluated too early.
#' @param panel.last	an expression to be evaluated after plotting has taken place but before the axes, title and box are added. See the comments about panel.first.
#' @param asp	the y/x aspect ratio, see plot.window.
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


