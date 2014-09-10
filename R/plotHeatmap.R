#' Plot heatmap with optional clustering
#'
#' Draw the heatmap plot from \code{\link{PlotSetArray}}, 
#' \code{\link{PlotSetList}}, \code{\link{PlotSetPair}} classes or
#' properly formated \code{\link[base]{list}} (see details) in active graphics window.
#' Axes and titles, keys and other plot elements are controled by function parameters.
#' 
#' 
#' @param plotset The dataset to plot - can be \code{\link{PlotSetArray}}, 
#' \code{\link{PlotSetList}}, \code{\link{PlotSetPair}} or
#' properly formated \code{\link[base]{list}}
#' @param clstmethod Determines the heatmap clusering algoritm "kmeans" for k-means (default, see \code{\link[stats]{kmeans}}), 
#'  "hclust" (see \code{\link[stats]{hclust}}) for hierarchical clustering, "ssom" for 
#'  (super) self organising map (see \code{\link[kohonen]{supersom}}) with trical topology and "none"
#'  of FALSE to turn off the clustering
#' @param clusters The number of claster for "kmeans" and "hclust", ignored for "ssom", defaults to 5L
#' @param ssomt1 Determins , determins the dimentionality of SOM - number of neurons in 1st dimention,
#'  numbbers of resulting cluters equils ssomt1*ssomt2, defaults to 2L
#' @param ssomt2 Determins , determins the dimentionality of SOM - number of neurons in 2st dimention,
#'  numbbers of resulting cluters equils ssomt1*ssomt2, defaults to 2L
#' @param include The logical vector indicating if given subplot should influence 
#' clustering and sorting, if given element is FALSE the sub-heatmap will be still ploted,
#' and the order of data rows will be determined vy clusterin/plottin other sub-heatmaps, 
#' defaults to \code{rep(TRUE, length(plotset))}
#' @param sortrows If TRUE the rows of heatmap will be sorted by mean value 
#'  across all heatmaps, defaults to FALSE
#' @param main The main title of the plot, shown in top-center part of the figure; defaults to NULL (not visible)
#' @param labels The character vector giving sub-titles of heatmaps (plotted over the heatmap and below the main title). 
#'  The defaults NULL value indicates taht feature/track file names will be used to generate the sub-titles.
#' @param ord The numeric vector determinin the plotting order of experiments. 
#'  The heatmap prepesenting feature-track pair with the highest priority will be 
#'  plotted leftmost. If NULL (default) the order established in \code{plotset} is used.
#' @param plotScale scale the avilable data before ploting, can be "linear" (do not scale, default), "log2" or "zscore"
#' @param legend if TRUE plot the color key
#' @param keepratio If TRUE keep 1:1 aspect ratio of the figure; defaults to FALSE
#' @param xlab label below x-axis
#' @param ylab label below y-axis
#' @param cex.main Main title font size in points, defaults to 16
#' @param cex.axis Axis numbers font size in points, defaults to 12
#' @param cex.lab Axis labels labes font size in points, Defaults to 12
#' @param cex.legend Keys labels font size in points, defaults to 12
#' @param autoscale if TRUE the color keys will be autoscaled
#' @param zmin global minimum value on color key, ignored if \code{autoscale} is TRUE
#' @param zmax global maximum value on color key, ignored if \code{autoscale} is TRUE
#' @param xlim the x limits (x1, x2) of the plot. Note that x1 > x2 is allowed and leads to a "reversed axis".
#'  The default value, NULL, indicates that the whole range present in \code{plotset} will be plotted.
#' @param ln.v Determins if vertical gide line(s) should be plotted (TRUE) or ommitted (FALSE).
#'  For anchored plots 2 lines indicateing the start and end of anchored distane are plotted.
#' @param s The saturation value used to autoscale color key limits, defaults to 0.01 
#' @param indi If TRUE (defaults) the independent color keys will be plotted below heatmaps,
#'  if FALSE the commmon color key is shown rightmost
#' @param o_min vector of length equil to number of sub heatmaps determining minimum 
#'  value on color key for each sub plot, if NULL (default) or NA the global settinsg are used,
#'  ignored in \code{indi} is FALSE 
#' @param o_max vector of length equil to number of sub heatmaps determining maximum
#'  value on color key for each sub plot, if NULL (default) or NA the global settinsg are used, 
#'  ignored in \code{indi} is FALSE 
#' @param colvec The vector of colors used to plot the lines and error estimate fields.
#'  If set value NULL (default) the automatically generated color vaues will be used.
#'  Accpeted values are: vector of any of the three kinds of R color specifications, i.e.,
#'  either a color name (as listed by colors()), a hexadecimal string of the form "#rrggbb" or "#rrggbbaa" (see rgb), 
#'  or a positive integer i meaning palette()[i]. See \code{\link[grDevices]{col2rgb}}. 
#' @param colorspace The olorspace of the heatmap, see \code{\link[grDevices]{grDevices}}
#' @param pointsize The default font point size to be used for plots. Defaults to 12 (1/72 inch).
#' 
#' @details
#'  list[[FEATURE]][[TRACK/MOTIF]][[KEY_VALUE]]
#'  
#' @family plotting functions
#' @export
#' 
#' 
setGeneric("plotHeatmap",
           function(plotset, ...) 
               standardGeneric("plotHeatmap")
)

#' @describeIn plotHeatmap Method for signature \code{\link[base]{list}} with 
#' following format: \code{list[[FEATURE]][[TRACK/MOTIF]][[KEY_VALUE]]}
setMethod("plotHeatmap", signature(plotset='list'),
           function(plotset, main="", labels=NA, legend=TRUE, keepratio=FALSE, 
                                  ord=1:length(plotset), plotScale="no", sortrows=FALSE, clusters=5L,
                                  clstmethod="kmeans", include=rep(TRUE, length(plotset)), ssomt1=2L, ssomt2=2L, 
                                  cex.main=16, ...) {
              
              if(keepratio) par(pty='s')
              
              if( is.null(plotset[[1]]$heatmap) ) 
                  stop('Heatmap plotting: No heatmap data avilabe! Re-run with "Calculate Heatmap" option selected.', call.=FALSE)
              if(length(unique(sapply(plotset, function(x) nrow(x[['heatmap']])))) != 1) 
                  stop('Heatmap plotting: All plots must have equal number of features. Do not plot heatmaps on multiple GFF/BED.', call.=FALSE)
              
              #Heatmap data aquizition (as list of matrixes)
              HLST <- lapply(plotset, '[[', 'heatmap')[ ord ]  
              
              #Optional scalling
              if ( plotScale ==  "log2" ) {
                  HLST <- lapply(HLST, log2 )
              } else if ( plotScale == "zscore" ) {
                  HLST <- lapply(HLST, scale )
              }
              
              #Preparing flat matrix fro sorrting an clustering
              Hclc <- do.call(cbind, HLST[ include ])
              
              finalOrd <- 1:nrow(Hclc)
              
              #Sorting
              if(sortrows) { 
                  sorting_order <- order(rowMeans(Hclc, na.rm=TRUE), decreasing = TRUE) 
                  finalOrd <- finalOrd[sorting_order] 
                  Hclc <- Hclc[sorting_order,]
              } else {
                  sorting_order <- 1:nrow(Hclc)
              }
              
              #Clustering
              if(clusters > 1 & clstmethod == 'kmeans') {
                  
                  Hcl <- Hclc; Hcl[is.na(Hcl)] <- 0
                  k <- kmeans(Hcl, clusters)
                  cls_order <- order(k$cluster)
                  classes <- k$cluster
                  
                  finalOrd <- finalOrd[cls_order]
                  clusts <- k$size
                  
              } else if(clstmethod == 'hclust') {
                  
                  Hcl <- Hclc; Hcl[is.na(Hcl)] <- 0
                  cls <- hclust(dist(Hcl))
                  cls_order <- cls$order
                  classes <- cutree(cls, clusters)
                  
                  finalOrd <- finalOrd[cls_order]
                  clusts <- table(classes)
                  
              } else if(clstmethod == 'ssom') {
                  
                  Hlist <- HLST[ include ]
                  Hlist <- lapply(Hlist, function(x) {x[is.na(x)] <- 0; if(sortrows) x <- x[sorting_order,]; x} )
                  
                  ssom <- supersom(Hlist, grid = class::somgrid(xdim = ssomt1, ydim = ssomt2, "hexagonal"), rlen = 100, toroidal=TRUE)
                  classes <- ssom$unit.classif
                  cls_order <- order(ssom$unit.classif)
                  
                  finalOrd <- finalOrd[cls_order]
                  clusts <- table(classes)
                  
              } else {
                  values$clusters <- NULL
                  clusts <- NULL
              }
              
              HLST <- lapply(HLST, function(x) { return(x[finalOrd, ]) } )
              
              lab <- sapply(plotset, '[[', 'desc')
              labels <- labels[1:length(plotset)]
              lab[!is.na(labels)] <- labels[!is.na(labels)]
              
              
              if( nchar(main) > 0 ) par(oma=c(0,0,(cex.main/12)+1,0) )
              
              heatmapPlotWrapper( HLST, clusts, 
                                  bins=plotset[[1]]$all_ind, 
                                  titles=lab, e=plotset[[1]]$e, 
                                  Leg=legend, ...)
              title(main, outer = TRUE, cex.main=cex.main/12)
          }
)

#' @describeIn plotHeatmap Method for signature \code{\link{PlotSetPair}}
#' @include PlotSetPair-class.R
setMethod("plotHeatmap", signature(plotset='PlotSetPair'),
          function(plotset, ...) {
              plotHeatmap(list(plotset), ...)
          }
)

#' @describeIn plotHeatmap Method for signature \code{\link{PlotSetList}}
#' @include PlotSetList-class.R
setMethod("plotHeatmap", signature(plotset='PlotSetList'),
          function(plotset, ...) {
              plotHeatmap(plotset$data, ...)
          }
)

#' @describeIn plotHeatmap Method for signature \code{\link{PlotSetArray}}
#' @include PlotSetArray-class.R
setMethod("plotHeatmap", signature(plotset='PlotSetArray'),
          function(plotset, ...) {
              plotHeatmap(unlist(plotset)$data, ...)
          }
)

