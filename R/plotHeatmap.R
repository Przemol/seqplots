#' Plot heatmap with optional clustering
#'
#'This function plots the heatmap from plot array.
#'
#' @param tracks list or vector of track paths (BigWig) and/or motif 
#' setup structers (as \code{\link{list}})
#' @param features vector of feature file paths (BED or GFF)
#' @param bin binning window size in base pairs, default 1L
#' @param rm0 remove 0 from mean/median calculation, default FALSE
#' @param x1 upsterem distance in base pairs, default 500L
#' @param x2 downsteram distance in base pairs, default 2000L
#' @param xm anchored distance in base pairs, default 1000L
#' @param type the type of the calculation, default, 'Point Features'
#' @param add_heatmap return the heatmap data,  default FALSE
#' @param cat3 function, that sends 1st level info to web interface
#' @param cat4 function, that sends 2nd level info to web interface
#' @param con connection to SQlite database storing genome information for files
#' @return nested list: OUT[[FEATURE]][[TRACK/MOTIF]][[VALUE]]
#' 
#' @export
#' 

plotHeatmap <- function(pl, title="", labels=NA, legend=TRUE, keepratio=FALSE, 
    ord=1:length(pl), scale_signal="no", sortrows=FALSE, clusters=5L,
    clstmethod="kmeans", include=rep(TRUE, length(pl)), ssomt1=2L, ssomt2=2L, 
    title_font_size=16, ...) {
  
  if(keepratio) par(pty='s')
  
  if( is.null(pl[[1]]$heatmap) ) 
    stop('Heatmap plotting: No heatmap data avilabe! Re-run with "Calculate Heatmap" option selected.', call.=FALSE)
  if(length(unique(sapply(pl, function(x) nrow(x[['heatmap']])))) != 1) 
    stop('Heatmap plotting: All plots must have equal number of features. Do not plot heatmaps on multiple GFF/BED.', call.=FALSE)
  
  #Heatmap data aquizition (as list of matrixes)
  HLST <- lapply(pl, '[[', 'heatmap')[ ord ]  
  
  #Optional scalling
  if ( scale_signal ==  "log2" ) {
    HLST <- lapply(HLST, log2 )
  } else if ( scale_signal == "zscore" ) {
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
    
    ssom <- supersom(Hlist, grid = somgrid(xdim = ssomt1, ydim = ssomt2, "hexagonal"), rlen = 100, toroidal=TRUE)
    classes <- ssom$unit.classif
    cls_order <- order(ssom$unit.classif)
    
    finalOrd <- finalOrd[cls_order]
    clusts <- table(classes)
    
  } else {
    values$clusters <- NULL
    clusts <- NULL
  }
  
  HLST <- lapply(HLST, function(x) { return(x[finalOrd, ]) } )
  
  lab <- sapply(pl, '[[', 'desc')
  labels <- labels[1:length(pl)]
  lab[!is.na(labels)] <- labels[!is.na(labels)]
  
  
  if( nchar(title) > 0 ) par(oma=c(0,0,(title_font_size/12)+1,0) )
  
  .heatmapPlotWrapper( HLST, clusts, 
                      bins=pl[[1]]$all_ind, 
                      titles=lab, e=pl[[1]]$e, ...)
  title(title, outer = TRUE, cex.main=title_font_size/12)
}
