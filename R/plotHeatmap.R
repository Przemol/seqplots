#' Plot heatmap with optional clustering
#' 
#' Draw the heatmap plot from \code{\link{PlotSetArray}}, 
#' \code{\link{PlotSetList}}, \code{\link{PlotSetPair}} classes or properly 
#' formatted \code{\link[base]{list}} (see details) in active graphics window. 
#' Axes and titles, keys and other plot elements are controlled by function 
#' parameters.
#' 
#' 
#' @param plotset The dataset to plot - can be \code{\link{PlotSetArray}}, 
#'   \code{\link{PlotSetList}}, \code{\link{PlotSetPair}} or properly formatted 
#'   \code{\link[base]{list}}
#' @param clstmethod Determines the heatmap clustering algorithm "kmeans" for 
#'   k-means (default, see \code{\link[stats]{kmeans}}), "hclust" (see 
#'   \code{\link[stats]{hclust}}) for hierarchical clustering, "ssom" for 
#'   (super) self organising map (see \code{\link[kohonen]{supersom}}) with 
#'   torus topology and "none" of FALSE to turn off the clustering
#' @param clusters The number of cluster for "kmeans" and "hclust", ignored for 
#'   "ssom", defaults to 5L
#' @param ssomt1 Determines , the dimensionality of SOM - number of neurons in 
#'   1st dimension, number of resulting clusters equals ssomt1*ssomt2, defaults 
#'   to 2L
#' @param ssomt2 Determines , the dimensionality of SOM - number of neurons in 
#'   2st dimension, number of resulting clusters equals ssomt1*ssomt2, defaults 
#'   to 2L
#' @param include The logical vector indicating if given subplot should 
#'   influence clustering and sorting, if given element is FALSE the sub-heatmap
#'   will be still plotted, and the order of data rows will be determined by 
#'   clustering/sorting other sub-heatmaps, defaults to NULL, which incluses all
#'   - equivalent to \code{rep(TRUE, length(plotset))}
#' @param sortrows If \code{"increasing"} or \code{"decreasing"} the rows of 
#'  heatmap will be sorted by mean value across all heatmaps,
#'  defaults to \code{FALSE} - not sorted. For backwards compatibility \code{TRUE} 
#'  is synonymous to "increasing".
#' @param main The main title of the plot, shown in top-centre part of the 
#'   figure; defaults to NULL (not visible)
#' @param labels The character vector giving sub-titles of heatmaps (plotted 
#'   over the heatmap and below the main title). The defaults NULL value 
#'   indicates that feature/track file names will be used to generate the 
#'   sub-titles.
#' @param plotScale scale the available data before plotting, can be "linear" 
#'   (do not scale, default), "log2" or "zscore"
#' @param legend if TRUE plot the colour key
#' @param keepratio If TRUE keep 1:1 aspect ratio of the figure; defaults to 
#'   FALSE
#' @param xlab label below x-axis
#' @param ylab label below y-axis
#' @param cex.main Main title font size in points, defaults to 16
#' @param cex.axis Axis numbers font size in points, defaults to 12
#' @param cex.lab Axis labels font size in points, Defaults to 12
#' @param cex.legend Keys labels font size in points, defaults to 12
#' @param autoscale if TRUE the colour keys will be auto scaled
#' @param zmin global minimum value on colour key, ignored if \code{autoscale} 
#'   is TRUE
#' @param zmax global maximum value on colour key, ignored if \code{autoscale} 
#'   is TRUE
#' @param xlim the x limits (x1, x2) of the plot. Note that x1 > x2 is allowed 
#'   and leads to a "reversed axis". The default value, NULL, indicates that the
#'   whole range present in \code{plotset} will be plotted.
#' @param ln.v Determins if vertical guide line(s) should be plotted (TRUE) or 
#'   ommitted (FALSE). For anchored plots 2 lines indicating the start and end 
#'   of anchored distance are plotted.
#' @param s The saturation value used to auto scale colour key limits, defaults 
#'   to 0.01
#' @param indi If TRUE (defaults) the independent colour keys will be plotted 
#'   below heatmaps, if FALSE the commmon colour key is shown rightmost
#' @param o_min vector of length equal to number of sub heatmaps determining 
#'   minimum value on color key for each sub plot, if NULL (default) or NA the 
#'   global settings are used, ignored in \code{indi} is FALSE
#' @param o_max vector of length equal to number of sub heatmaps determining 
#'   maximum value on color key for each sub plot, if NULL (default) or NA the 
#'   global settings are used, ignored in \code{indi} is FALSE
#' @param colvec The vector or list of colour values used generate sub-heatmaps 
#'   colorspaces. If NULL (default) the automatically generated colour 
#'   values will be used for all sub-heatmaps. If single color is provided, the 
#'   sequential colorspace reging from given color to white will be created. 
#'   If the vector of colors is provided, the continous pallete will be created 
#'   using these colors. NA value indicates default color pallete to be used for 
#'   give sub-heatmap. Accepted values are: vector of any of the three kinds 
#'   of R colour specifications, i.e., either a color name (as listed by 
#'   colors()), a hexadecimal string of the form "#rrggbb" or "#rrggbbaa" (see 
#'   rgb), or a positive integer i meaning palette()[i]. See 
#'   \code{\link[grDevices]{col2rgb}}.
#' @param clspace The colours pace of the heatmap, see 
#'   \code{\link[grDevices]{grDevices}}
#' @param pointsize The default font point size to be used for plots. Defaults 
#'   to 12 (1/72 inch).
#' @param embed If TRUE plot single (first) heatmap without using grid system. 
#'   Useful to embed heatmap in complex layouts, see 
#'   \code{\link[graphics]{layout}} and \code{\link[graphics]{par}} for details.
#'   Defaults to FALSE.
#' @param ggplot Use ggplot2 package instead of standard  R graphics, 
#'   defaults to FALSE
#' @param raster The bitmap raster is used to plot the heatmap image, see 
#'   "useRaster" option in \code{\link[graphics]{image}} function and 
#'   \code{\link[ggplot2]{geom_raster}} function for details, defaults to FALSE
#' @param ... parameters passed to internal plotting function
#'   
#' @return The cluster report \code{data.frame}, giving cluster assignments and
#' sorting order for each feature. It contains following columns: \itemize{ 
#' \item \strong{originalOrder} - number of feature (row) in GFF/BED, can be
#' used to restore original order after sorting on cluster ID \item
#' \strong{ClusterID} - the numeric ID of the cluster. The topmost cluster on
#' the heatmap is annotated with 1, and the bottom cluster with k, where k
#' equals to number of clusters selected, exported only if clustering is enabled
#' \item \strong{SortingOrder} - the order imposed on heatmap by sorting by mean
#' row(s) values, exported only if sorting is enabled \item \strong{FinalOrder}
#' - the final order of heatmap's rows, this can be influenced by sorting and
#' clustering; 1 indicates topmost row }
#' 
#' @export
#' @family plotting functions
#'   
#' @examples
#' # Get the paths of example files                      
#' bed1 <- system.file("extdata", 
#'      "Transcripts_ce10_chrI_100Kb.bed", package="seqplots")
#' bed2 <- system.file("extdata", 
#'      "GSM1208361_chrI_100Kb_PeakCalls.bed", package="seqplots")
#' bw1 <- system.file("extdata", 
#'      "GSM1208360_chrI_100Kb_q5_sample.bw", package="seqplots")
#' 
#' #If required install C. elegans genomic package from Bioconductor
#' if(!"BSgenome.Celegans.UCSC.ce10" %in% BSgenome::installed.genomes()) {
#'     if(.Platform$OS.type != "windows" || .Machine$sizeof.pointer != 4) {
#'          source("http://bioconductor.org/biocLite.R")
#'          biocLite("BSgenome.Celegans.UCSC.ce10")
#'      }
#' }
#' 
#' #Get getPlotSetArray for track and feature files
#' if(.Platform$OS.type != "windows" || .Machine$sizeof.pointer != 4) {
#'     plotset1 <- getPlotSetArray(bw1, c(bed1, bed2), 'ce10')
#' } else {
#'     load(system.file("extdata", "precalc_plotset.Rdata", package="seqplots"))
#' }
#' 
#' # equivalent to plot(plotset1, what='h') or plotset1$plot(what='h')
#' plotHeatmap(plotset1[1]) 
#' 
setGeneric(
    "plotHeatmap",
    function(
        plotset, main="", labels=NA, legend=TRUE, keepratio=FALSE, 
        plotScale="no", sortrows=FALSE, clusters=5L,
        clstmethod="kmeans", include=NULL, ssomt1=2L, ssomt2=2L, cex.main=16,  
        cex.lab=12.0, cex.axis=12.0, cex.legend=12.0, xlab='', ylab="",
        autoscale=TRUE, zmin=0, zmax=10, xlim=NULL, ln.v=TRUE, s = 0.01, 
        indi=TRUE, o_min=NA, o_max=NA, colvec=NULL, clspace=NULL, pointsize=12, 
        embed=FALSE, ggplot=FALSE, raster=FALSE, ...
    ) standardGeneric("plotHeatmap")
)

#' @describeIn plotHeatmap Method for signature \code{\link[base]{list}} with 
#' following format: \code{list[[FEATURE]][[TRACK/MOTIF]][[KEY_VALUE]]}
setMethod(
    "plotHeatmap", signature(plotset='list'),
    function(plotset, ...) {
        opar <- par(no.readonly = TRUE)[c('pty')]
        
        if(keepratio) par(pty='s')
        
        if( is.null(plotset[[1]]$heatmap) ) 
            stop(
                'Heatmap plotting: No heatmap data avilabe!
                Re-run with "Calculate Heatmap" option selected.', call.=FALSE
            )
        if(length(unique(sapply(
            plotset, function(x) nrow(x[['heatmap']])))) != 1) 
            stop(
                'Heatmap plotting: All plots must have equal number of features.
                Do not plot heatmaps on multiple GFF/BED.', call.=FALSE
            )
        
        if(is.null(include)) { include <- rep(TRUE, length(plotset)) }
        
        #Heatmap data aquizition (as list of matrixes)
        HLST <- lapply(plotset, '[[', 'heatmap')
        
        #Optional scalling
        if ( plotScale ==  "log2" ) {
            HLST <- lapply(HLST, log2 )
            HLST <- lapply(HLST, function(x) {x[is.infinite(x)] <- NA; return(x)} )
        } else if ( plotScale == "zscore" ) {
            HLST <- lapply(HLST, scale )
        }
        
        #Preparing flat matrix fro sorrting an clustering
        Hclc <- do.call(cbind, HLST[ include ])
        
        finalOrd <- 1:nrow(Hclc)
        RowMeans <- rowMeans(Hclc, na.rm=TRUE)
        
        #Sorting
        if(sortrows == 'decreasing' | as.character(sortrows) == "TRUE") { 
            sorting_order <- order(
                RowMeans, decreasing = TRUE
            ) 
            finalOrd <- finalOrd[sorting_order] 
            Hclc <- Hclc[sorting_order,]
        } else if(sortrows == 'increasing') { 
            sorting_order <- order(
                rowMeans(Hclc, na.rm=TRUE), decreasing = FALSE
            ) 
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
            #clusts <- k$size
            clusts <- table(classes)
            
        } else if(clstmethod == 'hclust') {
            
            Hcl <- Hclc; Hcl[is.na(Hcl)] <- 0
            cls <- hclust(dist(Hcl))
            cls_order <- cls$order
            
            #Awkward hack to rename class labels, so that they are in order
            init_cut <- cutree(cls, clusters)
            cut_map <- table(init_cut)[unique(init_cut[cls_order])]
            
            finalOrd <- finalOrd[cls_order]
            
            classes <- rep(1:clusters, cut_map)[order(finalOrd)]
            clusts <- cut_map
            
            #browser()
            
        } else if(clstmethod == 'ssom') {
            
            Hlist <- HLST[ include ]
            Hlist <- lapply(Hlist, function(x) {
                x[is.na(x)] <- 0; 
                if(sortrows == 'decreasing' | as.character(sortrows) == "TRUE" 
                    | sortrows == 'increasing') {
                    x <- x[sorting_order,]
                }
                return(x)
            })
            
            ssom <- supersom(
                Hlist, grid = class::somgrid(
                    xdim = ssomt1, ydim = ssomt2, "hexagonal"), 
                rlen = 100, toroidal=TRUE)
            
            classes <- ssom$unit.classif
            cls_order <- order(ssom$unit.classif)
            
            finalOrd <- finalOrd[cls_order]
            clusts <- table(classes)
            
        } else {
            classes <- NA
            clusts <- NULL
        }
        
        HLST <- lapply(HLST, function(x) { return(x[finalOrd, ]) } )
        
        lab <- sapply(plotset, '[[', 'desc')
        labels <- labels[1:length(plotset)]
        lab[!is.na(labels)] <- labels[!is.na(labels)]
        
        
        if( nchar(main) > 0 & !embed) par(oma=c(0,0,(cex.main/12)+1,0) )
        
        if( ggplot ) {
            ggHeatmapPlotWrapper( 
                HLST, axhline=clusts, bins=plotset[[1]]$all_ind, titles=lab, 
                e=plotset[[1]]$e, Leg=legend, cex.lab=cex.lab, cex.axis=cex.axis, 
                cex.legend=cex.legend, xlab=xlab, ylab=ylab, autoscale=autoscale, 
                zmin=zmin, zmax=zmax, xlim=xlim, ln.v=ln.v, s=s, indi=indi,
                o_min=o_min, o_max=o_max, colvec=colvec, colorspace=clspace, 
                pointsize=pointsize, embed=embed, main=main,
                ...
            )
        } else {
            heatmapPlotWrapper( 
                HLST, axhline=clusts, bins=plotset[[1]]$all_ind, titles=lab, 
                e=plotset[[1]]$e, Leg=legend, cex.lab=cex.lab, cex.axis=cex.axis, 
                cex.legend=cex.legend, xlab=xlab, ylab=ylab, autoscale=autoscale, 
                zmin=zmin, zmax=zmax, xlim=xlim, ln.v=ln.v, s=s, indi=indi,
                o_min=o_min, o_max=o_max, colvec=colvec, colorspace=clspace, 
                pointsize=pointsize, embed=embed, raster=raster, 
                dendro=if(clstmethod == 'hclust') as.dendrogram(cls) else NULL,
                ...
            )
            title(main, outer = TRUE, cex.main=cex.main/pointsize)
        }
        
        par(opar)
        
        out <- data.frame(
            originalOrder=1:length(finalOrd), 
            ClusterID=classes[order(sorting_order)], 
            SortingOrder=sorting_order, 
            FinalOrder=finalOrd,
            RowMeans=RowMeans
        )
        
        anno_list <- unique(lapply(plotset, '[[', 'anno'))
        if(length(anno_list) > 1) warning('Multiple features used to generate the heatmaps, first will be used to generate features report')
        anno <- anno_list[[1]]
        if( !is.null(anno) ) {
        
            meta <- elementMetadata(anno)
            meta <- meta[!grepl('IRanges', sapply(meta, class))]
            meta <- meta[sapply( meta, function(x) !all(is.na(x)))]
     
            if( length(colnames(meta)) ) {
                colnames(meta) <- paste0('metadata_', colnames(meta))
            }
            elementMetadata(anno) <- meta
            
            out_meta <- as.data.frame(anno)
            colnames(out_meta)[1] <- 'chromosome'
            out <- cbind(out_meta, out)[finalOrd,]
            
        }
        
        return( invisible(out) )
    }
)

#' @describeIn plotHeatmap Method for signature \code{\link{PlotSetPair}}
#' @include PlotSetPair-class.R
setMethod(
    "plotHeatmap", signature(plotset='PlotSetPair'),
    function(plotset, ...) {
        plotHeatmap(
            list(plotset), main, labels, legend, keepratio, 
            plotScale, sortrows, clusters, clstmethod, 
            include, ssomt1, ssomt2, cex.main,  cex.lab, cex.axis, 
            cex.legend, xlab, ylab, autoscale, zmin, zmax, xlim, ln.v, 
            s, indi, o_min, o_max, colvec, clspace, pointsize, 
            embed, ggplot, raster, ...
        )
    }
)

#' @describeIn plotHeatmap Method for signature \code{\link{PlotSetList}}
#' @include PlotSetList-class.R
setMethod(
    "plotHeatmap", signature(plotset='PlotSetList'), 
    function(plotset, ...) {
        plotHeatmap(
            plotset$data, main, labels, legend, keepratio, 
            plotScale, sortrows, clusters, clstmethod, 
            include, ssomt1, ssomt2, cex.main,  cex.lab, cex.axis, 
            cex.legend, xlab, ylab, autoscale, zmin, zmax, xlim, ln.v, 
            s, indi, o_min, o_max, colvec, clspace, pointsize, 
            embed, ggplot, raster, ...
        )
    }
)

#' @describeIn plotHeatmap Method for signature \code{\link{PlotSetArray}}
#' @include PlotSetArray-class.R
setMethod(
    "plotHeatmap", signature(plotset='PlotSetArray'), 
    function(plotset, ...) {
        plotHeatmap(
            unlist(plotset)$data, main, labels, legend, keepratio, 
            plotScale, sortrows, clusters, clstmethod, 
            include, ssomt1, ssomt2, cex.main,  cex.lab, cex.axis, 
            cex.legend, xlab, ylab, autoscale, zmin, zmax, xlim, ln.v, 
            s, indi, o_min, o_max, colvec, clspace, pointsize, 
            embed, ggplot, raster, ...
        )
    }
)

