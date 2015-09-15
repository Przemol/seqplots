#' Wrapper function, plotting the heatmap
#' 
#' This function is package internal and should not be executed directly by
#' users.
#' 
#' @param MAT - list of matrixes holding heatmap data
#' @param axhline - locations of horizontal lines separating the clusters
#' @param titles the sub-titles of heatmaps
#' @param bins the x-axis indicates in heatmap
#' @param cex.axis Axis numbers font size in points, defaults to 12
#' @param cex.lab Axis labels font size in points, Defaults to 12
#' @param cex.legend Keys labels font size in points, defaults to 12
#' @param xlab label below x-axis
#' @param ylab label below y-axis
#' @param leg if TRUE plot the color key
#' @param autoscale if TRUE the color keys will be auto scaled
#' @param zmin global minimum value on color key, ignored if \code{autoscale} is
#'   TRUE
#' @param zmax global maximum value on color key, ignored if \code{autoscale} is
#'   TRUE
#' @param xlim the x limits (x1, x2) of the plot. Note that x1 > x2 is allowed
#'   and leads to a "reversed axis". The default value, NULL, indicates that the
#'   whole range present in \code{plotset} will be plotted.
#' @param ln.v Determines if vertical guide line(s) should be plotted (TRUE) or
#'   ommitted (FALSE). For anchored plots 2 lines indicating the start and end
#'   of anchored distance are plotted.
#' @param e Determines the end of anchored distance
#' @param s The saturation value used to auto scale color key limits, defaults
#'   to 0.01
#' @param indi If TRUE (defaults) the independent color keys will be plotted
#'   below heatmaps, if FALSE the common color key is shown rightmost
#' @param o_min vector of length equal to number of sub heatmaps determining
#'   minimum value on color key for each sub plot, if NULL (default) or NA the
#'   global settings are used, ignored in \code{indi} is FALSE
#' @param o_max vector of length equal to number of sub heatmaps determining
#'   maximum value on color key for each sub plot, if NULL (default) or NA the
#'   global settings are used, ignored in \code{indi} is FALSE
#' @param colvec The vector of colors used to plot the lines and error estimate
#'   fields. If set value NULL (default) the automatically generated color
#'   values will be used. Accepted values are: vector of any of the three kinds
#'   of R color specifications, i.e., either a color name (as listed by
#'   colors()), a hexadecimal string of the form "#rrggbb" or "#rrggbbaa" (see
#'   rgb), or a positive integer i meaning palette()[i]. See
#'   \code{\link[grDevices]{col2rgb}}.
#' @param colorspace The colorspace of the heatmap, see
#'   \code{\link[grDevices]{grDevices}}
#' @param pointsize The default font point size to be used for plots. Defaults
#'   to 12 (1/72 inch).
#' @param embed Configures heatmap to be used embedded plot. Defaults
#'   to FALSE 
#' @param raster Uses raster graphics for heatmaps. Defaults to FALSE
#' @param ylim Y axis limits. Defaults to c(nrow(MAT[[1]]),1) 
#' @param dendro Dendrogram object, will be plotted left to heatmaps. Defaults 
#'   to NULL
#'   
#' @return \code{NULL}
#'   
#' @keywords internal
#'   
heatmapPlotWrapper <- function(MAT, axhline=NULL, titles=rep('', length(MAT)),
    bins=1:(ncol(MAT[[1]])/length(MAT)), cex.lab=12.0, cex.axis=12.0, 
    cex.legend=12.0, xlab='', ylab="", Leg=TRUE, autoscale=TRUE, zmin=0, 
    zmax=10, xlim=NULL, ln.v=TRUE, e=NULL, s = 0.01, indi=TRUE,
    o_min=NA, o_max=NA, colvec=NULL, colorspace=NULL, pointsize=12,
    embed=FALSE, raster=FALSE, ylim=c(nrow(MAT[[1]]),1), dendro=NULL, ...) {
    
    lfs  <- cex.lab / pointsize
    afs  <- cex.axis / pointsize
    lgfs <- cex.legend / pointsize
    
    datapoints <- unlist(MAT)
    NP <- length(MAT)
    if(!is.null(dendro)) NP <- NP+1
    raster <- length(unique(diff(bins)))==1 & raster
    

    if( !is.null(colvec) ) colvec[ grepl('#ffffff', colvec, ignore.case = TRUE) ] <- NA
    ncollevel = 1024
    if(length(colorspace)) {
        gcol <- colorRampPalette(colorspace)
    }else {
        gcol <- colorRampPalette(c(
            "#00007F", "blue", "#007FFF", "cyan", "#7FFF7F", "yellow", 
            "#FF7F00", "red", "#7F0000"
        ))     
    }
    min <- min(datapoints, na.rm=TRUE)
    max <- max(datapoints, na.rm=TRUE) 
    
    if (!indi) {
        if (autoscale) {
            zlim <- quantile(datapoints, c(s,1-s), na.rm=TRUE)
            zmin<-zlim[1]
            zmax<-zlim[2]
        } 
        if(!embed) 
            layout(
                matrix(seq(NP+1), nrow=1, ncol=NP+1), 
                widths=c(rep(12/NP, NP), 1), heights=rep(1,NP+1)
            )
        ColorRamp <-gcol(ncollevel)
        ColorLevels <- seq(to=zmax,from=zmin, length=ncollevel)#number sequence
    } else {
        if(!embed) invisible(capture.output( set.panel(1, NP) ))
    }
    
    if(!is.null(dendro)) {
        #dendro  <- color_branches(dendro, k=length(axhline))
        mar2=par()$mar; if(indi){
            if(NP>2) par(mar=c(12.3, 4.1, 2.6, 2.1))
            else par(mar=c(8.4, 4.1, 3.3, 2.1))
        } else {
            par(mar=c(3.0, 4.1, 2.3, 2.1))
        }
        plot(dendro, horiz=TRUE, leaflab='none', ylim=ylim, main='Dendrogram')
        par(mar=mar2)
        #rect.dendrogram(dendro, k=length(axhline), horiz=TRUE)
        abline(h=cumsum(axhline[-length(axhline)])+.5, lwd=2, col='red')
        axis(2, at=ylim, labels=ylim, cex.axis=afs*0.9, col.axis='darkgrey')
        axis(
            2, at=cumsum(axhline)-(axhline/2)+.5, 
            labels=paste0('C', 1:length(axhline)), las = 1, 
            col.axis='darkred', font.axis=2, cex.axis=afs
        )
    }
    
    for( i in seq(length(MAT)) ) {
        data <- MAT[[i]]
        

        xinds <- if (is.null(xlim)) range(bins) else xlim
        
        if( !indi ) {
            data[data<zmin] <- zmin
            data[data>zmax] <- zmax
            ColorRamp_ex <- ColorRamp[round( 
                (min(data, na.rm=TRUE)-zmin)*ncollevel/(zmax-zmin) ) : round( 
                    (max(data, na.rm=TRUE)-zmin)*ncollevel/(zmax-zmin) 
                )]
            image(
                bins, 1:nrow(data), t(data), axes=TRUE, col=ColorRamp_ex, 
                xlab=xlab, ylab=ylab, add=FALSE, ylim=ylim,
                xlim=if (is.null(xlim)) range(bins) else xlim,
                cex=1, cex.main=lfs, cex.lab=lfs, cex.axis=afs,
                useRaster=raster, xaxt="n", yaxt="n", panel.first={
                    axis(2, at=ylim, labels=ylim, cex.axis=afs*0.9,  col.axis='darkgrey')
                    if(is.null(e)) { 
                        axis(
                            1, at=c(min(xinds), 0,  max(xinds)), 
                            labels=c(num2bp(min(xinds)), '0bp', 
                                     num2bp(max(xinds))), cex.axis=afs
                        ) 
                    } else {
                        axis(
                            1, at=c(min(xinds), 0,  e, max(xinds)), 
                            labels=c(
                                num2bp(min(xinds)), '0bp', '0bp', 
                                num2bp(max(xinds)-e)
                            ), cex.axis=afs
                        )
                    }
                    rect(
                        par("usr")[1],par("usr")[3],par("usr")[2],
                        par("usr")[4],col="lightgrey"
                    )
                }, ...
            )
            
        } else {
            if (autoscale) {
                zlim <- quantile(data, c(s,1-s), na.rm=TRUE)
                zmin<-zlim[1]
                zmax<-zlim[2]
            } 
            
            if( is.na(o_min[i]) ) data[data<zmin] <- zmin 
                else data[data<o_min[i]] <- o_min[i]
            if( is.na(o_max[i]) ) data[data>zmax] <- zmax 
                else data[data>o_max[i]] <- o_max[i]
            
            keycolor_lim <- range(data, na.rm=TRUE)
            if( is.na(o_min[i]) ) keycolor_lim[1] <- zmin 
                else keycolor_lim[1] <- o_min[i]
            if( is.na(o_max[i]) ) keycolor_lim[2] <- zmax 
                else keycolor_lim[2] <- o_max[i]
            
            col <- if( is.null(colvec) ) {
                gcol(ncollevel)
            } else if( is.character(colvec[[i]]) & !any(is.na(colvec[[i]])) ) {
                if( length(colvec[[i]]) == 1 )    
                    colorRampPalette(c('white', colvec[[i]]))(ncollevel)
                else
                    colorRampPalette(colvec[[i]])(ncollevel)
            } else {
                gcol(ncollevel)
            }
            
            
            imPlot2(
                bins, 1:nrow(data), t(data), axes=TRUE, xlab=xlab, ylab=ylab, 
                xlim=if (is.null(xlim)) range(bins) else xlim,  
                zlim=keycolor_lim, col=col, ylim=ylim,
                legend.width=1, horizontal=TRUE, useRaster=raster, 
                xinds=xinds, e=e, xaxt="n", yaxt="n",
                cex=1, cex.main=lfs, cex.lab=lfs, cex.axis=afs, 
                ylast=nrow(data), afs=afs, 
                axis.args=list(cex.axis=afs), ...
            )
            
            
            
            
        }
        title( main=titles[i], cex.main=lfs ); box()
        if (!is.null(axhline)){
            #message(paste(axhline, collapse=', '))
            abline(h=cumsum(axhline)+.5, lwd=2)
            axis(
                2, at=cumsum(axhline)-(axhline/2)+.5, 
                labels=paste0('C', 1:length(axhline)), las = 1, 
                col.axis='darkred', font.axis=2, cex.axis=afs
            )
            
            #cumsum(axhline)
         
        }
        if (ln.v){
            abline(v=c(0, e), lwd=0.5)
        }
        
        if(embed) break()
    }
    
    #draw legend/color key for multiple heatmaps
    if(Leg & !indi & !embed) {
        opar <- par()[c('cex.axis', 'mar')]; par(cex.axis=lgfs, mar=c(0,0,0,0));
        plot.new(); 
        image.plot(
            1, ColorLevels, 
            matrix(data=ColorLevels, ncol=length(ColorLevels),nrow=1),
            col=ColorRamp, legend.only = TRUE, legend.shrink=1, 
            smallplot=c(.01,.25,0.3,.8)
        )
        par(opar)
    }
    if(!embed) layout(1)
}
