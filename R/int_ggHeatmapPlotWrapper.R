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
#'   
#' @return \code{NULL}
#'   
#' @keywords internal
#'   
ggHeatmapPlotWrapper <- function(MAT, axhline=NULL, titles=rep('', length(MAT)),
    bins=1:(ncol(MAT[[1]])/length(MAT)), cex.lab=12.0, cex.axis=12.0, 
    cex.legend=12.0, xlab='', ylab="", Leg=TRUE, autoscale=TRUE, zmin=0, 
    zmax=10, xlim=NULL, ln.v=TRUE, e=NULL, s = 0.01, indi=TRUE,
    o_min=NA, o_max=NA, colvec=NULL, colorspace=NULL, pointsize=12,
    embed=FALSE, raster=TRUE, main='', cex.title=20, ...) {
    
    datapoints <- unlist(MAT)
    NP=length(MAT)
    raster <- length(unique(diff(bins)))==1 & raster
    
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
        
        MAT <- lapply(MAT, function(x) {
            colnames(x) <- bins
            x[x<zmin] <- zmin 
            x[x>zmax] <- zmax 
            return(x)
        })
        names(MAT) <- titles
        
        p <- ggplot(melt(MAT), aes(Var2, Var1, fill = value))
        if(raster) {
            p <- p + geom_raster()
        } else {
            p <- p + geom_tile()
        }
        p <- p + facet_wrap(~L1, nrow=1)
        p <- p + scale_fill_gradientn(
            colours = gcol(100), limits = c(zmin, zmax), 
            breaks=c(zmin, zmax), labels=format(c(zmin, zmax), digits=2)
        ) 
        p <- p + scale_x_continuous(
            breaks=c(min(bins), 0, max(bins)),
            labels=sapply(c(min(bins), 0, max(bins)), num2bp),
            expand = c(0.05, 0.05)
        )
        p <- p + geom_hline(yintercept=cumsum(axhline)[-length(axhline)]+.5, size=1) 
        p <- p + geom_vline(xintercept=c(0, e), size=.75, colour='black') 
        p <- p + scale_y_reverse(
            #limits=if (is.null(xlim)) range(bins) else xlim,
            breaks=c(cumsum(axhline)-(axhline/2)+.5, nrow(data)),
            labels=c(paste0('C', 1:length(axhline)), nrow(data)),
            expand = c(0.015, 0.015)
        ) 
        p <- p + ggtitle(main) 
        p <- p + xlab(xlab) 
        p <- p + ylab(ylab)
        p <- p + guides(fill = guide_colorbar(title = "", raster = TRUE))
        p <- p + theme( 
            axis.text=element_text(size=cex.axis), 
            axis.title=element_text(size=cex.lab),
            title=element_text(size=cex.title),
            legend.text=element_text(size=cex.legend) 
        )
        
        print(p)
        return(p)
    }
    
    
    plots <- list()
    for (i in seq(NP)) {
        data <- MAT[[i]]
        
        
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
        
        #browser()
        colnames(data) <- bins
        p <- ggplot(melt(data), aes(Var2, Var1, fill = value))
        if(raster) {
            p <- p + geom_raster()
        } else {
            p <- p + geom_tile()
        }
        p <- p + scale_fill_gradientn(
            colours = gcol(100), limits = keycolor_lim, 
            breaks=keycolor_lim, labels=format(keycolor_lim, digits=2)
        ) 
        p <- p + scale_x_continuous(
            breaks=c(min(bins), 0, max(bins)),
            labels=sapply(c(min(bins), 0, max(bins)), num2bp),
            expand = c(0.05, 0.05)
        ) 
        p <- p + geom_hline(yintercept=cumsum(axhline)[-length(axhline)]+.5, size=1) 
        p <- p + geom_vline(xintercept=c(0, e), size=.75, colour='black') 
        p <- p + scale_y_reverse(
            #limits=if (is.null(xlim)) range(bins) else xlim,
            breaks=c(cumsum(axhline)-(axhline/2)+.5, nrow(data)),
            labels=c(paste0('C', 1:length(axhline)), nrow(data)),
            expand = c(0.015, 0.015)
        ) 
        p <- p + ggtitle(titles[i]) 
        p <- p + xlab(xlab) 
        p <- p + ylab(ylab) 
        p <- p + theme(
            legend.position = "bottom", 
            axis.text=element_text(size=cex.axis), 
            axis.title=element_text(size=cex.lab),
            title=element_text(size=cex.lab),
            legend.text=element_text(size=cex.legend) 
        )
        p <- p + guides(fill = guide_colorbar(barwidth = 10/5, barheight = 1, title = "", raster = TRUE))
        
        
        plots[[i]] <- p
        
        
    }
    
    #fix for importing ggplot2::ggplotGrob into namespace
    if(!"package:ggplot2" %in% search()) attachNamespace('ggplot2')
    grid <- marrangeGrob(
        plots, nrow=1, ncol=length(plots), 
        top = textGrob(main,gp=gpar(fontsize=cex.title))
    )
    grid.draw(grid[[1]])
    return(grid)
}
