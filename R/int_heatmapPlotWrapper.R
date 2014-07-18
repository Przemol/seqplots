#' Wrapper function, plotting the heatmap
#' 
#' This function is package internal and should not be executed directly
#' by users.
#'
#' @keywords internal
#'    

.heatmapPlotWrapper <- function(MAT, axhline=NULL, titles=rep('', length(MAT)),    bins=1:(ncol(MAT[[1]])/length(MAT)), 
                               lfs=12.0, afs=12.0, lgfs=12.0, xlabel='xlab', Leg=TRUE, autoscale=TRUE, zmin=0, zmax=10, ln.v=TRUE, e=NULL, xlim=NULL, ylabel="", s = 0.01, indi=TRUE,
                               o_min=NA, o_max=NA, colvec=NULL, colorspace=NULL, poinsize=12) {
    
    lfs  <- lfs / poinsize
    afs  <- afs / poinsize
    lgfs <- lgfs / poinsize
    opar <- par(no.readonly = TRUE)
    
    datapoints <- unlist(MAT)
    NP=length(MAT)
    raster <- length(unique(diff(bins)))==1
    
    #colvec[ grepl('#ffffff', colvec) ] <- NA
    ncollevel = 64
    if(length(colorspace)) {
        gcol <- colorRampPalette(colorspace)
    }else {
        gcol <- colorRampPalette(c("#00007F", "blue", "#007FFF", "cyan", "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"))
        #colorRampPalette(c("#053061","#2166AC","#4393C3","#92C5DE","#D1E5F0","#F7F7F7","#FDDBC7","#F4A582","#D6604D","#B2182B","#67001F"))     
    }
    min <- min(datapoints, na.rm=TRUE)
    max <- max(datapoints, na.rm=TRUE) 
    
    if (!indi) {
        if (autoscale) {
            zlim <- quantile(datapoints, c(s,1-s), na.rm=TRUE)
            zmin<-zlim[1]
            zmax<-zlim[2]
        } 
        #par(oma = c(0, 0, 3, 0))
        layout(matrix(seq(NP+1), nrow=1, ncol=NP+1), widths=c(rep(12/NP,NP),1), heights=rep(1,NP+1))
        ColorRamp <-gcol(ncollevel)
        ColorLevels <- seq(to=zmax,from=zmin, length=ncollevel)   #number sequence
    } else {
        set.panel(1, NP)
    }
    
    
    
    for (i in seq(NP)) {
        data <- MAT[[i]]
        
        par(cex=1, cex.main=lfs, cex.lab=lfs, cex.axis=afs)
        
        if( !indi ) {
            data[data<zmin] <- zmin
            data[data>zmax] <- zmax
            ColorRamp_ex <- ColorRamp[round( (min(data, na.rm=TRUE)-zmin)*ncollevel/(zmax-zmin) ) : round( (max(data, na.rm=TRUE)-zmin)*ncollevel/(zmax-zmin) )]
            image(bins, 1:nrow(data), t(data), axes=TRUE, col=ColorRamp_ex, xlab=xlabel, ylab=ylabel, xlim=if (is.null(xlim)) range(bins) else xlim, add=FALSE, ylim=c(nrow(data),1),
                  useRaster=raster, panel.first=rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col="lightgrey"))
            
        } else {
            if (autoscale) {
                zlim <- quantile(data, c(s,1-s), na.rm=TRUE)
                zmin<-zlim[1]
                zmax<-zlim[2]
            } 
            
            if( is.na(o_min[i]) ) data[data<zmin] <- zmin else data[data<o_min[i]] <- o_min[i]
            if( is.na(o_max[i]) ) data[data>zmax] <- zmax else data[data>o_max[i]] <- o_max[i]
            
            keycolor_lim <- range(data, na.rm=TRUE)
            if( is.na(o_min[i]) ) keycolor_lim[1] <- zmin else keycolor_lim[1] <- o_min[i]
            if( is.na(o_max[i]) ) keycolor_lim[2] <- zmax else keycolor_lim[2] <- o_max[i]
            
            col <- if( ifelse(is.character(colvec[i]), !is.na(colvec[i]), FALSE) ) colorRampPalette(c('white', colvec[i]))(ncollevel) else gcol(ncollevel)
            
            #par(cex=1, cex.main=lfs, cex.lab=lfs, cex.axis=afs)
            .imPlot2(bins, 1:nrow(data), t(data), axes=TRUE, xlab=xlabel, ylab=ylabel, 
                    xlim=if (is.null(xlim)) range(bins) else xlim,  ylim=c(nrow(data),1),
                    zlim=keycolor_lim, col=col,
                    legend.width=1, horizontal=TRUE, useRaster=raster)
            
        }
        title( main=titles[i]); box()
        if (!is.null(axhline)){
            hi = 0
            for (i in axhline){
                hi = hi+i
                abline(hi+0.5,0,lwd=4)
            }
        }
        if (ln.v){
            abline(v=c(0, e), lwd=2)
        }
    }
    
    #draw legend/color key for multiple heatmaps
    if(Leg & !indi) {
        par(cex.axis=lgfs, mar=c(0,0,0,0)); plot.new()
        image.plot(1, ColorLevels,matrix(data=ColorLevels, ncol=length(ColorLevels),nrow=1),col=ColorRamp, legend.only = TRUE, legend.shrink=1, smallplot=c(.1,.4,0.1,.9))
        #box()
    }
    par(opar); layout(1)
}
