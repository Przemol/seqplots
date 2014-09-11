###############################################################################
# Wrapper function, plotting the heatmap
###############################################################################


# Modified image.plot function from "fields" package, the grey rectangle 
# is used as background for heatmap, making NAs distinguishable from the data
# rect(usr[1], usr[3], usr[2], usr[4], col="grey")
###############################################################################
imPlot2 <- function (..., add = FALSE, nlevel = 64, horizontal = FALSE, 
                     legend.shrink = 0.9, legend.width = 1.2, legend.mar = ifelse(horizontal, 
                                                                                  3.1, 5.1), legend.lab = NULL, legend.line = 2, graphics.reset = FALSE, 
                     bigplot = NULL, smallplot = NULL, legend.only = FALSE, col = tim.colors(nlevel), 
                     lab.breaks = NULL, axis.args = NULL, legend.args = NULL, 
                     midpoint = FALSE, border = NA, lwd = 1, xinds=NULL, e=NULL) {
  old.par <- par(no.readonly = TRUE)
  info <- imageplot.info(...)
  if (add) {
    big.plot <- old.par$plt
  }
  if (legend.only) {
    graphics.reset <- TRUE
  }
  if (is.null(legend.mar)) {
    legend.mar <- ifelse(horizontal, 3.1, 5.1)
  }
  temp <- imageplot.setup(add = add, legend.shrink = legend.shrink, 
                          legend.width = legend.width, legend.mar = legend.mar, 
                          horizontal = horizontal, bigplot = bigplot, smallplot = smallplot)
  smallplot <- temp$smallplot
  bigplot <- temp$bigplot
  if (!legend.only) {
    if (!add) {
      par(plt = bigplot)
    }
    if (!info$poly.grid) {
      image(..., add = add, col = col, panel.last={
          rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col = "lightgrey")
          if(is.null(e)) axis(1) else axis(1, at=c(min(xinds), 0,  e, max(xinds)), labels=c(min(xinds), '0', '0', max(xinds)-e))
      })
    }
    else {
      poly.image(..., add = add, col = col, midpoint = midpoint, 
                 border = border, lwd.poly = lwd)
    }
    big.par <- par(no.readonly = TRUE)
  }
  if ((smallplot[2] < smallplot[1]) | (smallplot[4] < smallplot[3])) {
    par(old.par)
    stop("plot region too small to add legend\n")
  }
  ix <- 1
  minz <- info$zlim[1]
  maxz <- info$zlim[2]
  binwidth <- (maxz - minz)/nlevel
  midpoints <- seq(minz + binwidth/2, maxz - binwidth/2, by = binwidth)
  iy <- midpoints
  iz <- matrix(iy, nrow = 1, ncol = length(iy))
  breaks <- list(...)$breaks
  par(new = TRUE, pty = "m", plt = smallplot, err = -1)
  if (!is.null(breaks) & !is.null(lab.breaks)) {
    axis.args <- c(list(side = ifelse(horizontal, 1, 4), 
                        mgp = c(3, 1, 0), las = ifelse(horizontal, 0, 2), 
                        at = breaks, labels = lab.breaks), axis.args)
  }
  else {
    axis.args <- c(list(side = ifelse(horizontal, 1, 4), 
                        mgp = c(3, 1, 0), las = ifelse(horizontal, 0, 2)), 
                   axis.args)
  }
  if (!horizontal) {
    if (is.null(breaks)) {
      image(ix, iy, iz, xaxt = "n", yaxt = "n", xlab = "", 
            ylab = "", col = col)
    }
    else {
      image(ix, iy, iz, xaxt = "n", yaxt = "n", xlab = "", 
            ylab = "", col = col, breaks = breaks)
    }
  }
  else {
    if (is.null(breaks)) {
      image(iy, ix, t(iz), xaxt = "n", yaxt = "n", xlab = "", 
            ylab = "", col = col)
    }
    else {
      image(iy, ix, t(iz), xaxt = "n", yaxt = "n", xlab = "", 
            ylab = "", col = col, breaks = breaks)
    }
  }
  do.call("axis", axis.args)
  box()
  if (!is.null(legend.lab)) {
    legend.args <- list(text = legend.lab, side = ifelse(horizontal, 
                                                         1, 4), line = legend.line)
  }
  if (!is.null(legend.args)) {
    do.call(mtext, legend.args)
  }
  mfg.save <- par()$mfg
  if (graphics.reset | add) {
    par(old.par)
    par(mfg = mfg.save, new = FALSE)
    invisible()
  }
  else {
    par(big.par)
    par(plt = big.par$plt, xpd = FALSE)
    par(mfg = mfg.save, new = FALSE)
    invisible()
  }
}

###############################################################################

heatmapPlotWrapper <- function(MAT, axhline=NULL, titles=rep('', length(MAT)),	bins=1:(ncol(MAT[[1]])/length(MAT)), 
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
		xinds <- if (is.null(xlim)) range(bins) else xlim
        
    if( !indi ) {
      data[data<zmin] <- zmin
      data[data>zmax] <- zmax
      ColorRamp_ex <- ColorRamp[round( (min(data, na.rm=TRUE)-zmin)*ncollevel/(zmax-zmin) ) : round( (max(data, na.rm=TRUE)-zmin)*ncollevel/(zmax-zmin) )]
      image(bins, 1:nrow(data), t(data), axes=TRUE, col=ColorRamp_ex, xlab=xlabel, ylab=ylabel, xlim=if (is.null(xlim)) range(bins) else xlim, add=FALSE, ylim=c(nrow(data),1),
            useRaster=raster, , xaxt="n", panel.first={
                if(is.null(e)) axis(1) else axis(1, at=c(min(xinds), 0,  e, max(xinds)), labels=c(min(xinds), '0', '0', max(xinds)-e))
                rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col="lightgrey")
            }
        )
      
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
      imPlot2(bins, 1:nrow(data), t(data), axes=TRUE, xlab=xlabel, ylab=ylabel, 
              xlim=if (is.null(xlim)) range(bins) else xlim,  ylim=c(nrow(data),1),
              zlim=keycolor_lim, col=col,
              legend.width=1, horizontal=TRUE, useRaster=raster,
              xinds=xinds, e=e, xaxt="n")
      
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
