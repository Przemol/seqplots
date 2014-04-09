# TODO: Add comment
# 
# Author: przemol
###############################################################################

require(plotrix)
plotMext <- function(INPUTS, desc1="", y1=NULL, y2=NULL, x1=NULL, x2=NULL, type='dev', title="", Xtitle='TSS', Ytitle='Signal', plotScale='linear', EE=TRUE, Leg=1, 
		cex.axis=1.5, cex.lab=1.25, cex.main=2, cex.legend=1.20, pdf.size.X=16, pdf.size.Y=10, ln.v=FALSE, ln.h=1, colvec=NULL, pdf.name='out.pdf', 
    legend_pos='topright', legend_ext_pos="topleft", legend_ext=FALSE) {
  
	mm <- length(INPUTS)
	
	if (ln.v) { ln.v <- c(0, INPUTS[[1]]$e) } else { ln.v <- NULL }
	
	
	if (plotScale == 'log2') {
		for (i in 1:mm) { co <- diff(range(INPUTS[[i]]$means)); INPUTS[[i]]$means <- log2(abs(INPUTS[[i]]$means)); co <- diff(range(INPUTS[[i]]$means))/co; INPUTS[[i]]$stderror <- co*INPUTS[[i]]$stderror; INPUTS[[i]]$conint <- co*INPUTS[[i]]$conint; }
	} else if  (plotScale == 'zscore') {
		for (i in 1:mm) { co <- diff(range(INPUTS[[i]]$means)); INPUTS[[i]]$means <- as.numeric(scale(INPUTS[[i]]$means));  co <- diff(range(INPUTS[[i]]$means))/co; INPUTS[[i]]$stderror <- co*INPUTS[[i]]$stderror; INPUTS[[i]]$conint <- co*INPUTS[[i]]$conint;}
	}
	
	if (is.null(y1)) {
		y1 <- min( sapply(INPUTS, function(INPUT){ min(INPUT$means - INPUT$conint, na.rm=T) }) )
	}
	if (is.null(y2)) {
		y2 <- max( sapply(INPUTS, function(INPUT){ max(INPUT$means + INPUT$conint, na.rm=T) }) )
	}
	#desc <- sprintf("%s - %s", desc1, paste( sapply(INPUTS, function(INPUT){ INPUT$desc }), collapse=" vs. " ))
	
	if ( !is.null(colvec) ) {
		
		colvec[ grepl('#ffffff', colvec, ignore.case = TRUE) ] <- c("darkblue", "darkgreen", "darkred", "darkmagenta", "darkgray", "darkorange", "darkcyan", "black", rainbow(mm-8))[ grepl('#ffffff', colvec, ignore.case = TRUE) ]
		cols <- colvec
	} else {
		cols <- c("darkblue", "darkgreen", "darkred", "darkmagenta", "darkgray", "darkorange", "darkcyan", "black", rainbow(mm-8))
	}
	legendText=NULL
	
#		par(mar=c(3.2+cex.lab, 3.2+cex.lab, 2+cex.main, 3))
#		for (i in 1:mm) {
#			INPUT <- INPUTS[[i]]
#			if (i==1) { matplot( INPUT$all_ind, INPUT$means, type="l", col="red", lty="solid", xlab=Xtitle, ylab=Ytitle, main=title, ylim=c(y1, y2), xlim=c(x1, x2), log = "", cex.axis=cex.axis, cex.lab=cex.lab, cex.main=cex.main, mar=c(5.1, 4.1, 4.1, 2.1)) }
#			if(EE) { dispersion(INPUT$all_ind, INPUT$means, INPUT$conint, type="l", fill=adjustcolor(cols[i], 0.5), lty=2) }
#			if(EE) { dispersion(INPUT$all_ind, INPUT$means, INPUT$stderror, type="l", fill=adjustcolor(cols[i], 0.3), lty=2) }
#			lines( INPUT$all_ind, INPUT$means, col=cols[i])
#			legendText <- c(legendText, INPUT$desc)
#		}
#		if(Leg) {
#			if(EE) { legend("topleft", c("Mean\n(solid line)","Standard error\n(dark filed)", "95% CI\n(light field)"), pch=c(-1, 0, 0),  title="Fields and lines legend", lwd=c(3,15,15), lty=c(1,0,0), col=rgb(0,0,0, c(1,0.5, 0.3)), bg=rainbow(1, alpha=0), bty="n", cex=cex.legend, y.intersp=2, inset=0.0, seg.len=2, title.adj=c(2, 2)) }
#			legend("topright", legendText, col=cols, bg=rainbow(1, alpha=0),  bty="n", cex=cex.legend, y.intersp=2, inset=0.0, seg.len=2, title.adj=c(2, 2), lwd=15, pch=0, lty=0)
#		}
#		abline( v=ln.v, h=ln.h, col="gray" )
if (type == 'legend') {
  legendText <- sapply(INPUTS, '[[', 'desc')
  plot.new()
  if(Leg) {
    legend(legend_pos, legendText, col=cols, bg=rainbow(1, alpha=0),  bty="n", cex=cex.legend, y.intersp=2, inset=0.0, seg.len=2, title.adj=c(2, 2), lwd=15, pch=0, lty=0)
  }
  if(EE && legend_ext) { 
    legend(legend_ext_pos, c("Mean\n(solid line)","Standard error\n(dark filed)", "95% CI\n(light field)"), pch=c(-1, 0, 0),  title="Fields and lines legend", lwd=c(3,15,15), lty=c(1,0,0), col=rgb(0,0,0, c(1,0.5, 0.3)), bg=rainbow(1, alpha=0), bty="n", cex=cex.legend, y.intersp=2, inset=0.0, seg.len=2, title.adj=c(2, 2)) 
  }
} else {
  if (type == 'pdf') pdf(pdf.name, width = pdf.size.X , height = pdf.size.Y, onefile = FALSE, paper = "special") #, encoding = "TeXtext.enc")
		par(mar=c(3.2+cex.lab, 3.2+cex.lab, 2+cex.main, 3))
		for (i in 1:mm) {
			INPUT <- INPUTS[[i]]
			if (i==1) { matplot( INPUT$all_ind, INPUT$means, type="l", col="red", lty="solid", xlab=Xtitle, ylab=Ytitle, main=title, ylim=c(y1, y2), xlim=c(x1, x2), cex.axis=cex.axis, cex.lab=cex.lab, cex.main=cex.main) }
			if(EE) { dispersion(INPUT$all_ind, INPUT$means, INPUT$conint, type="l", fill=adjustcolor(cols[i], 0.5), lty=2) }
			if(EE) { dispersion(INPUT$all_ind, INPUT$means, INPUT$stderror, type="l", fill=adjustcolor(cols[i], 0.3), lty=2) }
			lines( INPUT$all_ind, INPUT$means, col=cols[i])
			legendText <- c(legendText, INPUT$desc)
		}
		if(Leg) {
			legend(legend_pos, legendText, col=cols, bg=rainbow(1, alpha=0),  bty="n", cex=cex.legend, y.intersp=2, inset=0.0, seg.len=2, title.adj=c(2, 2), lwd=15, pch=0, lty=0)
		}
    if(EE && legend_ext) { 
      legend(legend_ext_pos, c("Mean\n(solid line)","Standard error\n(dark filed)", "95% CI\n(light field)"), pch=c(-1, 0, 0),  title="Fields and lines legend", lwd=c(3,15,15), lty=c(1,0,0), col=rgb(0,0,0, c(1,0.5, 0.3)), bg=rainbow(1, alpha=0), bty="n", cex=cex.legend, y.intersp=2, inset=0.0, seg.len=2, title.adj=c(2, 2)) 
    }
		abline( v=ln.v, h=ln.h, col="gray" )
	if (type == 'pdf') dev.off()
}
	

	
	#if (pdf) {dev.off()}
	
	#return(outname)
}
