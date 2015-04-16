#' Wrapper function, plotting the average plot
#' 
#' This function is package internal and should not be executed directly
#' by users.
#' 
#' @param INPUTS list of pre calculated inputs
#' @param main an overall title for the plot: see title.
#' @param xlab a title for the x axis: see title.
#' @param ylab a title for the y axis: see title.
#' @param plotScale scale the available data before ploting, can be "linear" 
#'  (do not scale), "log2" or "zscore"
#' 
#' @return \code{NULL}
#' 
#' @inheritParams plot
#' @keywords internal
#'    

plotMext <- function(
    INPUTS, xlim=NULL, ylim=NULL, main=NULL, xlab='', ylab='', 
    plotScale='linear', type='full', error.estimates=TRUE, legend=TRUE, 
    legend_ext=FALSE, legend_pos='topright', legend_ext_pos="topleft",
    cex.axis=14, cex.lab=16, cex.main=20, cex.legend=10, ln.v=TRUE, 
    ln.h=NULL, colvec=NULL, pointsize=12, ...) {
    
    cex.axis   <- cex.axis   / pointsize
    cex.lab    <- cex.lab    / pointsize
    cex.main   <- cex.main   / pointsize
    cex.legend <- cex.legend / pointsize
    
    opar <- par()[c('cex', 'mar')]
    par(cex=1)
    
    mm <- length(INPUTS)
    
    anchor <- INPUTS[[1]]$e
    if (ln.v) { ln.v <- c(0, anchor) } else { ln.v <- NULL }
    
    if (plotScale == 'log2') {
        for (i in 1:mm) { 
            co <- diff(range(INPUTS[[i]]$means));
            INPUTS[[i]]$means <- log2(abs(INPUTS[[i]]$means));
            co <- diff(range(INPUTS[[i]]$means))/co 
            INPUTS[[i]]$stderror <- co*INPUTS[[i]]$stderror
            INPUTS[[i]]$conint <- co*INPUTS[[i]]$conint
        }
    } else if  (plotScale == 'zscore') {
        for (i in 1:mm) { 
            co <- diff(range(INPUTS[[i]]$means))
            INPUTS[[i]]$means <- as.numeric(scale(INPUTS[[i]]$means))
            co <- diff(range(INPUTS[[i]]$means))/co
            INPUTS[[i]]$stderror <- co*INPUTS[[i]]$stderror
            INPUTS[[i]]$conint <- co*INPUTS[[i]]$conint
        }
    }
    
    if (is.null(ylim)) {
        ylim[1] <- min( sapply(INPUTS, function(INPUT){ 
            min(INPUT$means - INPUT$conint, na.rm=TRUE) }) )
        ylim[2] <- max( sapply(INPUTS, function(INPUT){ 
            max(INPUT$means + INPUT$conint, na.rm=TRUE) }) )
    }
    
    if ( !is.null(colvec) ) {
        cols <- colvec
    } else {
        cols <- c("darkblue", "darkgreen", "darkred", "darkmagenta", "darkgray",
                    "darkorange", "darkcyan", "black", rainbow(mm-8))
    }
    legendText=NULL
    
    if (type == 'legend') {
        legendText <- sapply(INPUTS, '[[', 'desc')
        plot.new()
        if(legend) {
            legend(
                legend_pos, legendText, col=cols, bg=rainbow(1, alpha=0),  
                bty="n", cex=cex.legend, y.intersp=2, inset=0.0, seg.len=2, 
                title.adj=c(2, 2), lwd=15, pch=0, lty=0
            )
        }
        if(error.estimates && legend_ext) { 
            legend(
                legend_ext_pos, 
                c("Mean\n(solid line)","Standard error\n(dark area)", 
                    "95% CI\n(light area)"), pch=c(-1, 0, 0), lwd=c(3,15,15), 
                lty=c(1,0,0), col=rgb(0,0,0, c(1,0.5, 0.3)), 
                bg=rainbow(1, alpha=0), bty="n", cex=cex.legend, y.intersp=2, 
                inset=0.0, seg.len=2, title.adj=c(2, 2)) 
        }
    } else {
        par(mar=c(3.2+cex.lab, 3.2+cex.lab, 2+cex.main, 3))
        
        for (i in 1:mm) {
            INPUT <- INPUTS[[i]]
            xinds <- if (is.null(xlim)) range(INPUT$all_ind) else xlim
            if (i==1) { 
                plot(
                    INPUT$all_ind, INPUT$means, type="n", main=main, xlab=xlab,
                    ylab=ylab, xlim=xlim, ylim=ylim, cex.axis=cex.axis, 
                    cex.lab=cex.lab, cex.main=cex.main, xaxt="n", ...
                ) 
                if( !any(c( 
                    length(list(...)[['xaxt']])>0, 
                    list(...)[['axes']]==FALSE )
                )) { 
                    if(is.null(anchor)) { 
                        axis(
                            1, at=c(min(xinds), 0,  max(xinds)), 
                            labels=c(num2bp(min(xinds)), '0bp', 
                                     num2bp(max(xinds))), cex.axis=cex.axis
                        ) 
                    } else {
                        axis(
                            1, at=c(min(xinds), 0,  anchor, max(xinds)), 
                            labels=c(
                                num2bp(min(xinds)), '0bp', '0bp', 
                                num2bp(max(xinds)-anchor)
                            ), cex.axis=cex.axis
                        )
                    }
                }
            }
            if(error.estimates) { 
                dispersion(
                    INPUT$all_ind, INPUT$means, INPUT$conint, type="l", 
                    fill=adjustcolor(cols[i], 0.5), lty=2
                ) 
            }
            if(error.estimates) { 
                dispersion(
                    INPUT$all_ind, INPUT$means, INPUT$stderror, type="l", 
                    fill=adjustcolor(cols[i], 0.3), lty=2
                ) 
            }
            lines( INPUT$all_ind, INPUT$means, col=cols[i])
            legendText <- c(legendText, INPUT$desc)
        }
        if(legend) {
            legend(
                legend_pos, legendText, col=cols, bg=rainbow(1, alpha=0),  
                bty="n", cex=cex.legend, y.intersp=2, inset=0.0, seg.len=2, 
                title.adj=c(2, 2), lwd=15, pch=0, lty=0
            )
        }
        if(error.estimates && legend_ext) { 
            legend(
                legend_ext_pos, 
                c("Mean\n(solid line)", "Standard error\n(dark area)", 
                    "95% CI\n(light area)"), pch=c(-1, 0, 0), lwd=c(3,15,15), 
                lty=c(1,0,0), col=rgb(0,0,0, c(1,0.5, 0.3)), 
                bg=rainbow(1, alpha=0), bty="n", cex=cex.legend, y.intersp=2, 
                inset=0.0, seg.len=2, title.adj=c(2, 2)
            ) 
        }
        abline( v=ln.v, h=ln.h, col="gray" )
    }
    par(opar)
    
}
