#' Modified image.plot function from "fields" package, the grey rectangle
#' is used as background for heatmap, making NAs distinguishable from the data
#'
#'
#' \code{
#' #added following line of code:
#' rect(usr[1], usr[3], usr[2], usr[4], col="grey")
#' }
#' This function is package internal and should not be executed directly
#' by users.
#' 
#' For more help see documentation of image.plot function: 
#' \code{\link[fields]{image.plot}}
#' 
#' @return \code{NULL}
#'
#' @keywords internal
#' 
imPlot2 <- function (
    ..., add = FALSE, nlevel = 64, horizontal = FALSE, 
    legend.shrink = 0.9, legend.width = 1.2, 
    legend.mar = ifelse(horizontal,  3.1, 5.1), 
    legend.lab = NULL, legend.line = 2, graphics.reset = FALSE, 
    bigplot = NULL, smallplot = NULL, legend.only = FALSE, 
    col = tim.colors(nlevel), 
    lab.breaks = NULL, axis.args = NULL, legend.args = NULL, 
    midpoint = FALSE, border = NA, lwd = 1, xinds=NULL, e=NULL, ylast=0, afs=1) {
    
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
    temp <- imageplot.setup(
        add = add, legend.shrink = legend.shrink, 
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
                rect(
                    par("usr")[1],par("usr")[3],par("usr")[2],
                    par("usr")[4],col = "lightgrey"
                )
                axis(2, at=list(...)$ylim, labels=list(...)$ylim, cex.axis=afs*0.9,  col.axis='darkgrey')
                if(is.null(e)) { 
                    axis(
                        1, at=c(min(xinds), 0,  max(xinds)), 
                        labels=c(num2bp(min(xinds)), '0bp', num2bp(max(xinds))),
                        cex.axis=afs
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
            })
        }
        else {
            poly.image(
                ..., add = add, col = col, midpoint = midpoint, 
                border = border, lwd.poly = lwd
            )
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
        legend.args <- list(
            text = legend.lab, side = ifelse(horizontal, 1, 4), 
            line = legend.line)
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
