#' Converts numeric values labels with metric system suffix, i.e k, M, G, etc. 
#'
#' @param n The numeric value, that will be converted to label.
#' 
#' @return \code{character}
#'
#' @keywords internal
#' 
num2sci <- function( n ) {
    number <- abs(n)
    if(number == n) sign <- '' else sign <- '-'
    
    lut <- c(1e-24, 1e-21, 1e-18, 1e-15, 1e-12, 1e-09, 1e-06, 
             0.001, 1, 1000, 1e+06, 1e+09, 1e+12, 1e+15, 1e+18, 1e+21, 
             1e+24)
    pre <- c("y", "z", "a", "f", "p", "n", "u", "m", "", "k", 
             "M", "G", "T", "P", "E", "Z", "Y")
    ix <- findInterval(number, lut)
    if (lut[ix]!=1) {
        sistring <- paste0(
            sign, format(round(number/lut[ix], 3), digits = 3), pre[ix]
        )
        
    } else {
        sistring <- format(n, digits=3)
    }
    return(sistring)
}

#' Converts numeric values to labels with base pairs units, 
#' i.e bp, kb, Mb, Gb, etc. 
#'
#' @param n The numeric value, that will be converted to label.
#' 
#' @return \code{character}
#'
#' @keywords internal
#' 
num2bp <- function( n ) {
    if(n == 0)  return(paste0(0, 'bp'))
    
    number <- abs(n)
    if(number == n) sign <- '' else sign <- '-'
    
    lut <- c(1e-24, 1e-21, 1e-18, 1e-15, 1e-12, 1e-09, 1e-06, 
             0.001, 1, 1000, 1e+06, 1e+09, 1e+12, 1e+15, 1e+18, 1e+21, 
             1e+24)
    pre <- c("yb", "zb", "ab", "fb", "pb", "nb", "ub", "mb", "", "kb", 
             "Mb", "Gb", "Tb", "Pb", "Eb", "Zb", "Yb")
    ix <- findInterval(number, lut)
    if (lut[ix]!=1) {
        sistring <- paste0(
            sign, format(round(number/lut[ix], 3), digits = 3), pre[ix]
        )

    } else {
        sistring <- paste0(format(n, digits=3), 'bp')
    }
    return(sistring)
}
