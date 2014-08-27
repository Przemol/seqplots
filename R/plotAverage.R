#' external wrapper function, plotting the average plot
#' 
#' This function is package internal and should not be executed directly
#' by users.
#' 
#' @keywords internal
#' 

plotAverage <- function(plotme, keepratio=FALSE, ord=NULL, labels=NULL, ...) {
 opar <- par(no.readonly = TRUE)['pty']
  if(keepratio) par(pty='s')
  if( length(labels) ) {
    labels <- labels[1:length(plotme)]
    plotme <- Map(function(x, y) {if(!is.na(y)) x[['desc']]<-y; return(x)}, data, labels)
  }
  if( length(ord) ) { plotme <- plotme[ ord ] }
  plotMext(plotme, ...) 
 par(opar)
 return(invisible(NULL))
}
