# 
# 
# require(methods)
# 
# #S5
# PlotSetArray <- setRefClass("PlotSetArray", fields = list( data = "list", annotations = "list")  )
# PlotSetArray$methods( show = function() {
#   if(length(data)) cat( 'PlotSetArray with', length(data), 'feature(s) and', length(data[[1]]), 'tracks.' ) 
#   else  cat( 'PlotSetArray with no data.' )  
# })
# PlotSetArray$methods( getRow = function(i) data[as.integer(i)] )
# PlotSetArray$methods( subset = function(i, j) data[[as.integer(i)]][[as.integer(j)]] )
# setMethod("[", c("PlotSetArray", "ANY", "missing", "ANY"), function(x, i, j, ..., drop=TRUE) x$getRow(i) )
# setMethod("[", c("PlotSetArray", "ANY", "ANY", "ANY"), function(x, i, j, ..., drop=TRUE) x$subset(i, j) )
# 
# 
# z=PlotSetArray(data=list(letters))
# 
# #S4
# PlotSetArrayS4 <- setClass("PlotSetArrayS4", slots = list( data = "list", annotations = "list") )
# getGeneric("[")
# 
# setMethod("[", c("PlotSetArrayS4", "integer", "missing", "ANY"), function(x, i, j, ..., drop=TRUE) x$getRow(i) )
# 
# setMethod("[", c("PlotSetArrayS4", "integer", "integer", "ANY"), function(x, i, j, ..., drop=TRUE) x$getRow(i) )
# 
# 
# 
# y=PlotSetArrayS4(data=list(letters))
# 
# hmap <- function (x, ...) 
#   UseMethod("heatmap")z
# 
# 
# track <- setClass("track",
#                   slots = c(x="numeric", y="numeric"))
