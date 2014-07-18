

require(methods)
#dd <- get(load('tests/0001_heatTest1_2x2.Rdata'))

PlotSetPair <- setRefClass("PlotSetPair", fields = list( data = "list", annotations = "list")  )

#S5 class definition
message(getwd())
PlotSetList <- setRefClass("PlotSetList", fields = list( data = "list", annotations = "list")  )
PlotSetList$methods( npaires = function() 
    length(data) )
PlotSetList$methods( show = function() 
    cat( 'PlotSetList with', npaires(), 'feature/tracks pairs.' ) )
#PlotSetList$methods( plotAverage = source('../inst/seqplots/functions/plotMext.R', local = TRUE)$value )
PlotSetList$methods( get = function(i) PlotSetList(data=data[i]) )
PlotSetList$methods( plot = function(what='a', ...) {
  if (what=="a") plotAverage(data, ...) else if (what=="h") plotHeatmap(data, ...) else stop('Unknown type of the plot!')
})






PlotSetArray <- setRefClass("PlotSetArray", fields = list( data = "list", annotations = "list")  )
PlotSetArray$methods( nfeatures = function() 
    length(data) )
PlotSetArray$methods( ntracks = function() 
    if(length(data)) length(data[[1]]) else 0 )
PlotSetArray$methods( pairind = function() 
    as.list(data.frame(t(expand.grid(1:nfeatures(), 1:ntracks())))) )
PlotSetArray$methods( unlist = function() 
    PlotSetList(data=lapply(pairind(), function(x) data[[x[2]]][[x[1]]] )) )
PlotSetArray$methods( pairs = function() as.data.frame(t(as.data.frame( 
    sapply(pairind(), function(x) c(x[1]*(x[2]-1)+x[2], x[1], x[2], 
    gsub('\n@', ' *AND* ', data[[x[2]]][[x[1]]]$desc))), 
    row.names=c('ID', 'FeatureID', 'TrackID', 'Pair name') ))) ) 
PlotSetArray$methods( show = function() cat( 'PlotSetArray with', nfeatures(), 
    'feature(s) and', ntracks(), 'tracks.' ) )
PlotSetArray$methods( show = function() cat( 'PlotSetArray with', nfeatures(), 
                                             'feature(s) and', ntracks(), 'tracks.' ) )
PlotSetArray$methods( getByID = function(i) unlist()$get(i) )
PlotSetArray$methods( get = function(i, j) PlotSetList( data=list(data[[i]][[j]]) ) )
PlotSetArray$methods( plot = function(...) unlist()$plot(...) )


setGeneric('pairs')
setGeneric('plot')
setMethod(unlist, c("PlotSetArray"), function(x) x$unlist() )
setMethod(plot,   c("PlotSetArray"), function(x) x$plot() )
setMethod(pairs,  c("PlotSetArray"), function(x) x$pairs() )


PlotSetArray$methods( getRow = function(i) data[as.integer(i)] )
PlotSetArray$methods( subset = function(i, j) data[[as.integer(i)]][[as.integer(j)]] )


# PlotSetArray$methods( pairs = function() as.list(data.frame(t(expand.grid(1:2, 1:2)))))
# 
# PlotSetArray$methods( pairs = function(i, j) data[[as.integer(i)]][[as.integer(j)]] )



#Set method forgeneric functions
setMethod("[", c("PlotSetArray", "ANY", "missing", "ANY"), function(x, i, j, ..., drop=TRUE) x$getByID(i) )
setMethod("[", c("PlotSetArray", "ANY", "ANY", "ANY"), function(x, i, j, ..., drop=TRUE) x$get(i, j) )


# co <- lapply(input$plot_this, function(x) fromJSON(x))
# pl <- lapply(co, function(x) values$grfile[[x[2]]][[x[1]]] )
# 
# z=PlotSetArray(data=dd)
# y=PlotSetArray()
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

# getPlotSetArray(features ='/Users/przemol/SeqPlots_data/files/HTZ1_Differential_genes_TOP100_v2.gff', 
#                 tracks ='/Users/przemol/SeqPlots_data/files/worm_pileup_unique.bw',
#                 refgenome='ce10')

# z<- getPlotSetArray(features ='tests/PooledCapTSS_ce.gff', tracks ='tests/cpg_200bp_ce10.bw',refgenome='ce10')
