# dd <- get(load('tests/0001_heatTest1_2x2.Rdata'))
# z=PlotSetArray(data=dd)
# PlotSetArray(data=dd)$getPairs(list(c( 'HTZ1_Differential_genes_TOP100_v2', 'HTZ1_JA00001_IL1andIL2_F_N2_L3_NORM_linear_1bp_IL010andIL009_averaged' )))

#' PlotSetPair Reference Class
#'
#' @field data a list holding the plot data
#' @field annotations list of annotations
#' 
PlotSetPair <- setRefClass("PlotSetPair", fields = list( data = "list", annotations = "list")  )

#' PlotSetList Reference Class
#'
#' @field data a nested list holding the data
#' @field annotations list of annotations
#' 
PlotSetList <- setRefClass("PlotSetList", fields = list( data = "list", annotations = "list")  )
PlotSetList$methods( npaires = function() length(data) )
PlotSetList$methods( info = function() as.data.frame(t(as.data.frame( 
    sapply(1:length(data), function(x) c(x, gsub('\n@', ' @ ', data[[x]]$desc))), 
    row.names=c('ID', 'Pair name') ))) ) 
PlotSetList$methods( show = function() {
    cat( 'PlotSetList with', npaires(), 'feature/tracks pairs.\nContain:\n' ) 
    print(info()); return(NULL);
})
#PlotSetList$methods( plotAverage = source('../inst/seqplots/functions/plotMext.R', local = TRUE)$value )
PlotSetList$methods( get = function(i) PlotSetList(data=data[i]) )
PlotSetList$methods( plot = function(what='a', ...) {
  if (what=="a") plotAverage(data, ...) else if (what=="h") plotHeatmap(data, ...) 
  else message('Unknown type of the plot, use what="a" for average plot and what="h" for heatmap')
})

#' PlotSetArray Reference Class
#'
#' @field data a nested list holding the data
#' @field annotations list of annotations
#' 
PlotSetArray <- setRefClass("PlotSetArray", fields = list( data = "list", annotations = "list")  )
PlotSetArray$methods( nfeatures = function() 
    length(data) )
PlotSetArray$methods( ntracks = function() 
    if(length(data)) length(data[[1]]) else 0 )
PlotSetArray$methods( pairind = function() 
    as.list(data.frame(t(expand.grid(1:nfeatures(), 1:ntracks())))) )
PlotSetArray$methods( unlist = function() 
    PlotSetList(data=lapply(pairind(), function(x) data[[x[1]]][[x[2]]] )) )
PlotSetArray$methods( info = function() as.data.frame(t(as.data.frame( 
    sapply(pairind(), function(x) c(x[1], x[2], 
    gsub('\n@', ' @ ', data[[x[1]]][[x[2]]]$desc))), 
    row.names=c('FeatureID', 'TrackID', 'Pair name') ))) ) 
PlotSetArray$methods( show = function() {
    cat( 'PlotSetArray with', nfeatures(), 'feature(s) and', ntracks(), 'tracks.\nContain:\n' )
    print(info()); return(NULL);
})


PlotSetArray$methods( as.array = function(x, ...) { do.call(rbind, worm$data) })

PlotSetArray$methods( getByID = function(i) unlist()$get(i) )
PlotSetArray$methods( get = function(i, j) PlotSetArray( data=lapply( worm$data[i], '[', j) ) )

PlotSetArray$methods( getPairs = function(i) PlotSetList(data=lapply( i, function(x) data[[x[2]]][[x[1]]] )) )
PlotSetArray$methods( plot = function(...) unlist()$plot(...) )


setGeneric('pairs')
setGeneric('plot')
setMethod(unlist, c("PlotSetArray"), function(x) x$unlist() )
setMethod(plot,   c("PlotSetArray"), function(x, ...) x$plot(...) )
setMethod(plot,   c("PlotSetList"), function(x, ...) x$plot(...) )
#setMethod(pairs,  c("PlotSetArray"), function(x) x$pairs() )


PlotSetArray$methods( getRow = function(i) data[as.integer(i)] )
PlotSetArray$methods( subset = function(i, j) data[[as.integer(i)]][[as.integer(j)]] )


# PlotSetArray$methods( pairs = function() as.list(data.frame(t(expand.grid(1:2, 1:2)))))
# 
# PlotSetArray$methods( pairs = function(i, j) data[[as.integer(i)]][[as.integer(j)]] )



#Set method forgeneric functions
setMethod("[", signature(x = "PlotSetArray", i = "ANY", j = "missing"),
          function (x, i, j, ...) {
              message('NARGS: ', nargs())
              if((na <- nargs()) == 2)
                  x$getByID(i)
              else if(na == 3)
                  x$get(i, 1:worm$ntracks())
              else stop("invalid nargs()= ",na)
          })
setMethod("[", c("PlotSetArray", "ANY", "vector"), function(x, i, j) x$get(i, j) )

# setMethod("[", c("PlotSetArray", "ANY", "missing", "ANY"), function(x, i, j, ..., drop=TRUE) x$get(i, 1:worm$ntracks()) )
# setMethod("[", c("PlotSetArray", "vector"), function(x, i) x$getByID(i) )


MotifSetup <- setRefClass("MotifSetup", fields = list( data = "list", annotations = "list")  )
MotifSetup$methods( nmotifs = function() 
    length(data) )
MotifSetup$methods( show = function() 
    cat( 'MotifSetup with', nmotifs(), ' motifs.' ) )
MotifSetup$methods( addMotif = function(pattern, window=200L, heatmap=TRUE, revcomp=TRUE, genome=NULL, name=pattern ) {
    ind <- length(data)+1
    data[[ind]] <<- list(pattern=pattern, window=window, heatmap=heatmap, revcomp=revcomp, genome=genome, name=name)
    names(data)[[ind]] <<- name
})
MotifSetup$methods( addBigWig = function(file_path) 
    data[[length(data)+1]] <<- file_path)


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

# getPlotSetArray(features ='/Users/przemol/SeqPlots_data/files/HTZ1_Differential_genes_TOP100_v2.gff', tracks ='/Users/przemol/SeqPlots_data/files/worm_pileup_unique.bw',refgenome='ce10')

# z<- getPlotSetArray(features ='tests/PooledCapTSS_ce.gff', tracks ='tests/cpg_200bp_ce10.bw',refgenome='ce10')
