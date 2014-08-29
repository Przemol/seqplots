#' PlotSetArray Reference Class
#'
#' @field data a nested list holding the data
#' @field annotations list of annotations
#' 
#' @include PlotSetPair-class.R
#' @family SeqPlotsClasses
#' @export
#' 
PlotSetArray <- setRefClass("PlotSetArray", 
    fields = list( data = "list", annotations = "list"),
    methods = list(
        nfeatures = function() {
            'Outputs the number of features in the PlotSetArray'
            length(data)
        },
        ntracks = function() {
            'Outputs the number of tracks in the PlotSetArray'
            if(length(data)) length(data[[1]]) else 0
        },
        pairind = function() {
            'Outputs the list of pair IDs'
            as.list(data.frame(t(expand.grid(1:nfeatures(), 1:ntracks()))))
        },
        unlist = function() {
            'Flattens PlotSetArray to PlotSetList'
            PlotSetList(data=lapply(pairind(), function(x) data[[x[1]]][[x[2]]] ))
        }
    )
)


PlotSetArray$methods( info = function() as.data.frame(t(as.data.frame( 
    sapply(pairind(), function(x) c(x[1], x[2], 
                                    gsub('\n@', ' @ ', data[[x[1]]][[x[2]]]$desc))), 
    row.names=c('FeatureID', 'TrackID', 'Pair name') ))) ) 
PlotSetArray$methods( show = function() {
    cat( 'PlotSetArray with', nfeatures(), 'feature(s) and', ntracks(), 'tracks.\nContain:\n' )
    print(info()); return(NULL);
})
PlotSetArray$methods( as.array = function(x, ...) { do.call(rbind, lapply( data, function(x) lapply(x, function(y) do.call(PlotSetPair, y) ))) })
PlotSetArray$methods( getByID = function(i) unlist()$get(i) )
PlotSetArray$methods( get = function(i, j) PlotSetArray( data=lapply( data[i], '[', j) ) )
PlotSetArray$methods( getPairs = function(i) PlotSetList(data=lapply( i, function(x) data[[x[2]]][[x[1]]] )) )
PlotSetArray$methods( plot = function(...) unlist()$plot(...) )
PlotSetArray$methods( getRow = function(i) data[as.integer(i)] )
PlotSetArray$methods( subset = function(i, j) data[[as.integer(i)]][[as.integer(j)]] )

setGeneric('plot')
setMethod(unlist, c("PlotSetArray"), function(x) x$unlist() )
setMethod(plot,   c("PlotSetArray"), function(x, ...) x$plot(...) )
setMethod("[", signature(x = "PlotSetArray", i = "ANY", j = "missing"),
          function (x, i, j, ...) {
              if((na <- nargs()) == 2)
                  x$getByID(i)
              else if(na == 3)
                  x$get(i, 1:x$ntracks())
              else stop("invalid nargs()= ",na)
          })
setMethod("[", c("PlotSetArray", "ANY", "vector"), function(x, i, j) x$get(i, j) )
setMethod("[[", c("PlotSetArray", "ANY"), function(x, i, ...) {
    if(length(i) > 1 ) stop('recursive indexing not allowed')
    do.call(PlotSetPair, x$getByID(i)$data[[1]])
})
