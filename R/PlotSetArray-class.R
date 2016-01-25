#' PlotSetArray Reference Class
#' 
#' @field data a nested list holding the data
#' @field annotations list of annotations
#'   
#' @section Subsetting: \code{x} is an object of \code{PlotSetArray} class: 
#'   \itemize{ \item \code{x[1:2,1:2]} produces \code{\link{PlotSetArray}} with
#'   2 feature(s) and 2 tracks. \item \code{x[1:2]} produces
#'   \code{\link{PlotSetList}} with 2 feature/tracks pairs. \item \code{x[[1]]}
#'   produces single \code{\link{PlotSetPair}}. \item \code{unlist(x)} produces
#'   \code{\link{PlotSetList}} with all feature/tracks pairs. \item
#'   \code{x$as.array()} produces the matrix of \code{\link{PlotSetPair}} 
#'   classes with all feature/tracks pairs. }
#'   
#' @include PlotSetPair-class.R
#' @family classes
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
            PlotSetList(
                data=lapply(pairind(), function(x) data[[x[1]]][[x[2]]] )
            )
        },
        info = function() {
            "Outputs data.frame describing the content of PlotSetList"
            if( nfeatures() & ntracks() ) {
                as.data.frame(t(as.data.frame( 
                    sapply(pairind(), function(x) c(x[1], x[2], 
                        gsub('\n@', ' @ ', data[[x[1]]][[x[2]]]$desc))), 
                    row.names=c('FeatureID', 'TrackID', 'Pair name') 
                )))
            } else {
                NULL
            }
        },
        show = function() {
            cat( 
                'PlotSetArray with', nfeatures(), 'feature(s) and', ntracks(), 
                'tracks.\nContain:\n' 
            )
            print(info()); return(NULL);
        },
        as.array = function(x, ...) { 
            "Converts PlotSetArray calss to matrix of PlotSeqPairs"
            do.call(rbind, lapply( data, function(x) lapply(x, 
                function(y) do.call(PlotSetPair, y) 
            )))
        },
        getByID = function(i) {
            "Subseting method, returns PlotSeqList"
            unlist()$get(i)
        },
        get = function(i, j) {
            "Subseting method, returns PlotSetArray"
            PlotSetArray( data=lapply( data[i], '[', j) )
        },
        getPairs = function(i) {
            "Subseting method, takes pair IDs list, returns PlotSetList"
            PlotSetList(data=lapply( i, function(x) data[[x[2]]][[x[1]]] ))
        },
        plot = function(...) {
            "Plot the PlotSetArray, i.e. all PlotSetPairs within class. 
            See \\code{\\link{plot}} for datails."
            unlist()$plot(...)
        },
        getRow = function(i) {
            "Subseting method, get row of data as list"
            data[as.integer(i)]
        },
        subset = function(i, j) {
            "Subseting method, get PlotSetPair as list"
            data[[as.integer(i)]][[as.integer(j)]]
        },
        anno = function(n) {
            'Extracts the genomic locations for nth feature as GRanges'
            if( length(annotations) < n ) return(NA) else return(annotations[[n]])
        }
    )
)
