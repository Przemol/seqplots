#' PlotSetList Reference Class
#'
#' @field data a nested list holding the data
#' @field annotations list of annotations
#' 
#' @section Subsetting:
#' 
#' \code{x} is an object of \code{PlotSetList} class:
#'  \itemize{
#'  \item \code{x[1:2]} produces \code{\link{PlotSetList}} with 2 feature/tracks
#'  pairs. 
#'  \item \code{x[[1]]} produces single \code{\link{PlotSetPair}}.
#' }
#' 
#' @include PlotSetPair-class.R
#' @family classes
#' @export
#' 
PlotSetList <- setRefClass("PlotSetList", 
    fields = list( data = "list", annotations = "list"),
    methods = list(
        npaires = function() {
            "Outputs the number (integer) of PlotSetPairs in the PlotSetList"
            length(data)
        },
        info = function() {
            "Outputs data.frame describing the content of PlotSetList"
            if( npaires() ) {
                as.data.frame(t(as.data.frame( 
                    sapply(1:length(data), function(x) c(
                        x, gsub('\n@', ' @ ', data[[x]]$desc))
                    ), row.names=c('ID', 'Pair name') 
                )))
            } else {
                NULL
            }
        },
        show = function() {
            cat( 
                'PlotSetList with', npaires(), 
                'feature/tracks pairs.\nContain:\n' 
            ) 
            print(info()); return(NULL);
        },
        get = function(i) {
            "Subseting method"
            PlotSetList(data=data[i])
        },
        plot = function(what='a', ...) {
            "Plot the PlotSetList, i.e. all PlotSetPairs in the list. 
            See \\code{\\link{plot}} for datails."
            if (what=="a") plotAverage(data, ...) 
            else if (what=="h") plotHeatmap(data, ...) 
            else message(paste(
                'Unknown type of the plot, use what="a"',
                'for average plot and what="h" for heatmap.'
            ))
        }
        
    )
)



