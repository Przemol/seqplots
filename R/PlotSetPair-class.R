#' PlotSetPair Reference Class
#'
#' @field means numeric vector of means
#' @field stderror numeric vector of standard errors
#' @field conint numeric vector of 95\% confidence intervals
#' @field all_ind numeric vector giving the relative position of the bins in the
#'   genome
#' @field numeric value giving the length of anchored distance, NULL for point
#'   feature plots
#' @field desc character string describing the PlotSetPair
#' @field heatmap numeric matrix used for plotting the heatmap
#' 
#' @family classes
#' @export
#' 
PlotSetPair <- setRefClass("PlotSetPair", 
    fields = list(
        means='numeric', stderror='numeric', conint='numeric', 
        all_ind='numeric', e='ANY', desc='character', heatmap='matrix',
        anno='ANY'
    ),
    methods = list(
        show = function() {
            cat( 'PlotSetPair contatining:', gsub('\n@', ' @ ', desc) )
        },
        as.list = function() {
            "Convert to PlotSetPair list."
            list(
                means=means, stderror=stderror, conint=conint, 
                all_ind=all_ind, e=e, desc=desc, heatmap=heatmap
            )
        },
        plot = function(what='a', ...) {
            "Plot the PlotSetPair class. See \\code{\\link{plot}} for datails."
            if (what=="a") plotAverage(list(.self), ...) 
                else if (what=="h") plotHeatmap(list(.self), ...) 
            else message(paste(
                'Unknown type of the plot, use what="a"',
                'for average plot and what="h" for heatmap'))
        }
    )
)

