#' MotifSetup Reference Class
#' 
#' @usage
#' MotifSetup()$addMotif(pattern, window=200L, heatmap=TRUE, 
#'             revcomp=TRUE, genome=NULL, name=pattern)
#'
#' @field data a nested list holding the data
#' @field annotations list of annotations
#' 
#' @family classes
#' @export
#' 
MotifSetup <- setRefClass("MotifSetup", fields = list( data = "list", annotations = "list")  )
MotifSetup$methods( nmotifs = function() {
    "Prints number of motifs in class"
    length(data) 
})
MotifSetup$methods( show = function() {
    "Show method"
    cat( 'MotifSetup with', nmotifs(), ' motifs.' ) 
})
MotifSetup$methods( addMotif = function(pattern, window=200L, heatmap=TRUE, revcomp=TRUE, genome=NULL, name=pattern ) {
    "Adds new motif."
    ind <- length(data)+1
    data[[ind]] <<- list(pattern=pattern, window=window, heatmap=heatmap, revcomp=revcomp, genome=genome, name=name)
    names(data)[[ind]] <<- name
    return(.self)
})
MotifSetup$methods( addBigWig = function(file_path) {
    "Adds new BigWig file."
    data[[length(data)+1]] <<- file_path
})
