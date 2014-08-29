#' MotifSetup Reference Class
#'
#' @field data a nested list holding the data
#' @field annotations list of annotations
#' 
#' @family SeqPlotsClasses
#' @export
#' 
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
