#if( !'seqplots' %in% .packages() ) {
suppressPackageStartupMessages({
    require(parallel)
    require(shiny)
    require(jsonlite)
    require(RSQLite)
    require(rtracklayer)
    require(BSgenome)
    require(kohonen)
    require(plotrix)
    require(fields)
    library(RColorBrewer) 
    library(seqplots)
})
#}


if(Sys.getenv('root') == '' ) {
  source('server_config.R')
}
.libPaths(c( file.path(Sys.getenv('root'), 'genomes'), .libPaths() ))

options(shiny.deprecation.messages=FALSE)
