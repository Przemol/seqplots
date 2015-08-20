#if( !'seqplots' %in% .packages() ) {
suppressPackageStartupMessages({
    require(parallel)
    require(shiny)
    require(jsonlite)
    require(RSQLite)
    require(rtracklayer)
    require(BSgenome)
    require(kohonen)
    require(Cairo)
    require(plotrix)
    require(fields)
    library(RColorBrewer) 
    library(seqplots)
})
#}


if(Sys.getenv('root') == '' ) {
  source('server_config.R')
}
.libPaths(c( .libPaths(), file.path(Sys.getenv('root'), 'genomes') ))

GENOMES <- BSgenome:::installed.genomes(splitNameParts=TRUE)$provider_version
if( length(GENOMES) ) 
    names(GENOMES) <- gsub('^BSgenome.', '', BSgenome:::installed.genomes())

options(shiny.deprecation.messages=FALSE)
