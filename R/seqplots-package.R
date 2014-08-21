#' @docType package
#' @name seqplots
#' 
#' @title 
#' SeqPlots - An interactive tool for visualizing track signals and sequence 
#' motif densities along genomic features using average plots and heatmaps.
#' 
#' @description
#' SeqPlots is a web browser tool for plotting average track signals (e.g. read 
#' coverage) and sequence motif densities over user specified genomic features. 
#' The data can be visualized in linear plots with error estimates or as series 
#' of heatmaps that can be sorted and clustered. The software can be run locally 
#' on a desktop or deployed on a server and allows easy data sharing. SeqPlots 
#' pre-calculates and stores binary result matrices, allowing rapid plot 
#' generation. Plots can also be run in batch.
#' 
#' @references https://bitbucket.org/przemol/seqplots/wiki/Home
#' @author Przemyslaw Stempor
#' @details
#' \tabular{ll}{
#'  Package: \tab SeqPlots\cr
#'  Type: \tab Package\cr
#'  Version: \tab 0.9.2RC\cr
#'  Date: \tab 2013-07-01\cr
#'  License: \tab BSD\cr
#'  LazyLoad: \tab yes\cr
#' }
#' 
#' @keywords CHiP-seq genomics plotting sequencing
#' 
#' @import digest rtracklayer GenomicRanges BSgenome Biostrings IRanges methods
#' @import DBI RSQLite parallel RJSONIO Cairo
#' @import grid methods
#' @importFrom fields image.plot set.panel imageplot.info imageplot.setup tim.colors poly.image
#' @importFrom kohonen supersom
#' @importFrom plotrix dispersion
#' @importFrom shiny runApp
#' @importClassesFrom rtracklayer file
NULL

#@importFrom rtracklayer import export BigWigFile summary SeqinfoForBSGenome wigToBigWig
