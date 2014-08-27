.onAttach <- function(libname, pkgname) {
    #Fix for annoying BiocGenerics namespace problems
    if(!"package:BiocGenerics" %in% search()) attachNamespace('BiocGenerics')
    packageStartupMessage('SeqPlots ready, type "run()" to start web interface (GUI) or "?getPlotSeqArray" for console mode help.')
}
