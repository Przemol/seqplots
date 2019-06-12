.onAttach <- function(libname, pkgname) {
    #Fix for annoying BiocGenerics namespace problems
    if(!"package:BiocGenerics" %in% search()) attachNamespace('BiocGenerics')
    packageStartupMessage(paste(
        'SeqPlots ready, type "run()" to start web interface (GUI) or',
        '"?getPlotSetArray" for console mode help.'
    ))
}
