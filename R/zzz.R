.onAttach <- function(libname, pkgname) {
    #Fix for annoying BiocGenerics namespace problems
    if(!"package:BiocGenerics" %in% search()) attachNamespace('BiocGenerics')
    packageStartupMessage('SeqPlots loaded')
}
