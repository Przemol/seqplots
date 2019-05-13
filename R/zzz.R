.onAttach <- function(libname, pkgname) {
    #Fix for annoying BiocGenerics namespace problems
    if(!"package:BiocGenerics" %in% search()) attachNamespace('BiocGenerics')
    packageStartupMessage(paste(
        'SeqPlots ready, type "run()" to start web interface (GUI) or',
        '"?getPlotSetArray" for console mode help.'
    ))
    msg <- sprintf(
        "Package '%s' is deprecated and will be removed from Bioconductor
         version %s", pkgname, "3.11")
    .Deprecated(msg=paste(strwrap(msg, exdent=2), collapse="\n"))
}
