
newsummarymethod <- setMethod("summary", "BigWigFile",
  function(object, which = as(seqinfo(object), "GenomicRanges"), size = 1L, type = c("mean", "min", "max", "coverage", "sd"), defaultValue = NA_real_) {
    message('Summary allowing NAs')
    which <- as(which, "GRanges")
    if (!is.numeric(size))
      stop("'size' must be numeric")
    size <- recycleIntegerArg(size, "size", length(which))
    type <- match.arg(type)
    if (type == "sd") type <- "std"
    if (!isSingleNumberOrNA(defaultValue))
    stop("'defaultValue' must be a single number or NA")
    summaryList <- .Call('BWGFile_summary_withNA', path.expand(path(object)),
      as.character(seqnames(which)),
      ranges(which), size, type,
      as.numeric(defaultValue))
    names(summaryList) <- names(which)
    return(RleList(summaryList))
})

.onLoad <- function(libname, pkgname) {
	message('Loaded OK')
}