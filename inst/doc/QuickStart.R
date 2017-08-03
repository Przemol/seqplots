## ----echo=FALSE, results='asis', echo=FALSE------------------------------
err <- try({
    npos <- which(rev(strsplit(getwd(), '/')[[1]]) == "seqplots")-1
    if(!length(npos)) npos <- 1
    ppath <- paste(rep('../', npos), collapse = '')
})
if(class(err) =='try-error') ppath <- '../../'
cat(knitr::knit_child(
    text = gsub(
        "img/",
        paste0(ppath, "inst/seqplots/www/help/img/"),
        readLines( paste0(ppath, "inst/seqplots/www/help/QuickStart.md") )
    ), quiet = TRUE
), sep = '\n')

## ----echo=FALSE----------------------------------------------------------
sessionInfo()

