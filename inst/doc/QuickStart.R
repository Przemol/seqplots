## ----echo=FALSE, results='asis', echo=FALSE------------------------------
# knit the first three lines of first.Rmd
cat(knitr::knit_child(
    text = gsub(
        "img/",
        "../inst/seqplots/www/help/img/",
        readLines('../inst/seqplots/www/help/QuickStart.md')
    ),
    quiet = TRUE
), sep = '\n')

## ----echo=FALSE----------------------------------------------------------
sessionInfo()

