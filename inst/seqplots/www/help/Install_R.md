Installation - R/Bioconductor
=============================

> The Bioconductor package runs SeqPlots locally on desktop computers from the R
environment. It is available for Windows, GNU/Linux and Mac OS X operating
systems. For further information please refer to [SeqPlots Bioconductor webpage](http://www.bioconductor.org/packages/release/bioc/html/seqplots.html).


**System requirements:**

-   [R 3.1 or higher](http://www.r-project.org/)

How to install
--------------

To install SeqPlots package, start R and enter:

```{r eval=FALSE}
source("http://bioconductor.org/biocLite.R")
biocLite("seqplots")
```

How to start
------------

To start SeqPlots web interface, start R and enter:

```{r eval=FALSE}
library(seqplots)
run()
```

After a successful initiation the user interface will be opened in your
default web browser. For further usage please refer to
**Quick start guide** or specific chapters.

The `run` function accepts "root" argument, which allows to change
the data location folder (by default your home directory will be used),
e.g.:

```{r eval=FALSE}
run(root='/path/to/data/location')
```

### Additional genome packages

Genomic packages can be installed using standard bioconductor installer
(Internet connection is required). For example, to instal human reference
genome (hg19):

```{r eval=FALSE}
source("http://bioconductor.org/biocLite.R")
biocLite("BSgenome.Hsapiens.UCSC.hg19")
```

Genome packages are required before uploading files for plotting. Full
list of supported genomes is avilable here:
http://www.bioconductor.org/packages/release/BiocViews.html#___BSgenome

