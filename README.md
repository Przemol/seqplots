![SeqPlots](https://github.com/Przemol/seqplots/wiki/img/ico_full_color.png)
=================================================================================

> An interactive tool for visualizing track signals and sequence motif densities along genomic features using average plots and heatmaps

[![Build Status](https://travis-ci.org/Przemol/seqplots.svg?branch=master)](https://travis-ci.org/Przemol/seqplots)
[![codecov.io](http://codecov.io/github/Przemol/seqplots/coverage.svg?branch=master)](http://codecov.io/github/Przemol/seqplots?branch=master)

![Examples of Seq Plots interface and outputs](https://github.com/Przemol/seqplots/wiki/img/SeqPlots_fig1_web.png)

:exclamation: **RELEASE NOTE** :exclamation:

GitHub repository holds development version of SeqPlots. The release is available on Bioconductor.
Please report the problem, bugs, unexpected behaviors and missing features using [**issue tracker**](../issues).

Live demo
---------

The working demonstration preview of SeqPlots is available on ShinyApps.io:
**https://seqplots.shinyapps.io/seqplots/**

Demo limits:

-	the application contains some pre-loaded data (*C. elegans* genes and histone modifications)
-	size of uploaded files is limited to 30MB, please bin or subset your tracks to fit within the limit (due to ShinyApps beta upload limitations)
-	only *C. elegans* genome is available (due ShinyApps beta space limits)
-	uploaded files will be deleted after some time

Introduction
------------

The SeqPlots is universal, web browser based tool for plotting
average track signal (e.g. reads coverage) and sequence motif density over
user specified genomic features. The data are visualized on linear plot,
with error estimates as fields, or as series of heatmaps that can be sorted
and clustered. The dual-purpose implementation allows running the software
locally on desktop or deploying it on server. The unique features of our
software are collaboration and data sharing capabilities, as well as
ability to store pre-calculated result matrixes, that combine many
sequencing experiments and in-silico generated tracks with multiple
different features. These binaries can be further used to generate new
combination plots on fly, run automated batch operations or share with
colleagues, who can adjust their plotting parameters without loading actual
tracks and recalculating numeric values.

Installation
------------

Install develpment version from Bioconductor:

```{r}
source("http://bioconductor.org/biocLite.R")
biocLite("seqplots")
```

To install GitHub vesrion (in order to solve dependencies install from Bioconductor first):
```{r}
if (!require("devtools")) install.packages("devtools")
devtools::install_github('przemol/seqplots', build_vignettes=FALSE)
```

Quick start
-----------

To start web browser GUI:

```{r}
library(seqplots)
run()
```

To start with R scripting mode:

```{r}
?getPlotSetArry
```

Mac OS X bundle app
-------------------
Mac OS X bundle is an easy way to use SeqPlots for Mac OS X users. It contains R binaries pre-installed Bioconductor dependencies. Additionally, the reference genome for *Caenorhabditis elegans* is included. The sequences for other popular model organisms can be downloaded using graphical user interface.

**Download the current version here:
https://github.com/Przemol/seqplots_osx/releases/latest**

The bundle requires OS X 10.6 (Snow Leopard) and above with X11 installed. X11 was included with OS X up to version 10.7 (Lion). OS X 10.8 (Mountain Lion) and above require installation of Xquartz package, see more here: http://support.apple.com/en-us/HT201341

Docker image
------------
SeqPlots is available as Docker image, which can be easily run on Mac OS, Windows and Linux systems. The image can be obtained from Docker Hub: https://hub.docker.com/r/przemol/seqplots/

Refer to https://www.docker.com/ for instructions how to install Docker and run the image.

References
----------

**R project and Bioconductor**

-   R Core Team (2014). R: A language and environment for statistical computing. R Foundation for Statistical Computing, Vienna, Austria. URL http://www.R-project.org/.
-   Bioconductor: Open software development for computational biology and bioinformatics R. Gentleman, V. J. Carey, D. M. Bates, B.Bolstad, M.Dettling, S. Dudoit, B. Ellis, L. Gautier, Y. Ge, and others 2004, Genome Biology, Vol. 5, R80.  URL http://www.bioconductor.org/.
- RStudio and Inc. (2014). shiny: Web Application Framework for R. R package version 0.10.1. http://shiny.rstudio.com/
- **Other CRAN packages:** digest, DBI,RSQLite, RJSONIO, plotrix, fields, grid, kohonen, Cairo and parallel
- **Bioconductor packages:** IRanges, BSgenome, Rsamtools, rtracklayer, GenomicRanges and Biostrings 

**JavaScript and CSS**

- jQuery framework - http://jquery.com
- Bootstrap - http://getbootstrap.com
- DataTables, Table plug-in for jQuery - http://www.datatables.net
- jQuery File Upload Plugin - https://github.com/blueimp/jQuery-File-Upload
- jQuery throttle - http://benalman.com/projects/jquery-throttle-debounce-plugin/
- jQuery Cookie Plugin - https://github.com/carhartl/jquery-cookie
- Modernizer JS library - http://modernizr.com
- JavaScript Templates - https://github.com/blueimp/JavaScript-Templates
- JavaScript Color Picker - http://jscolor.com
- md5-js - https://github.com/wbond/md5-js
- Font Awesome - http://fortawesome.github.io/Font-Awesome
- Google Fonts - https://www.google.com/fonts
- jQuery user interface - http://jqueryui.com (documentation)
- jquery.tocify.js: jQuery Table of Contents - https://github.com/gfranko/jquery.tocify.js (documentation)
- Strapdown https://github.com/arturadib/strapdown (documentation)
- Bootswatch themes - http://bootswatch.com (documentation)
- google-code-prettify - https://code.google.com/p/google-code-prettify (documentation)
- marked - https://github.com/chjj/marked (documentation)

**Important conceptual contribution to the project**

- Liu T, Ortiz J, Taing L, Meyer C, Lee B, Zhang Y, Shin H, Wong S, Ma J, Lei Y, et al. 2011. [Cistrome: an integrative platform for transcriptional regulation studies.](http://www.ncbi.nlm.nih.gov/pubmed/21859476) Genome Biology 12: R83.
- Thomas Williams, Colin Kelley and others (2010). Gnuplot 4.4: an interactive plotting program. URL http://www.R-project.org/.
- Kent, W.J., Sugnet, C.W., Furey, T.S., Roskin, K.M., Pringle, T.H., Zahler, A.M. and Haussler, a. D. (2002). [The Human Genome Browser at UCSC](http://www.ncbi.nlm.nih.gov/pubmed/12045153). Genome Research. 12:996–1006.
- Kent WJ, Zweig AS, Barber G, Hinrichs AS, Karolchik D. (2010). [BigWig and BigBed: enabling browsing of large distributed datasets.](http://www.ncbi.nlm.nih.gov/pubmed/20639541) Bioinformatics. 1;26(17):2204-7
- Nicol, J.W., Helt, G.A., Blanchard, S.G., Raja, A. and Loraine, A.E. (2009). [The Integrated Genome Browser: free software for distribution and exploration of genome-scale datasets.](http://www.ncbi.nlm.nih.gov/pubmed/19654113) Bioinformatics (Oxford, England). 25:2730–1.
- Thorvaldsdóttir, H., Robinson, J.T. and Mesirov, J.P. (2012). [Integrative Genomics Viewer (IGV): high-performance genomics data visualization and exploration.](http://www.ncbi.nlm.nih.gov/pubmed/22517427) Briefings in bioinformatics. bbs017


**Server deployment**

- Shiny Server - https://github.com/rstudio/shiny-server
- ShinyApps - https://github.com/rstudio/shinyapps


**Publications containing figures made by SeqPlots**

- Chen RA, Stempor P, Down TA, Zeiser E, Feuer SK, Ahringer J. [Extreme HOT regions are CpG-dense promoters in C. elegans and humans.](http://www.ncbi.nlm.nih.gov/pubmed/24653213) Genome Res 24(7):1138-1146 Jul 2014
- Latorre I, Chesney MA, Garrigues JM, Stempor P et al. [The DREAM complex promotes gene body H2A.Z for target repression](http://www.ncbi.nlm.nih.gov/pubmed/25737279). Genes Dev 2015 Mar 1;29(5):495-500.
