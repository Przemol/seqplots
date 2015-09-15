Welcome to **SeqPlots**
=======================
> An interactive tool for visualizing NGS signals and sequence motif densities along genomic features using average plots and heatmaps.

![Examples of Seq Plots interface and outputs](img/ico_full_color.png)
![Examples of Seq Plots interface and outputs](img/SeqPlots_fig1_web.png)

Summary
-------
SeqPlots is a web browser tool for plotting average track signals (e.g. read coverage) and sequence motif densities over user specified genomic features. The data can be visualized in linear plots with error estimates or as series of heatmaps that can be sorted and clustered. The software can be run locally on a desktop or deployed on a server and allows easy data sharing.  SeqPlots pre-calculates and stores binary result matrices, allowing rapid plot generation.  Plots can also be run in batch.

Availability
------------
SeqPlots is distributed as [Bioconductor package](http://www.bioconductor.org/packages/release/bioc/html/seqplots.html) available on platforms and operating systems supported by R project. A standalone [SeqPlot OS X bundle](https://github.com/Przemol/seqplots_osx/zipball/master), combing R and all required packages, is available as for Mac OS X (10.6 or higher). SeqPlots can also be deployed on a server using free and open sourced (GPL licensed) [Shiny Server](https://github.com/rstudio/shiny-server).

Key features
------------
- Easy to use web interface (R or shell expertise not required)
- Web server or desktop versions
-	Generates publication quality plots out of the box
-	Plots average signals or heatmaps
-	Accepts Wiggle, BedGraph, BigWiggle, and GFF and BED formats
-	Calculates motif density from reference genome packages
-	Tracks and features are searchable and old calculations stored
-	Converts tracks to binary BigWiggle format for rapid data extraction and efficient storage
-	Implemented using Shiny R framework providing internet browser reactive GUI and session based connectivity (websocets)

Video presentation
------------------
<div class="embed-responsive embed-responsive-16by9">
  <iframe width="853" height="480" src="https://www.youtube.com/embed/e29CMRriROM?VQ=HD720&amp;rel=0&amp;showinfo=0" frameborder="0" allowfullscreen></iframe>
</div>
<br />

Issues and bugs
---------------
Please visit [**issue tracker**](https://github.com/Przemol/seqplots/issues) to view currently know issues. To report a new issue/bug/feature request please click [**here**](https://github.com/Przemol/seqplots/issues/new). If the issue is connected to file upload please attach the file in the form.
