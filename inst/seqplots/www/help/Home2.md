Welcome to **SeqPlots** genome analysis 
=======================================
![Examples of Seq Plots interface and outputs](img/ico_full_color.png)
<h4 align="center">Article [[PDF](https://wellcomeopenresearch.org/articles/1-14/v1/pdf)]</h4>
Stempor P and Ahringer J. **SeqPlots - Interactive software for exploratory data analyses, pattern discovery and visualization in genomics** [version 1; referees: 2 approved, 1 approved with reservations]. *Wellcome Open Res 2016*, **1**:14 
[doi: 10.12688/wellcomeopenres.10004.1](http://dx.doi.org/10.12688/wellcomeopenres.10004.1)

![Examples of Seq Plots interface and outputs](img/SeqPlots_fig1_web.png)

Summary
-------
SeqPlots is a user-friendly exploratory data analysis (EDA) and visualization software for genomics. After choosing groups of signal and feature files and defining plotting parameters, users can generate profile plots of average signal or heat maps clustered using different algorithms in a matter of seconds through the graphical user interface (GUI) controls. SeqPlots accepts all major genomic file formats as input and can also generate and plot user defined motif densities. Profile plots and heatmaps are highly configurable and batch operations can be used to generate a large number of plots at once. SeqPlots is available as a GUI application for Mac or Windows and Linux, or as an R/Bioconductor package.  It can also be deployed on a server for remote and collaborative usage. The analysis features and ease of use of SeqPlots encourages wide data exploration, which should aid the discovery of novel genomic associations.

Availability
------------
SeqPlots is distributed as user-friendly stand-alone [applications for Mac and Windows or Linux](#installation---app-for-mac-win-and-linux). SeqPlots is also available as an [R/Bioconductor package](#installation---rbioconductor) or can be [deployed as a server application](#installation---server-deployment), which is useful for data sharing within laboratories, collaborative usage and remote work. The source code and open development tools, including wiki, bug tracker, and pull requests, are available via GitHub at https://github.com/Przemol/seqplots.


Key features
------------
- Easy to use web interface (R or shell expertise not required)
- Web server or desktop versions
-	Generates publication quality plots out of the box
-	Plots average signals or heatmaps
-	Accepts Wiggle, BedGraph, BigWiggle, BAM and GFF, BED GTF formats
-	Calculates motif density from reference genomes
-	Tracks and features are searchable and previous calculations can be saved
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
