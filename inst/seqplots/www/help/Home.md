![Examples of Seq Plots interface and outputs](wiki/img/ico_full_color.png)
> Interactive software for exploratory data analyses, pattern discovery and visualization in genomics.

![Examples of Seq Plots interface and outputs](wiki/img/SeqPlots_fig1_web.png)

Summary
-------
SeqPlots is a user-friendly exploratory data analysis (EDA) and visualization software for genomics. After choosing groups of signal and feature files and defining plotting parameters, users can generate profile plots of average signal or heat maps clustered using different algorithms in a matter of seconds through the graphical user interface (GUI) controls. SeqPlots accepts all major genomic file formats as input and can also generate and plot user defined motif densities. Profile plots and heatmaps are highly configurable and batch operations can be used to generate a large number of plots at once. SeqPlots is available as a GUI application for Mac or Windows and Linux, or as an R/Bioconductor package.  It can also be deployed on a server for remote and collaborative usage. The analysis features and ease of use of SeqPlots encourages wide data exploration, which should aid the discovery of novel genomic associations.

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

Issues and bugs
---------------
Please visit [**issue tracker**](https://github.com/Przemol/seqplots/issues) to view currently know issues. To report a new issue/bug/feature request please click [**here**](https://github.com/Przemol/seqplots/issues/new). If the issue is connected to file upload please attach the file in the form.
