Explanations
============

------------------------------------------------------

* "**feature**" - a genomic interval defined by **chromosome** name, **start** and **end** positions and the **directionality** (strand). The end must always be a bigger number than start, so the width of the range is not negative. Start and end means here the numeric start of the interval and should not be confused with TSS and TTS.

    For example, in BED format this information is stored in following text tab delimited format: `chr7    127471196  127472363  .  .  +`

    ------------------------------------------------------

* "**directionality**" - the strand of genomic feature, determining if the plotting range should be anchored around the star or and, and the direction in which signal is being processed to create the average track or heatmap. Unknown directionality is marked by `*` and treated as `+` for calculations.

    ------------------------------------------------------

*  "**track**" - the file assigning the continuous signal (score) to genomic locations across the chromosomes. The signal usually comes from sequencing experiments, like ChIP-seq, RNA-seq, DNase-seq, MNase-seq, or from computational tools, for example nucleosome occupancy prediction, CpG density.

    For example, in BedGraph format this information is stored in following text tab delimited format: `chr19 49302300 49302600 -0.75`

    ------------------------------------------------------

*  "**reference genome package**" - the R BSgemome package containing the full reference sequence for given species. It is also used to provide universal chromosome names and chromosome lengths taht are used as plotting boundaries.

    ------------------------------------------------------

*  "**reads coverage**" - The basic way to calculate the signal from sequencing based assays. The numeric representation shows how much reads was aligned to given genomic location. This can be a proxy to protein-DNA binding (ChIP-seq) or the expression (RNA-seq). Can be calculated using BedTools: http://bedtools.readthedocs.org/en/latest/content/tools/genomecov.html  Also known as `pileups`.

    ------------------------------------------------------

