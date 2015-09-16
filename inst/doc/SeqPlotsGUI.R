## ----eval=FALSE----------------------------------------------------------
#  List of 2
#   $ HTZ1_Differential_genes_TOP100_v2.gff:List of 2
#    ..$ HTZ1_JA00001_IL1andIL2_F_N2_L3_NORM_linear_1bp_IL010andIL009_averaged.bw    :List of 7
#    .. ..$ means   : num [1:501] 2.52 2.52 2.52 2.53 2.54 ...
#    .. ..$ stderror: num [1:501] 0.114 0.112 0.111 0.11 0.109 ...
#    .. ..$ conint  : num [1:501] 0.226 0.223 0.221 0.218 0.217 ...
#    .. ..$ all_ind : num [1:501] -1000 -995 -990 -985 -980 -975 -970 -965 -960 -955 ...
#    .. ..$ e       : NULL
#    .. ..$ desc    : chr "HTZ1_JA00001_IL1andIL2...\n@HTZ1_Differential_genes_TOP100_v2"
#    .. ..$ heatmap : num [1:100, 1:501] 2.36 5.25 2.2 3.48 4.32 ...
#    ..$ HTZ1_JA00001_IL3andIIL5_F_lin35_L3_NORM_linear_1bp_IL008andIL011_averaged.bw:List of 7
#    .. ..$ means   : num [1:501] 2.36 2.35 2.35 2.36 2.38 ...
#    .. ..$ stderror: num [1:501] 0.126 0.125 0.125 0.126 0.125 ...
#    .. ..$ conint  : num [1:501] 0.249 0.249 0.247 0.251 0.249 ...
#    .. ..$ all_ind : num [1:501] -1000 -995 -990 -985 -980 -975 -970 -965 -960 -955 ...
#    .. ..$ e       : NULL
#    .. ..$ desc    : chr "HTZ1_JA00001_IL3andIIL5...\n@HTZ1_Differential_genes_TOP100_v2"
#    .. ..$ heatmap : num [1:100, 1:501] 2.61 3.17 1.42 2.46 4.26 ...
#   $ HTZ1_Differential_genes_BOTTOM100.gff:List of 2
#    ..$ HTZ1_JA00001_IL1andIL2_F_N2_L3_NORM_linear_1bp_IL010andIL009_averaged.bw    :List of 7
#    .. ..$ means   : num [1:501] 1.57 1.57 1.58 1.6 1.62 ...
#    .. ..$ stderror: num [1:501] 0.0996 0.0985 0.1003 0.1022 0.1018 ...
#    .. ..$ conint  : num [1:501] 0.198 0.195 0.199 0.203 0.202 ...
#    .. ..$ all_ind : num [1:501] -1000 -995 -990 -985 -980 -975 -970 -965 -960 -955 ...
#    .. ..$ e       : NULL
#    .. ..$ desc    : chr "HTZ1_JA00001_IL1andIL2...n@HTZ1_Differential_genes_BOTTOM100"
#    .. ..$ heatmap : num [1:100, 1:501] 1.64 1.37 1.61 1.77 1.86 ...
#    ..$ HTZ1_JA00001_IL3andIIL5_F_lin35_L3_NORM_linear_1bp_IL008andIL011_averaged.bw:List of 7
#    .. ..$ means   : num [1:501] 1.94 1.94 1.95 1.96 1.97 ...
#    .. ..$ stderror: num [1:501] 0.123 0.123 0.124 0.126 0.128 ...
#    .. ..$ conint  : num [1:501] 0.244 0.245 0.246 0.251 0.253 ...
#    .. ..$ all_ind : num [1:501] -1000 -995 -990 -985 -980 -975 -970 -965 -960 -955 ...
#    .. ..$ e       : NULL
#    .. ..$ desc    : chr "HTZ1_JA00001_IL3andIIL5...\n@HTZ1_Differential_genes_BOTTOM100"
#    .. ..$ heatmap : num [1:100, 1:501] 1.61 1.37 1.29 3.04 3.77 ...
#  

## ----echo=FALSE, results='asis', echo=FALSE------------------------------
# knit the first three lines of first.Rmd
files <- c(
  'Files.md',
  'PlotSets.md',
  'Plotting.md',
  'Heatmaps.md',
  'Outputs.md',
  'Batch.md',
  'SaveAndLoad.md',
  'AdvOup.md',
  'Genomes.md',
  'Errors.md',
  'Terms.md',
  'References.md'
)

cat(knitr::knit_child(
    text = gsub(
        "img/",
        "../inst/seqplots/www/help/img/",
        unlist(lapply(file.path('../inst/seqplots/www/help', files), readLines))
    ),
    quiet = TRUE
), sep = '\n')

## ----echo=FALSE----------------------------------------------------------
sessionInfo()

