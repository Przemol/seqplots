# dd <- get(load('tests/0001_heatTest1_2x2.Rdata'))
# z=PlotSetArray(data=dd)
# PlotSetArray(data=dd)$getPairs(list(c( 'HTZ1_Differential_genes_TOP100_v2', 'HTZ1_JA00001_IL1andIL2_F_N2_L3_NORM_linear_1bp_IL010andIL009_averaged' )))





# setMethod("[", c("PlotSetArray", "ANY", "missing", "ANY"), function(x, i, j, ..., drop=TRUE) x$get(i, 1:worm$ntracks()) )
# setMethod("[", c("PlotSetArray", "vector"), function(x, i) x$getByID(i) )





# co <- lapply(input$plot_this, function(x) fromJSON(x))
# pl <- lapply(co, function(x) values$grfile[[x[2]]][[x[1]]] )
# 
# z=PlotSetArray(data=dd)
# y=PlotSetArray()
# 
# #S4
# PlotSetArrayS4 <- setClass("PlotSetArrayS4", slots = list( data = "list", annotations = "list") )
# getGeneric("[")
# 
# setMethod("[", c("PlotSetArrayS4", "integer", "missing", "ANY"), function(x, i, j, ..., drop=TRUE) x$getRow(i) )
# 
# setMethod("[", c("PlotSetArrayS4", "integer", "integer", "ANY"), function(x, i, j, ..., drop=TRUE) x$getRow(i) )
# 
# 
# 
# y=PlotSetArrayS4(data=list(letters))
# 
# hmap <- function (x, ...) 
#   UseMethod("heatmap")z
# 
# 
# track <- setClass("track",
#                   slots = c(x="numeric", y="numeric"))

# getPlotSetArray(features ='/Users/przemol/SeqPlots_data/files/HTZ1_Differential_genes_TOP100_v2.gff', tracks ='/Users/przemol/SeqPlots_data/files/worm_pileup_unique.bw',refgenome='ce10')

# z<- getPlotSetArray(features ='tests/PooledCapTSS_ce.gff', tracks ='tests/cpg_200bp_ce10.bw',refgenome='ce10')
