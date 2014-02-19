#Heatmap plotting function
plotHeatmap <- function(pl, title=input$title) {
  if( length(pl) > 10 ) stop('Heatmap plotting: Select less than 10 checkboxes!')
  if( is.null(pl[[1]]$heatmap) ) stop('Heatmap plotting: No heatmap data avilabe! Re-run with "Calculate Heatmap" option selected.')
  Hclc <- do.call(cbind, lapply(pl[values$include], '[[', 'heatmap')) 
  
  pl <- pl[order(values$priors, decreasing=TRUE)]
  H <- do.call(cbind, lapply(pl, '[[', 'heatmap'))
  
  if ( input$scale_signal == "Do not transform" ) {
    #Do Nothing
  } else if ( input$scale_signal ==  "Log2 transform" ) {
    H <- log2(H)
  } else if ( input$scale_signal == "Z-score transform" ) {
    H <- scale(H)
  }
  #H[is.na(H)] <- 0
  if(input$img_sort) { H <- H[order(rowMeans(Hclc, na.rm=TRUE)),] }
  if(input$img_clusters > 1) {
    Hcl <- Hclc; Hcl[is.na(Hcl)] <- 0
    k<-kmeans(Hcl, input$img_clusters) #OPTIONAL: Change this line for differnt number of clusters
    kcenter_sum <- apply(k$centers,1,sum)
    orderkcenter <- order(kcenter_sum)
    orderindex <- order(orderkcenter)
    k1_new <- orderindex[k$cluster]# new class id sorted by center.
    orderk<-order(k1_new)
    k$size <- k$size[orderkcenter]
    H <- H[orderk,]
    #session$sendInputMessage('clusters', list(value=orderk))
    session$sendCustomMessage("jsExec", paste0("$('#clusters').val('",toJSON(k1_new),"').change()"))
    clusts <- k$size
  } else {
    values$clusters <- NULL
    clusts <- NULL
  }
  lab <- sapply(pl, '[[', 'desc')
  new_lab <- values$lables[ order(values$priors, decreasing=TRUE) ]
  lab[new_lab!=''] <- new_lab[new_lab!='']
  #lab <- lab[order(values$priors, decreasing=TRUE)]
  
  o_min <- as.numeric( values$override_min[order(values$priors, decreasing=TRUE)] )
  o_max <- as.numeric( values$override_max[order(values$priors, decreasing=TRUE)] )
  
  runGalaxy( H, clusts, wigcount=length(pl), 
             bins=pl[[1]]$all_ind, 
             titles=lab, e=pl[[1]]$e, 
             xlim=if(!input$xauto) NULL else c(input$xmin1, input$xmin2), 
             ylabel=input$ylabel,
             lfs=input$labels_font_size, 
             afs=input$axis_font_size, 
             xlabel=input$xlabel, 
             Leg = input$legend, 
             lgfs=input$legend_font_size,
             autoscale=!input$heatmapzauto, 
             zmin=input$zmin1, 
             zmax=input$zmin2, 
             ln.v=input$lnv, 
             indi=input$indi, 
             s=input$hsccoef,
             o_min=o_min, 
             o_max=o_max)
  par(cex=input$title_font_size)
  title(input$title, outer = TRUE)
}
