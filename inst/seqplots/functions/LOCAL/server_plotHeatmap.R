#Heatmap plotting function
plotHeatmap <- function(pl, title=input$title, legend=TRUE) {
  
  if(input$pty) par(pty='s')
  
  if( length(pl) > 10 ) 
    stop('Heatmap plotting: Select less than 10 checkboxes!', call.=FALSE)
  if( is.null(pl[[1]]$heatmap) ) 
    stop('Heatmap plotting: No heatmap data avilabe! Re-run with "Calculate Heatmap" option selected.', call.=FALSE)
  if(length(unique(sapply(pl, function(x) nrow(x[['heatmap']])))) != 1) 
    stop('Heatmap plotting: All plots must have equal number of features. Do not plot heatmaps on multiple GFF/BED.', call.=FALSE)
  
  #Heatmap data aquizition (as list of matrixes)
  ord <- if( length(subplotSetup$prior) & ('prior' %in% input$subplot_options) ) order(subplotSetup$prior, decreasing=TRUE) else 1:length(pl)
  HLST <- lapply(pl, '[[', 'heatmap')[ ord ]  
  
  #Optional scalling
  if ( input$scale_signal == "Do not transform" ) {
    #Do Nothing
  } else if ( input$scale_signal ==  "Log2 transform" ) {
    HLST <- lapply(HLST, log2 )
  } else if ( input$scale_signal == "Z-score transform" ) {
    HLST <- lapply(HLST, scale )
  }
  
  #Preparing flat matrix fro sorrting an clustering
  if( length(subplotSetup$inc) & input$heat_include ) {
    Hclc <- do.call(cbind, HLST[ as.logical(subplotSetup$inc)[ ord ] ])
  } else {
    Hclc <- do.call(cbind, HLST)
  }
  
  finalOrd <- 1:nrow(Hclc)

  #Sorting
  if(input$img_sort) { 
    sorting_order <- order(rowMeans(Hclc, na.rm=TRUE), decreasing = TRUE) 
    finalOrd <- finalOrd[sorting_order] 
    Hclc <- Hclc[sorting_order,]
    session$sendCustomMessage("jsExec", paste0("$('#sortingord').val('",toJSON(sorting_order),"').change()"))
  } else {
    sorting_order <- 1:nrow(Hclc)
    session$sendCustomMessage("jsExec", "$('#sortingord').val('').change()")
  }
  
  #Clustering
  if(input$img_clusters > 1 & input$img_clstmethod == 'kmeans') {
    
    Hcl <- Hclc; Hcl[is.na(Hcl)] <- 0
    k <- kmeans(Hcl, input$img_clusters)
    cls_order <- order(k$cluster)
    classes <- k$cluster
    
    finalOrd <- finalOrd[cls_order]
    clusts <- k$size
    session$sendCustomMessage("jsExec", paste0("$('#clusters').val('",toJSON(classes[order(sorting_order)]),"').change()"))
    
  } else if(input$img_clstmethod == 'hclust') {
    
    Hcl <- Hclc; Hcl[is.na(Hcl)] <- 0
    cls <- hclust(dist(Hcl))
    cls_order <- cls$order
    classes <- cutree(cls, input$img_clusters)
    
    finalOrd <- finalOrd[cls_order]
    clusts <- table(classes)
    session$sendCustomMessage("jsExec", paste0("$('#clusters').val('",toJSON(classes),"').change()"))
    
  } else if(input$img_clstmethod == 'ssom') {
    
    if( length(subplotSetup$inc) & input$heat_include ) {
      Hlist <- HLST[ as.logical(subplotSetup$inc)[ ord ] ]
    } else {
      Hlist <- HLST
    }
 
    Hlist <- lapply(Hlist, function(x) {x[is.na(x)] <- 0; if(input$img_sort) x <- x[sorting_order,]; x} )
    
    ssom <- supersom(Hlist, grid = somgrid(xdim = input$img_ssomt1, ydim = input$img_ssomt2, "hexagonal"), rlen = 100, toroidal=TRUE)
    classes <- ssom$unit.classif
    cls_order <- order(ssom$unit.classif)
    
    finalOrd <- finalOrd[cls_order]
    clusts <- table(classes)
    session$sendCustomMessage("jsExec", paste0("$('#clusters').val('",toJSON(classes),"').change()"))
    
  } else {
    values$clusters <- NULL
    clusts <- NULL
    session$sendCustomMessage("jsExec", "$('#clusters').val('').change()")
  }
  
  HLST <- lapply(HLST, function(x) { return(x[finalOrd, ]) } )
  session$sendCustomMessage("jsExec", paste0("$('#finalord').val('",toJSON(finalOrd),"').change()"))
  
  lab <- sapply(pl, '[[', 'desc')
  if( length(subplotSetup$label) & ('label' %in% input$subplot_options) ) {
    new_lab <- subplotSetup$label[ord]
    lab[new_lab!=''] <- new_lab[new_lab!='']
  }
  
  o_min <- if( length(subplotSetup$min) & input$heat_min_max ) as.numeric( subplotSetup$min[ord] ) else rep(NA, length(pl))
  o_max <- if( length(subplotSetup$max) & input$heat_min_max ) as.numeric( subplotSetup$max[ord] ) else rep(NA, length(pl))
  
  if( nchar(title) > 0 ) par(oma=c(0,0,(input$title_font_size/12)+1,0) )
  
  heatmapPlotWrapper( HLST, clusts, 
                      bins=pl[[1]]$all_ind, 
                      titles=if(legend) lab else NULL, e=pl[[1]]$e, 
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
                      o_max=o_max,
                      colvec=if("color" %in% input$subplot_options) subplotSetup$color[ord] else NULL,
                      colorspace=if(input$heat_colorspace) c(input$heat_csp_min, input$heat_csp_mid, input$heat_csp_max) else NULL)
  title(title, outer = TRUE, cex.main=input$title_font_size/12)
}
