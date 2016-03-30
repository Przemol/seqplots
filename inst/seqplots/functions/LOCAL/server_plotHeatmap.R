#Heatmap plotting function
plotHeatmapLocal <- function(pl, title=input$title, legend=TRUE) {
  
    if( length(pl) > 10 ) 
        stop('Heatmap plotting: Select less than 10 checkboxes!', call.=FALSE)
    if( is.null(pl[[1]]$heatmap) ) 
        stop('Heatmap plotting: No heatmap data avilabe! Re-run with "Calculate Heatmap" option selected.', call.=FALSE)
    if(length(unique(sapply(pl, function(x) nrow(x[['heatmap']])))) != 1) 
        stop('Heatmap plotting: All plots must have equal number of features. Do not plot heatmaps on multiple GFF/BED.', call.=FALSE)
    
    if(!exists(".Random.seed", where = globalenv())) runif(1)
    if(input$heat_seed) {
        assign(".Random.seed", values$seed, pos = globalenv())
    }
    
    
    seed <- .Random.seed
    
    
    ord <- if( length(subplotSetup$prior) & ('prior' %in% input$subplot_options) ) order(subplotSetup$prior, decreasing=TRUE) else 1:length(pl)
    pl <- pl[ ord ]
    
    if ( input$scale_signal == "Do not transform" ) {
        plotScale <-  'linear'
    } else if ( input$scale_signal ==  "Log2 transform" ) {
        plotScale <-  'log2'
    } else if ( input$scale_signal == "Z-score transform" ) {
        plotScale <-  'zscore'
    }
    
    lab <- sapply(pl, '[[', 'desc')
    if( length(subplotSetup$label) & ('label' %in% input$subplot_options) ) {
        new_lab <- subplotSetup$label[ord]
        lab[new_lab!=''] <- new_lab[new_lab!='']
    }
    
    o_min <- if( length(subplotSetup$min) & input$heat_min_max ) as.numeric( subplotSetup$min[ord] ) else rep(NA, length(pl))
    o_max <- if( length(subplotSetup$max) & input$heat_min_max ) as.numeric( subplotSetup$max[ord] ) else rep(NA, length(pl))
    
    if(input$heat_subclust == "All clusters" | !input$heat_seed) {
        ylim <- c(nrow(pl[[1]]$heatmap), 1)
    } else{
        n <- as.numeric(input$heat_subclust)
        ylim <- rev(range(values$clusters==n))
    }
    
    if( !nchar(input$heat_colorspace)) {
        colorspace <- c('#011279', '#ffffff', '#ab1500')
    } else if(input$heat_colorspace == 'Custom') {
        colorspace <- c(input$heat_csp_min, input$heat_csp_mid, input$heat_csp_max)
    } else if(input$heat_colorspace %in% rownames(brewer.pal.info)) {
        colorspace <- brewer.pal(brewer.pal.info[input$heat_colorspace,1], input$heat_colorspace)
    } else if(input$heat_colorspace == 'jet') {
        colorspace <- c("#00007F", "blue", "#007FFF", "cyan", "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000") 
    } else if( grepl('\\.', input$heat_colorspace) ) {
        colorspace <- get(input$heat_colorspace)(11)
    } else {
        colorspace <- NULL
    }
    
    if( input$heat_colorspace_rev ) colorspace <- rev(colorspace)
    
    out <- seqplots::plotHeatmap(
        pl, 
        main = title, 
        legend = legend, 
        keepratio = input$pty,
        plotScale = plotScale,
        include = if( length(subplotSetup$inc) & input$heat_include ) 
            as.logical(subplotSetup$inc)[ ord ] else NULL,
        sortrows = input$img_sort,
        clusters = input$img_clusters, 
        clstmethod = input$img_clstmethod, 
        ssomt1 = input$img_ssomt1, 
        ssomt2 = input$img_ssomt2,
        labels = if(legend) lab else NULL, 
        o_min = o_min, 
        o_max = o_max, 
        xlim = if(!input$xauto) NULL else c(input$xmin1, input$xmin2), 
        ylab = input$ylabel,
        cex.lab = input$labels_font_size,
        cex.axis = input$axis_font_size,
        cex.main = input$title_font_size,
        cex.legend = input$legend_font_size,
        xlab = input$xlabel,
        autoscale = !input$heatmapzauto, 
        zmin = input$zmin1, 
        zmax = input$zmin2, 
        ln.v = input$lnv, 
        indi = input$indi, 
        s = input$hsccoef,
        colvec=if("color" %in% input$subplot_options) subplotSetup$color[ord] else NULL,
        clspace=colorspace,
        raster=input$raster,
        ggplot=input$ggplot,
        ylim=ylim
    ) 
    
    session$sendCustomMessage("jsExec", paste0(
        "$('#sortingord').val('", toJSON(out$SortingOrder), "').change()"
    ))
    session$sendCustomMessage("jsExec", paste0(
        "$('#clusters').val('",   toJSON(out$ClusterID   ), "').change()"
    ))
    session$sendCustomMessage("jsExec", paste0(
        "$('#finalord').val('",   toJSON(out$FinalOrder  ), "').change()"
     ))
    session$sendCustomMessage("jsExec", paste0(
        "$('#rowmeans').val('",   toJSON(out$RowMeans    ), "').change()"
    ))
    
    attr(out, "seed") <- seed
    return(out)
}
