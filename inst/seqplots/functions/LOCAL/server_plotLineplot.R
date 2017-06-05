#Lineplot plotting function
plotLineplotLocal <- function(pl, title=input$title, type='dev', legend=TRUE, batchcolor=NULL) {
    
    ord <- if( length(subplotSetup$prior) & ('prior' %in% input$subplot_options) ) order(subplotSetup$prior, decreasing=TRUE) else 1:length(pl)
    pl <- pl[ ord ]
    
    if( length(subplotSetup$label) & ('label' %in% input$subplot_options) ) {
        lab <- subplotSetup$label[ ord ]
    } else ( 
        lab <- NULL 
    )

    if ( input$scale_signal == "Do not transform" ) {
        plotScale <-  'linear'
    } else if ( input$scale_signal ==  "Log2 transform" ) {
        plotScale <-  'log2'
    } else if ( input$scale_signal == "Z-score transform" ) {
        plotScale <-  'zscore'
    }

    seqplots::plotAverage(
        pl, 
        xlim = c(if(!input$xauto) NULL else input$xmin1, if(!input$xauto) NULL else input$xmin2),
        ylim = c(if(!input$yauto) NULL else input$ymin1, if(!input$yauto) NULL else input$ymin2), 
        keepratio = input$pty, 
        labels = lab, 
        main = title, 
        type = type, 
        xlab = input$xlabel, 
        ylab = input$ylabel, 
        plotScale = plotScale,
        error.estimates = input$ee, 
        legend = input$legend & legend, 
        legend_ext = input$legend_ext, 
        legend_pos = input$legend_pos, 
        legend_ext_pos = input$legend_ext_pos, 
        cex.axis = input$axis_font_size, 
        cex.lab = input$labels_font_size, 
        cex.main = input$title_font_size, 
        cex.legend = input$legend_font_size, 
        ln.v = input$lnv,
        ln.h = if(input$lnh) input$lnh_pos else NULL, 
        colvec = if("color" %in% input$subplot_options) { if(!is.null(batchcolor)) batchcolor else subplotSetup$color[ord] } else NULL, 
        pointsize = 12
    ) 
  

  

  
#   plotMext(pl, 
#            x1=if(!input$xauto) NULL else input$xmin1, 
#            x2=if(!input$xauto) NULL else input$xmin2, 
#            y1=if(!input$yauto) NULL else input$ymin1, 
#            y2=if(!input$yauto) NULL else input$ymin2,
#            title=title, Xtitle = input$xlabel, Ytitle = input$ylabel, 
#            colvec = if("color" %in% input$subplot_options) subplotSetup$color[ord] else NULL, 
#            plotScale = plotScale, EE = input$ee, Leg = input$legend & legend,
#            cex.axis = input$axis_font_size, cex.lab = input$labels_font_size, cex.main = input$title_font_size, cex.legend = input$legend_font_size, 
#            ln.v=input$lnv, ln.h=if(input$lnh) input$lnh_pos else NULL, 
#            legend_pos=input$legend_pos, legend_ext_pos=input$legend_ext_pos, legend_ext=input$legend_ext, type=type)  
}
