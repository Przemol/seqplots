#Lineplot plotting function
plotLineplot <- function(pl, title=input$title) {
  
  ord <- order(values$priors, decreasing=TRUE)
  pl <- pl[ ord ]
  pl <- Map(function(x, y) {if(nchar(y)) x[['desc']]<-y; return(x)}, pl, values$lables[ ord ])
  
  if (input$cust_col) {
    co <- lapply(input$plot_this, function(x) fromJSON(x))
    cltab <- sapply( co, function(x) eval(substitute(input$b, list(b = paste0('col_',x[1],'x', x[2]) ))) )
  } else {
    cltab <- NULL
  }
  
  if ( input$scale_signal == "Do not transform" ) {
    plotScale <-  'linear'
  } else if ( input$scale_signal ==  "Log2 transform" ) {
    plotScale <-  'log2'
  } else if ( input$scale_signal == "Z-score transform" ) {
    plotScale <-  'zscore'
  }
  
  plotMext(pl, 
           x1=if(!input$xauto) NULL else input$xmin1, 
           x2=if(!input$xauto) NULL else input$xmin2, 
           y1=if(!input$yauto) NULL else input$ymin1, 
           y2=if(!input$yauto) NULL else input$ymin2,
           title=title, Xtitle = input$xlabel, Ytitle = input$ylabel, colvec = cltab, plotScale = plotScale, EE = input$ee, Leg = input$legend,
           cex.axis = input$axis_font_size, cex.lab = input$labels_font_size, cex.main = input$title_font_size, cex.legend = input$legend_font_size, 
           ln.v=input$lnv, ln.h=if(input$lnh) input$lnh_pos else NULL, 
           legend_pos=input$legend_pos, legend_ext_pos=input$legend_ext_pos, legend_ext=input$legend_ext)  
}
