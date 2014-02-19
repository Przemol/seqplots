mcDoParallel <- quote({
  
  if( (input$reactive) ) {
    #common
    is.null(list(
      input$plot_this, input$xmin1, input$xmin2, input$xauto, input$title, input$xlabel, input$ylabel, input$scale_signal, input$legend,
      input$legend_font_size, input$axis_font_size, input$labels_font_size, input$title_font_size, input$lnv, input$lnh, values$priors, values$lables, reactiveValuesToList(subplotSetup)
    ))
    
    if (!input$img_heatmap) { 
      #colors
      is.null(list(
        input$yauto, input$ymin1, input$ymin2, input$cust_col, input$ee,  input$lnh_pos,
        legend_pos=input$legend_pos, legend_ext_pos=input$legend_ext_pos, legend_ext=input$legend_ext
      ))
    } else {
      is.null(list(
        input$heatmapzauto, input$zmin1, input$zmin2, values$override_max, values$override_max, input$img_clusters, input$img_sort,
        input$lnv, indi=input$indi, s=input$hsccoef
      ))
    }
  } else {
    if(input$replot==0) return()
  }
  if( is.null(isolate(input$plot_this)) ) return()
  
  
  if(is.null(isolate(values$proc))) {
    n<<-0
    session$sendCustomMessage("jsExec", "$('#progressModal').modal('show');")
    
    values$proc <- parallel::mcparallel({
      out <- list()
      a <- tempfile(pattern = "sessionID_", tmpdir = 'tmp', fileext = '.png')
      # Generate the PNG
      png(a, width=1240, height=720)
      
      co <- lapply(input$plot_this, function(x) fromJSON(x))
      pl <- lapply(co, function(x) values$grfile[[x[2]]][[x[1]]] )
      
      if(input$recordHistory) { dev.control(displaylist="enable") }
      
      if ( !input$img_heatmap ) {
        plotLineplot(pl=pl)
      } else {
        plotHeatmap(pl=pl)
      }
      
      if(input$recordHistory) { out$plot <- recordPlot(); dev.control(displaylist="inhibit");  }
      
      dev.off()
      out$url <- a
      
      class(out) <- 'ans'; out 
      
    })
    values$calcMsg1 <- 'Plotting'; values$calcMsg2 <- '.'
    
    invalidateLater(100, session)
  } else if ( parallel:::selectChildren(isolate(values$proc)) == parallel:::processID(isolate(values$proc)) ) {
    res <- parallel::mccollect(isolate(values$proc), wait=FALSE)[[1]]
    if( class(res) == 'character' ) {
      invalidateLater(100, session)
      values[[ res[1] ]] <- res[2]  
    } else {
      if(class(res) == 'try-error' ) {
        parallel::mccollect( isolate(values$proc) ); values$proc <- NULL 
        session$sendCustomMessage("jsAlert", res); session$sendCustomMessage("jsExec", "$('#progressModal').modal('hide');")
      } else if ( is.null(res) ) {
        parallel::mccollect( isolate(values$proc) ); values$proc <- NULL 
        session$sendCustomMessage("jsExec", "$('#progressModal').modal('hide'); alert('Job canceled.');")
      } else {
        parallel::mccollect( isolate(values$proc) )
        values$proc <- NULL 
        session$sendCustomMessage("jsExec", "$('#progressModal').modal('hide');")
        values$im <- as.character(res$url)
        if( !is.null(res$plot) ) isolate({ values$plotHistory[[length(values$plotHistory)+1]] <- res$plot })
        #values$plotHistory <- res$plot
      }
    }
  } else {   n<<-n+1; if(!n%%10) values$calcMsg2 <- paste0(isolate(values$calcMsg2), '.'); invalidateLater(100, session); }
})