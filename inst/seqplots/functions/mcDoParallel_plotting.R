mcDoParallel <- quote({
  
  if( (input$reactive) ) {
    #common
    is.null(list(
      input$plot_this, input$xmin1, input$xmin2, input$xauto, input$title, input$xlabel, input$ylabel, input$scale_signal, input$legend,
      input$legend_font_size, input$axis_font_size, input$labels_font_size, input$title_font_size, input$lnv, input$lnh, values$priors, values$lables, 
      reactiveValuesToList(subplotSetup), input$cust_col, input$subplot_options
    ))
    
    if (!input$img_heatmap) { 
      is.null(list(
        input$yauto, input$ymin1, input$ymin2, input$ee,  input$lnh_pos,
        legend_pos=input$legend_pos, legend_ext_pos=input$legend_ext_pos, legend_ext=input$legend_ext
      ))
    } else {
      is.null(list(
        input$heatmapzauto, input$zmin1, input$zmin2, values$override_max, values$override_max, input$img_clusters, input$img_sort,
        input$lnv, indi=input$indi, input$hsccoef, input$img_clstmethod, input$heat_colorspace, input$heat_csp_min, input$heat_csp_mid, input$heat_csp_max,
        input$heat_include, input$heat_min_max
      ))
    }
  } else {
    if(input$replot==0) return()
  }
  if( is.null(isolate(input$plot_this)) ) return()
  
  do <- quote({ 
      session$sendCustomMessage("jsExec", "$('#progressModal').modal('show').find('#summary2').text('Plotting...').parent().find('#summary3').text('')")
      out <- list()
      
      dir.create(file.path(Sys.getenv('root'), 'tmp', values$sessionID), showWarnings = FALSE)
      
      a <- file.path('tmp', values$sessionID, paste('Plot_', chartr(' :', '_-', Sys.time()), '.pdf', sep=''))
      # Generate the PNG
      #pdf(a, height = 210, width = 297, units='mm', res=100, pointsize = 12)
      pdf(
          file.path(Sys.getenv('root'), a), width = input$pdf_x_size, 
          height = input$pdf_y_size, onefile = TRUE, paper=input$paper
      )
      co <- lapply(input$plot_this, function(x) fromJSON(x))
      pl <- lapply(co, function(x) values$grfile[[x[2]]][[x[1]]] )
      
      if(input$recordHistory) { dev.control(displaylist="enable") }
      
      if ( !input$img_heatmap ) {
            ans <- plotLineplotLocal(pl=pl)
      } else {
            ans <- plotHeatmapLocal(pl=pl)
      }
      
      if(input$recordHistory) { out$plot <- recordPlot(); dev.control(displaylist="inhibit");  }
      
      dev.off()
      out$url <- a
      out$seed <- attr(ans, 'seed')
      
      class(out) <- 'ans'; out 
  })
  
  
  if( .Platform$OS.type == 'windows' | isolate(input$setup_multithread) == FALSE) {
    isolate({
      session$sendCustomMessage("jsExec", "$('#progressModal').modal('show').find('#summary3').text('Single process plotting.').parent().find('button').prop('disabled', true);")
      out <- try( eval( do ) )
      session$sendCustomMessage("jsExec", "$('#progressModal').modal('hide').find('#summary2').text('').parent().find('button').prop('disabled', false);")
    })
    if (class(out) == "try-error") {
      session$sendCustomMessage( "jsAlert", paste('ERROR:', attr(out, 'condition')$message) ) 
    } else {
      values$im <-  as.character(out$url)
      values$seed <- out$seed
    }
  } else {
    
    mceval(do, 
      quote({ 
        session$sendCustomMessage("jsDots", ".") 
      }),
      quote({ 
        values$im <- as.character(res$url)
        values$seed <- res$seed
        values$plotid  <- isolate( if( is.numeric(values$plotid) ) values$plotid + 1 else 1 )
        if( !is.null(res$plot) ) isolate({ values$plotHistory[[length(values$plotHistory)+1]] <- res$plot })
      })
    )
    
#     if(is.null(isolate(values$proc))) {
#       values$proc <- parallel::mcparallel(do)
#       invalidateLater(100, session)
#       
#     } else if ( parallel:::selectChildren(isolate(values$proc)) == parallel:::processID(isolate(values$proc)) ) {
#       res <- parallel::mccollect(isolate(values$proc), wait=FALSE)[[1]]
#       
#       if( class(res) == 'character' ) {
#         invalidateLater(100, session)
#         values[[ res[1] ]] <- res[2] 
#         stop(res)
#         
#       } else {
#         if(class(res) == 'try-error' ) {
#           parallel::mccollect( isolate(values$proc) ); values$proc <- NULL 
#           session$sendCustomMessage("jsAlert", res); session$sendCustomMessage("jsExec", "$('#progressModal').modal('hide');")
#         } else if ( is.null(res) ) {
#           parallel::mccollect( isolate(values$proc) ); values$proc <- NULL 
#           session$sendCustomMessage("jsExec", "$('#progressModal').modal('hide'); alert('Job canceled.');")
#         } else {
#           parallel::mccollect( isolate(values$proc) )
#           values$proc <- NULL 
#           session$sendCustomMessage("jsExec", "$('#progressModal').modal('hide').find('#summary2').text('')")
#           
#           values$im <- as.character(res$url)
#           if( !is.null(res$plot) ) isolate({ values$plotHistory[[length(values$plotHistory)+1]] <- res$plot })
#           #values$plotHistory <- res$plot
#         }
#       }
#     } else { session$sendCustomMessage("jsExec", "$('#summary3').text( $('#summary3').text().length < 50 ? $('#summary3').text()+'.' : '.' )"); invalidateLater(100, session); }
#     
  }
  
  
  
  
  
})





