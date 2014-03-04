mcCalcStart <- quote({
  
  if( is.null(input$TR_calculate) )  return()
  
  values$calcID <- input$TR_calculate
  updateSelectInput(session, 'publicRdata', 'Load public file', c( ' ', dir('publicFiles')), ' ')
  values$grfile <- NULL
  
  ok_msg <- div(style='margin-top:10px;', id=as.character(input$TR_calculate), class="alert alert-success", 
                HTML('<button type="button" class="close" data-dismiss="alert">x</button><strong>Calculation complete!</strong> You can plot or save the results in public files.')
  ) 
  
  
  if (is.null(isolate(values$proc))) {
    values$proc <- parallel::mcparallel(
      if ( length( values$SFsetup ) > 0 | length( input$f_tracks ) > 0 ) {
        
        procQuick(c(input$f_tracks, values$SFsetup), input$f_features,
                  x1 = input$plot_upstream, xm = input$anchored_downstream, x2 = input$plot_downstream,
                  type = input$plot_type, bin= as.numeric(input$BWbin),
                  cat3=cat3, cat4=cat4, rm0=input$rm0, ignore_strand=input$ignore_strand, add_heatmap=input$add_heatmap, con=con)
        
      }  else ( stop('Nothing to calculate!') )
      
    )
    values$calcMsg1 <- 'Started NOW'
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
        values$grfile <- res
        values$proc <- NULL 
        values$calcMsg1 <-  paste('FINISHED') 
        session$sendCustomMessage("jsExec", "$('#progressModal').modal('hide'); alert('Done!');")
        values$plotMsg <- ok_msg
      }
    }
    
  } else { invalidateLater(100, session); }
})