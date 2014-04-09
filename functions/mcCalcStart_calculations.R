mcCalcStart <- quote({
  
  if( is.null(input$TR_calculate) )  return()
  
  values$calcID <- input$TR_calculate
  updateSelectInput(session, 'publicRdata', 'Load public file', c( ' ', dir('publicFiles')), ' ')
  values$grfile <- NULL
  
  do <- quote({
    session$sendCustomMessage("jsExec", "$('#progressModal').modal('show').find('#summary2').text('Initializing...').parent().find('#summary3').text('')")
    cat3 <- function(x) { session$sendCustomMessage("jsExec", sprintf("$('#summary2').text('%s')", x)) }
    cat4 <- function(x) { session$sendCustomMessage("jsExec", sprintf("$('#summary3').text('%s')", x)) }
    if ( length( values$SFsetup ) > 0 | length( input$f_tracks ) > 0 ) {
      procQuick(c(input$f_tracks, values$SFsetup), input$f_features,
                x1 = input$plot_upstream, xm = input$anchored_downstream, x2 = input$plot_downstream,
                type = input$plot_type, bin= as.numeric(input$BWbin),
                cat3=cat3, cat4=cat4, rm0=input$rm0, ignore_strand=input$ignore_strand, add_heatmap=input$add_heatmap, con=con)
      
    }  else ( stop('Nothing to calculate!') )
  })
  
  if( .Platform$OS.type == 'windows' | isolate(input$setup_multithread) == FALSE) {
    
    isolate({
      session$sendCustomMessage("jsExec", "$('#progressModal').modal('show').find('#summary2').text('Calculating plot set single process...').parent().find('button').prop('disabled', true);") 
      values$grfile <- eval( do )
      session$sendCustomMessage("jsExec", "$('#progressModal').modal('hide').find('#summary2').text('').parent().find('button').prop('disabled', false);")
    })
    
  } else {
    
    mceval(do, NULL,
           quote({ 
             values$grfile <- res
             session$sendCustomMessage("jsAlert", "Job done!")
             values$plotMsg <- div(style='margin-top:10px;', id=as.character(input$TR_calculate), class="alert alert-success", 
                                   HTML('<button type="button" class="close" data-dismiss="alert">x</button><strong>Calculation complete!</strong> You can plot or save the results in public files.')
             ) 
           }),
    )
    
#     if (is.null(isolate(values$proc))) {
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
#     
#         }
#       }
#       
#     } else { invalidateLater(100, session); }
    
  }
})










