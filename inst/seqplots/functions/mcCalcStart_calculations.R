mcCalcStart <- quote({
  
  if( is.null(input$TR_calculate) )  return()
  if( is.null( isolate(values$proc)) ) {
    values$calcID <- input$TR_calculate
    updateSelectInput(session, 'publicRdata', 'Load public file', c( '', dir('publicFiles')))
    values$grfile <- NULL
    pb_max <- (length(input$f_tracks)+length(values$SFsetup))*length(input$f_features)
    values$progress <- shiny::Progress$new(session, min=0, max=pb_max)
  }
  do <- quote({
      
    on.exit(values$progress$close())
    values$progress$set(value = 0, message = 'Calculation in progress...', detail = '')  
    session$sendCustomMessage("jsExec", "$('#progressModal').modal('show').find('#summary2').text('Initializing...').parent().find('#summary3').text('')")
    
    last_msg <- Sys.time()
    cat3 <- function(x) { 
        if(Sys.time() - last_msg > 1) {
            session$sendCustomMessage("jsExec", sprintf("$('#summary2').text('%s')", x))
            values$progress$set(value = as.numeric(sub('.+\\[(.+)\\/.+\\]$', '\\1', x )), message = NULL, detail = sub('.+\\[(.+)]$', '\\1', x ))
            last_msg <<- Sys.time()
        }
    }
    
    cat4 <- function(x) { 
        if(Sys.time() - last_msg > 1) {
            session$sendCustomMessage("jsExec", sprintf("$('#summary3').text('%s')", x)) 
            last_msg <<- Sys.time()
        }
    }
    
    if ( length( values$SFsetup ) > 0 | length( input$f_tracks ) > 0 ) {
        
        out <- seqplots::getPlotSetArray(
                tracks=c(file.path('files', sort(input$f_tracks)), values$SFsetup),
                features=file.path('files', sort(input$f_features)),
                refgenome=con,
                xmin = input$plot_upstream, 
                xanchored = input$anchored_downstream, 
                xmax = input$plot_downstream,
                type = gsub('^(.)[a-z]+ (.).+', '\\1\\2', tolower(input$plot_type)), 
                bin= as.numeric(input$BWbin),
                rm0=input$rm0,
                ignore_strand=input$ignore_strand,
                add_heatmap=input$add_heatmap,
                verbose=TRUE, lvl1m=cat3, lvl2m=message
            )       
        out$data 
      #procQuick(c(sort(input$f_tracks), values$SFsetup), sort(input$f_features),
       #         x1 = input$plot_upstream, xm = input$anchored_downstream, x2 = input$plot_downstream,
        #        type = input$plot_type, bin= as.numeric(input$BWbin),
         #       cat3=cat3, cat4=cat4, rm0=input$rm0, ignore_strand=input$ignore_strand, add_heatmap=input$add_heatmap, con=con)
      
    }  else ( stop('Nothing to calculate!') )
  })
  
  if( .Platform$OS.type == 'windows' | isolate(input$setup_multithread) == FALSE) {
    
    isolate({
      session$sendCustomMessage("jsExec", "$('#progressModal').modal('show').find('#summary2').text('Calculating plot set single process...').parent().find('button').prop('disabled', true);") 
      values$grfile <- try( eval( do ) )
      session$sendCustomMessage("jsExec", "$('#progressModal').modal('hide').find('#summary2').text('').parent().find('button').prop('disabled', false);")
    })
    if (class(values$grfile) == "try-error") {
      session$sendCustomMessage( "jsAlert", paste('ERROR:', attr(values$grfile, 'condition')$message) ) 
    }
    
  } else {
    
    mceval(
        do, 
        quote({ 
            session$sendCustomMessage("jsDots", ".") 
        }),
        quote({ 
            values$grfile <- res
            session$sendCustomMessage("jsAlert", "Job done!")
        })
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










