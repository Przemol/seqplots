# TODO: Add comment
# 
# Author: przemol
###############################################################################

mcCalcStart <- quote({
	
	if( is.null(input$TR_calculate) )  return()
	calcID <<- input$TR_calculate

	
	if (is.null(proc)) {
		proc <<- multicore::parallel(
				procTSSsimple(input$f_tracks, input$f_features,
						XlimMin = input$plot_upstream, XlimMid = input$anchored_downstream, XlimMax = input$plot_downstream,
						type = input$plot_type,
						cat3=cat3, cat4=cat4, output=output, rm0=input$rm0, ignore_strand=input$ignore_strand)		
		)
		values$calcMsg1 <- 'Started'
		invalidateLater(1000)
	} else if ( multicore::selectChildren(proc) == multicore::processID(proc) ) {
		res <- unlist(multicore::collect(proc, wait=FALSE))
		
		if(class(res) == 'character') {
			invalidateLater(1000)
			values$calcMsg1 <- res	
		} else {
			values$grfile <<- res
			multicore::collect(proc)
			proc <<- NULL
			values$calcMsg1 <-  paste('FINISHED') 
		}
		
	} else { invalidateLater(1000); }
})


output$plot_message <- reactiveUI(function() { 	
			if( !is.null(input$TR_calculate) ) {
				
				calcID <<- input$TR_calculate
				
				if((length(input$f_tracks) < 1) | (length(input$f_features) < 1)) {
					plot_message <<- div(style='margin-top:10px;', id=as.character(input$TR_calculate), class="alert alert-error in fade", HTML('<button type="button" class="close" data-dismiss="alert">x</button><strong>Error!</strong> Select at least one track and one feature file.')) 
					return( tags$script(ID=input$TR_calculate, "$('#progressModal').modal('hide')") )
				}	
				
				try_res <- try({
							grfile <<- procTSSsimple(input$f_tracks, input$f_features,
									XlimMin = input$plot_upstream, XlimMid = input$anchored_downstream, XlimMax = input$plot_downstream,
									type = input$plot_type,
									cat3=cat3, cat4=cat4, output=output, rm0=input$rm0, ignore_strand=input$ignore_strand)
							
						})
				if(is(try_res, 'try-error')) {
					plot_message <<- div(style='margin-top:10px;', id=as.character(input$TR_calculate), class="alert alert-error in fade", HTML(paste('<button type="button" class="close" data-dismiss="alert">x</button><strong>Error!</strong> ', as.character(try_res) ))) 
					return( tags$script(ID=input$TR_calculate, "$('#progressModal').modal('hide')") )
				} else {
					plot_message <<- div(style='margin-top:10px;', id=as.character(calcID), class="alert alert-success", HTML('<button type="button" class="close" data-dismiss="alert">x</button><strong>Calculation complete!</strong> You can plot or save the results in public files.')) 
					return(	tags$script(ID=calcID, "$('#progressModal').modal('hide')") )
				}
				
				
			} else if( !is.null(calcID) ) {
				plot_message
			}
			
		})






obs <- observe( {
			if (input$parast==0) return()
			
			f <- function(x) {
				for(i in 1:10) {
					multicore::sendMaster(as.character(i))
					Sys.sleep(2)
				}
				sqrt(x)
			}
			
			if (is.null(proc)) {
		
				proc <<- multicore::parallel(f( 10 )) 
				values$calcMsg1 <- 'Started'
				invalidateLater(1000)
			} else if ( multicore::selectChildren(proc) == multicore::processID(proc) ) {
				res <- unlist(multicore::collect(proc, wait=FALSE))
				
				if(class(res) == 'character') {
					invalidateLater(1000)
					values$calcMsg1 <- res	
				} else {
					multicore::collect(proc)
					proc <<- NULL
					values$calcMsg1 <-  paste('FINISHED:',res) 
				}
				
			} else { invalidateLater(1000); }
		}, label='CalcMain', suspended = FALSE )