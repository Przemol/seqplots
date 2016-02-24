# SeqPlots server file - initial cleaning
# 
# Author: Przemyslaw Stempor
#For local testes: Sys.setenv(root=file.path(path.expand("~"), "SeqPlots_data"), web=getwd()); require(shiny); runApp()
#session$registerDataObj('im', file.path(Sys.getenv("web", '.'), 'www/help/help.html'), function(data, req) { shiny:::httpResponse(content=readChar(data, file.info(data)$size)) })
###############################################################################

#options("xtable.sanitize.text.function" = identity)
options("shiny.maxRequestSize" = -1)
#options("bitmapType" = "cairo")
#options(shiny.reactlog = FALSE)

##Turn off experimental
#require(rCharts)
#options(RCHART_WIDTH = 800)

sourceDir <- function(path, ...) {
  for (nm in list.files(path, pattern = "[.][RrSsQq]$")) {
    source(file.path(path, nm), ...)
  }
}

sqlite <- RSQLite::SQLite()
if( Sys.getenv('root') != '' ) {
  con <- dbConnect(sqlite, dbname = file.path(Sys.getenv('root'),'files.sqlite'))
} else if( file.exists('server_config.R')  ) {
  source('server_config.R')
  con <- dbConnect(sqlite, dbname = file.path(Sys.getenv('root'),'files.sqlite'))
} else {
  con <- dbConnect(sqlite, dbname = 'files.sqlite')
}



shinyServer(function(input, output, clientData, session) {
  
  #Reactive values definition
  subplotSetup <- reactiveValues( )
  urlSetup <- reactiveValues( )
  values <- reactiveValues( 
      grfile=NULL, calcID=NULL, plotMsg=NULL, refFileGrids=NULL, proc=NULL, 
      im=NULL, clusters=NULL, SFsetup=list(), plotHistory=list(),
      sessionID=gsub('[^A-Za-z0-9]', '_', session$request$HTTP_SEC_WEBSOCKET_KEY),
      GENOMES=NULL
  )
  
  updateGenomes <- function() {
      gen <- BSgenome:::installed.genomes(splitNameParts=TRUE)$provider_version
      if( length(gen) ) 
          names(gen) <- gsub('^BSgenome.', '', BSgenome:::installed.genomes())
      return( gen[!duplicated(gen)] )
  }    

  observe({
      values$GENOMES <- updateGenomes()
  })
  
  #Source functions
  if( Sys.getenv('web') != '' ) setwd(Sys.getenv('web'))
  sourceDir('functions')
  source( file.path(Sys.getenv("web", '.'), 'functions/LOCAL/mceval.R'), local=TRUE )
  source( file.path(Sys.getenv("web", '.'), 'functions/LOCAL/server_plotHeatmap.R'), local=TRUE )
  source( file.path(Sys.getenv("web", '.'), 'functions/LOCAL/server_plotLineplot.R'), local=TRUE )
  
  if( Sys.getenv('root') != '' ) setwd(Sys.getenv('root'))
	suppressMessages( addResourcePath(prefix='files', directoryPath='./files') )
	suppressMessages( addResourcePath(prefix='tmp', directoryPath='./tmp') )
  
	#Debug code: Testing eval statement
  if( Sys.getenv("seqplots_debug", FALSE) ) {
	  output$debug_out <- renderPrint({
	    if(input$debug_submit==0) return()
	    isolate( eval(parse(text=input$debug_cmd)) )
	  })
  }
  observe({
    updateSelectInput(session, "file_genome", choices = values$GENOMES)
    updateCheckboxGroupInput(session, 'inst_genomes', choices = unique(installed.genomes()))
  })
  
	
  #Add [S]equence [F]eature setup and reset observers
  observe({
    if(input$SFreset==0) return()
    isolate({ values$SFsetup <- list() }) 
  })
  observe({   
    if(input$SFadd==0) return()
    isolate({
      if(!  grepl( paste0('^[',paste(DNA_BASES,collapse=''),']+$'), toupper(input$SFpattern) ) ) { session$sendCustomMessage("jsAlert", 'Use DNA letters only'); return() }
        #DNA_ALPHABET
        values$SFsetup[[length(values$SFsetup)+1]] <- list(
          name=ifelse(nchar(input$SFname)==0, toupper(input$SFpattern), input$SFname),
          genome="Determined automatically from feature file",
          pattern=toupper(input$SFpattern),
          window=input$SFbin,
          heatmap=input$SFadvanced,
          revcomp=input$SFrevcomp
        ) 
        names(values$SFsetup) <-  make.unique( sapply(values$SFsetup, '[[', 'name') )
      })
  })
  output$SFsetup  <- renderPrint({ 
  	str(values$SFsetup) 
  })
  
  #Subclust logic
  observe({
      values$clusters; input$replot
      if( !isolate(input$heat_seed) ) {
          updateSelectInput(session, 'heat_subclust', choices='All clusters')
          return()
      }
      if( isolate(input$heat_subclust) != "All clusters") return()
      clusters <- values$clusters
      updateSelectInput(session, 'heat_subclust', choices = c('All clusters', sort(unique(clusters))))
      
  })
  
  #Multicore calculations definictions 
  observe( mcCalcStart, quoted = TRUE, label = 'BigCalc')
  observe( mcDoParallel, quoted = TRUE, label = 'Plotting')
  
  #Multicore calculations text outputs and cancel logic
  observe({
    if(input$cancel==0) return()
    parallel:::mckill( isolate(values$proc), signal = 9L )
  })
	
	#Plot message output
	output$plot_message 	<- renderUI({ if( !is.null(values$plotMsg) ) values$plotMsg })
	
	#Rendering plot table	
	observe({
		if( is.null(input$publicRdata) ) { return() }		
		if( input$publicRdata == '' & is.null(values$calcID) )   { values$grfile <- NULL; return() }
		if( input$publicRdata == '' | !nchar(input$publicRdata) )  { return() }
		message('Loading Rdata file: "', input$publicRdata, '"')
		values$grfile <- get(load( file.path('publicFiles', input$publicRdata )))
		values$calcID <- NULL
	})
	output$htmltab <- reactive({
		if( is.null( values$grfile ) )	return('')	
		return( renderHTMLgrid(values$grfile, TRUE, urlSetup$select, addcls=digest::digest(input$publicRdata), isolate(input$subplot_options)) )					
	})
	
	#Determined if plot and dataset save menu shoud be visible
	observe({
	    updateCheckboxInput(session, "showplot", value = !is.null(values$grfile))
	})
	
	#Rendering the image
	output$image <- renderImage({
	  if(is.null(values$im)) return(
        list(src = '', contentType = 'image/png', 
	         alt = 'Select feature/track pair(s) and press "Line plot" or "Heatmap" button'
	    )
	  )
	  list(
        src = values$im,
	    contentType = 'image/png',
	    alt = "Image cannot be displayed"
      )
	}, deleteFile = FALSE)
	
	renderPDF <- function(expr, env=parent.frame(), quoted=FALSE) {
	    func <- shiny::exprToFunction(expr, env, quoted)
	    function() {
	        value <- func()
	        value
	    }
	}
	
	output$thecanvas <- renderPDF({
	    list(im=values$im, id=values$plotid)
	})
  
	output$pdfLink <- renderUI({
	    #if( is.null(values$plotid) ) return()
	    tags$a(
	        tags$span(icon("file-pdf-o", "fa-lg"), 'PDF'), 
	        class="btn btn-small btn-primary", href=values$im, 
	        target='_new', style='margin-left: 5px'
	   )
	}) 
	
	#rendering data dependant plot controles
	observe({
	    if(!is.null(values$grfile)) {
	        
	        rn  <- range( values$grfile[[1]][[1]]$all_ind )
	        rnY <- extendrange( sapply( unlist(values$grfile, recursive=FALSE, use.names=FALSE), '[[', 'means'), f=.1 )
	        
	        updateNumericInput(session, 'xmin1', value = rn[1], min = rn[1], max = rn[2], step = 1L)
	        updateNumericInput(session, 'xmin2', value = rn[2], min = rn[1], max = rn[2], step = 1L)
	        
	        updateNumericInput(session, 'ymin1', value = rnY[1], step = 1L)
	        updateNumericInput(session, 'ymin2', value = rnY[2], step = 1L)
	        
	        #sliderInput('xlim', 'X-axis limits:', min=rn[1], max=rn[2], value=c(rn[1], rn[2]), step=1)
	    }
	})
	

  ## Download handlers
  
	#Legend download handler
	output$downloadLegend <- downloadHandler(
		filename = function() {
			paste('Legend_', chartr(' :', '_-', Sys.time()), '.pdf', sep='')
		},
		content = function(file) {
			co <- lapply(input$plot_this, function(x) fromJSON(x))
			pl <- lapply(co, function(x) values$grfile[[x[2]]][[x[1]]] )
			pdf(file, width = 10.0, height = 10.0, onefile = FALSE, paper = input$paper)
			  plotLineplotLocal(pl=pl, type='legend')
			dev.off()
		},
		contentType = 'application/pdf'
	)
  
  #History download handler
  output$downloadHistory <- downloadHandler(
    filename = function() {
      paste('History_', gsub(' ', '_', Sys.time()), '.pdf', sep='')
    },
    content = function(file) {
      pdf(file, width = 16.0, height = 10.0, onefile = TRUE) #, encoding = "TeXtext.enc")
      for(recPlot in values$plotHistory) {
        #recPlot <- values$plotHistory
        for (i in 1:length(recPlot[[1]])) #@jjallaire
        {
          symbol <- recPlot[[1]][[i]][[2]][[1]]
          if ("NativeSymbolInfo" %in% class(symbol)) {
            if (!is.null(symbol$package))
              name <- symbol$package[["name"]]
            else name <- symbol$dll[["name"]]
            pkgDLL <- getLoadedDLLs()[[name]]
            nativeSymbol <- getNativeSymbolInfo(name = symbol$name,
                                                PACKAGE = pkgDLL, withRegistrationInfo = TRUE)
            recPlot[[1]][[i]][[2]][[1]] <- nativeSymbol
          }
        }
        replayPlot(recPlot)
        }
      dev.off()
    },
    contentType = 'application/pdf'
  )
  
  #Batch operations download handler
  output$downloadBatchColLineplot <- downloadHandler(
    filename = function() {
      paste('Batch_', gsub(' ', '_', Sys.time()), '.pdf', sep='')
    },
    content = function(file) {
      pdf(file, width = input$pdf_x_size, height = input$pdf_y_size, onefile = TRUE, paper=input$paper) #, encoding = "TeXtext.enc")
      par(mfrow=c(input$grid_y_size, input$grid_x_size))
      if(input$pty_batch) par(pty='s')
      nc <- length(values$grfile[[1]]) 
      nr <- length(values$grfile)
      if(input$batch_how=="columns") {
        for(n in 1:nc) {
          pl <- lapply(1:nr, function(x) values$grfile[[x]][[n]] )
          t1 <- sapply(pl, '[[', 'desc') 
          
          title <- input[[paste0('label_',n,'x',1)]]
          if(!nchar(title)) title <- gsub(input$multi_name_flt, '', unique( Map('[[', strsplit(t1, '\n@'), 1) ))
          
          
          if (input$batch_what == "lineplots") {
            plotLineplotLocal(pl, title=title) 
          } else {
            plotHeatmapLocal(pl, title=title) 
          } 
        }
      } else if(input$batch_how=="rows") {
        for(n in 1:nr) {
          pl <- lapply(1:nc, function(x) values$grfile[[n]][[x]] )
          t1 <- sapply(pl, '[[', 'desc') 
          
          title <- input[[paste0('label_',1,'x',n)]]
          if(!nchar(title)) title <- gsub(input$multi_name_flt, '', unique( Map('[[', strsplit(t1, '\n@'), 2) ))
          
          if (input$batch_what == "lineplots") {
            plotLineplotLocal(pl, title=title) 
          } else {
            plotHeatmapLocal(pl, title=title) 
          } 
        }
      } else if(input$batch_how=="single")  {
        for(n in 1:nr) {
          for(m in 1:nc) {
            pl <- list(values$grfile[[n]][[m]])
            title <- input[[paste0('label_',m,'x',n)]]
            if(!nchar(title)) title <- pl[[1]]$desc
            if (input$batch_what == "lineplots") {
              plotLineplotLocal(pl, title=title, legend=FALSE) 
            } else {
              plotHeatmapLocal(pl, title=title, legend=FALSE) 
            } 
          }  
        }
      }
      dev.off()
    },
    contentType = 'application/pdf'
  )
	
	#Lineplot PDF download handler
	output$downloadPlot <- downloadHandler(
		filename = function() {
			paste('Plot_', gsub(' ', '_', Sys.time()), '.pdf', sep='')
		},
		content = function( file ) {			
		  co <- lapply(input$plot_this, function(x) fromJSON(x))
		  pl <- lapply(co, function(x) values$grfile[[x[2]]][[x[1]]] )
		  pdf(file, width = as.integer(input$pdf_x_size), height = as.integer(input$pdf_y_size), paper=input$paper)
		    plotLineplotLocal(pl=pl)		
		  dev.off()
		  #Sys.sleep(1)
		},
		contentType = 'application/pdf'
	)
  
	#Heatmap download handler
	output$downloadHeatmap <- downloadHandler(
			filename = function() {
				paste('Plot_', gsub(' ', '_', Sys.time()), '.pdf', sep='')
			},
			content = function( file ) {
				co <- lapply(input$plot_this, function(x) fromJSON(x))
				pl <- lapply(co, function(x) values$grfile[[x[2]]][[x[1]]] )
				pdf(file, width = as.integer(input$pdf_x_size), height = as.integer(input$pdf_y_size), paper=input$paper)
					plotHeatmapLocal(pl=pl)				
				dev.off()
			}
	)
  
	#Clusters download handler
	output$downloadClusters <- downloadHandler(
	  filename = function() {
	    paste('Clusters_', gsub(' ', '_', Sys.time()), '.csv', sep='')
	  },
	  content = function( file ) {
	    if( is.null(values$clustrep) ) stop('Plot heatmap with clusters or ordering first!')
	    write.csv(values$clustrep, file=file, row.names = FALSE)
	  }
	)

  #Dataset download handler
  output$RdataDoenloadButton <- downloadHandler(
    filename = function() { input$publicRdata },
    content = function( file ) { 
      source <- file.path(getwd(), 'publicFiles', input$publicRdata )
      if( !file.exists(source) ) stop('File does not exist: ', file)
      file.copy(source, file)
    }
  )
  
  ## File operations
	
	#Adding a file to the server
	observe({			
		if( is.null(input$TR_addFile) ) return()
    message('Processing file: ', input$TR_addFile$name, ' [', input$TR_addFile$jobID, ']')
		isolate({
		  tryCatch({
  				x <- input$TR_addFile$jobID
          file_name <- input$TR_addFile$name
  				file_genome <- input$TR_addFile$genome
  				file_user	<- input$TR_addFile$user
  				file_comment<- input$TR_addFile$comments
  				file.copy(from=input[[x]][['datapath']], to=file.path('tmp', file_name))
  				doFileOperations(file.path('tmp', file_name), final_folder='files', file_genome, file_user, file_comment, con=con)
  			
  				session$sendCustomMessage("jsExec", sprintf( '$("#%s").html(\' <span class="label label-success">SUCCESS</span> File %s [%.2f MB] uploaded. \')', 
  				                                             x,  input[[x]][['name']],  input[[x]][['size']] / 1e6 ))
  				#values$refFileGrids <- runif(1)
          
		  }, error = function(e) {
		      file.remove( file.path('tmp', input$TR_addFile$name) )
		      session$sendCustomMessage("jsExec", sprintf( '$("#%s").html(\' <span class="label label-danger">ERROR</span> %s\')', 
		                                                   input$TR_addFile$jobID, "File processing error..." ))
		      session$sendCustomMessage("jsAlert", geterrmessage() )
          
		      #values$refFileGrids <- runif(1)
		  })	
		})
	})
	
    #Get the list of save datasets
    updateSelectizeInput(
      session, 'publicRdata', choices = c( '', dir('publicFiles'))
    )
  
	#Save dataset file logic
	observe({
		if( input$RdataSaveButton == 0 ) return()	
		isolate({
			if (is.null(values$grfile)) {
        session$sendCustomMessage("jsAlert", 'Run calculation first!')
        return(NULL)
			}
			to_save <- values$grfile
			save(to_save, file=file.path('publicFiles', paste0(input$RdataSaveName, '.Rdata')))
					
			message(paste('File saved: ',input$RdataSaveName))
			session$sendCustomMessage("jsAlert", sprintf("File saved: %s", paste0(input$RdataSaveName, '.Rdata')) )
			updateSelectizeInput(session, 'publicRdata', choices = c( '', dir('publicFiles')))
		})
	})
	
	#Remove dataset file logic
	observe({
		if( input$RdataRemoveButton == 0 ) return()
		isolate({
			file.remove( file.path('publicFiles', input$publicRdata) )
			message(paste('File removed: ',input$publicRdata))
			session$sendCustomMessage("jsAlert", sprintf("File removed: %s", input$publicRdata) )
			updateSelectizeInput(session, 'publicRdata',choices = c( '', dir('publicFiles')))
		})
	})
  
  #Feature and track tables - single file removal
  observe({
  		if( is.null(input$delFileVar) ) return()
  		sql_string <- paste0("DELETE FROM files WHERE name = '", input$delFileVar , "'")
  		row_aff <- dbGetRowsAffected(dbSendQuery(con, sql_string))
  		moved <- file.rename(file.path('files', input$delFileVar), file.path('removedFiles', input$delFileVar))
  		session$sendCustomMessage("jsAlert", sprintf("Db=%i; Mv=%i; OK", row_aff, moved));
  		values$refFileGrids <- runif(1)	
  })
  
  #Feature and track tables - multiple file removal
  observe({
    if( is.null(input$TR_delate) ) return()
    isolate({    
        f_delate <- c(
            values$track[input$trackDT_rows_selected,'name'],
            values$feature[input$featureDT_rows_selected,'name']
        )
    
        #actionButton('test', 'TEST', onClick="Shiny.onInputChange('confirm', confirm('Are you sure?'));")    
        
      rmf <- function(x) {
        sql_string <- paste0("DELETE FROM files WHERE name = '", x , "'")
        row_aff <- dbGetRowsAffected(dbSendQuery(con, sql_string))
        moved <- file.rename(file.path('files',  x), file.path('removedFiles', x))
        if(row_aff & moved) return(TRUE) else return(FALSE)
      }
      res <- sapply( f_delate, rmf)
      session$sendCustomMessage("jsAlert", sprintf("Db=%i; Mv=%i; OK", sum(res), sum(res)) )
      values$refFileGrids <- runif(1)	
    })
  })
  


  #Subplot setup logic
  observe({ 
    selections <- c(input$subplot_options, c('inc', 'min', 'max')[c(input$heat_include, input$heat_min_max, input$heat_min_max)])
    
    if( length( selections ) ) {  
      show <- paste(paste0('.div_', selections), collapse=', ')
      session$sendCustomMessage("jsExec", paste0("$('.div_separator,",  show, "').show().children().tooltip()") )
      opt <- c("color", "label", "prior", 'inc', 'min', 'max')
      hide <- paste(paste0('.div_', opt[!opt %in% selections]), collapse=', ')
      session$sendCustomMessage("jsExec", paste0("$('",  hide, "').hide()") )
      extract_grid_values <- function(nam) {
        sapply( lapply(input$plot_this, fromJSON), function(x) eval(substitute(input$b, list(b = paste0(nam,'_',x[1],'x', x[2]) ))) ) 
      }
      for(n in opt) {
        if(n %in% selections) {
          subplotSetup[[n]] <- extract_grid_values(n)
        } else {
          subplotSetup[[n]] <- NULL
        }
      }
    } else {
      session$sendCustomMessage("jsExec", "$('.div_setup').hide()" )
      subplotSetup <- reactiveValues( )
    }
  })

  
  #Generating feature/track tables
  #TODO: merge in one observer
  
# 	#Generate file table for tracks and features
#   observe({
#     values$refFileGrids; input$reloadgrid; input$files; input$TR_delfile; input$upload; input$TR_addFile; input$delFileVar;
#     session$sendCustomMessage("jsExec", "$('#tracktable').html('Loading...')")
#     tab <- dbGetQuery(con, paste0("SELECT * FROM files WHERE type='track' AND name LIKE('%",input$filter_all,"%')"))[,c(-1,-4)]
#     if( nrow(tab) < 1 ) {return(p('No files found!'))} 
#     ex <- as.matrix(tab); rownames(ex) <- NULL; colnames(ex) <- NULL
#     session$sendCustomMessage("jsCreatedDT", list(tab=ex, id='tracktable'))
#   })
#   
#   #Generate file table for features
# 	observe({
# 		values$refFileGrids; input$reloadgrid; input$files; input$TR_delfile; input$upload; input$TR_addFile;
# 		session$sendCustomMessage("jsExec", "$('#featuretable').html('Loading...')")
# 		tab <- dbGetQuery(con, paste0("SELECT * FROM files WHERE type='feature' AND name LIKE('%",input$filter_all,"%')"))[,c(-1,-4)]
# 		if( nrow(tab) < 1 ) {return(p('No files found!'))}
# 		ex <- as.matrix(tab); rownames(ex) <- NULL; colnames(ex) <- NULL
# 		session$sendCustomMessage("jsCreatedDT", list(tab=ex, id='featuretable'))
# 
# 	})
  
  
  #Generate file table for tracks and features with function
  fileSelectionDataTable <- function(type) {

      out <- DT::renderDataTable({
        
        values$refFileGrids; input$reloadgrid; input$files; input$TR_delfile; input$upload; input$TR_addFile;
          
        tab <- dbGetQuery(con, paste0("SELECT * FROM files WHERE type='", type, "' AND name LIKE('%",input$filter_all,"%')"))[,c(-1,-4)]
        if( nrow(tab) < 1 ) {return(p('No files found!'))} 
        values[[type]] <- tab
        
        #tab$ctime <- as.POSIXct(tab$ctime)
        tab <- cbind(tab,  dl='',  rm='')
        rownames(tab) <- NULL
        
        options = list(
            lengthChange = TRUE,
            order=DT::JS('[[ 1, "desc" ]]'),
            lengthMenu=DT::JS('[[10, 25, 50, 100, -1], [10, 25, 50, 100, "All"]]'),
            language=DT::JS('{"sLengthMenu": "_MENU_ records per page"}'),
            dom="<'row'<'col-md-4'i><'col-md-3'C><'col-md-5'f>><'row'<'col-md-12'tr>><'row'<'col-md-6'l><'col-md-6'p>>",
            columns=DT::JS( readLines(file.path(Sys.getenv("web", '.'), 'ui/DataTablesColumnSetup.js')) ),
            searchHighlight = TRUE,
            searchCols=DT::JS('[null,null,null,null,{"search": typeof demo == "undefined" ? null : demo}]'),
            pagingType="full_numbers",
            searchDelay=10,
            processing = TRUE,
            search = list(regex = TRUE)
        )
        
        dt <- DT::datatable(
            tab,
            rownames = FALSE,
            filter = 'bottom',
            options = options,
            selection = 'multiple',
            extensions = c('ColVis')
       
        ) 
        return(dt)
      })
  }
      
        #options = dt_opt
#         callback = DT::JS("function(oTable) {
#           var table = $('#' + oTable.context[0].sTableId);
#           var tables = table.parents('.dataTables_wrapper').find('table')
#           tables.addClass('table-condensed table-bordered');
#           //zzz=oTable.context[0];
#           oTable.draw();
#           $(tables[2]).removeClass('table-bordered');
#         }")
#      )

  
  output$trackDT <- fileSelectionDataTable('track')   
                                        
                                        
      

  output$featureDT <- fileSelectionDataTable('feature')
  
  observe({
  	session$sendCustomMessage("jsExec", "Shiny.shinyapp.$socket.onclose = function () { $(document.body).addClass('disconnected'); alert('Connection to server lost!'); }")
    session$sendCustomMessage("jsExec", "$('.load_div').fadeOut(1000);")
    session$sendCustomMessage("jsExec", "animateTitle();")
    if(Sys.getenv('tutorial', TRUE)) session$sendCustomMessage("jsExec", "startTutorial();")
    
    
    #Session elem:  "clientData","input","isClosed","onFlush","onFlushed","onSessionEnded","output","request","sendCustomMessage","sendInputMessage" 
    #sapply(ls(session$request), function(x) session$request[[x]])
  	#sapply(names(session$clientData), function(x) session$clientData[[x]])
  	#str(as.list(session$clientData))
    
    message(Sys.time(), ' -> Running at ', session$request$HTTP_ORIGIN, ', ', session$clientData$url_hostname, ' [', session$request$HTTP_SEC_WEBSOCKET_KEY, ']')
  })
  session$onSessionEnded(function() { 
      unlink(file.path(Sys.getenv('root'), 'tmp', isolate(values$sessionID)), recursive=TRUE) 
  })
  session$onSessionEnded(function() { message(Sys.time(), ' -> Client connection closed', ' [', session$request$HTTP_SEC_WEBSOCKET_KEY, ']' ) })

  
  #Server reset action
  observe({
  	if( Sys.getenv("SHINY_SERVER_VERSION") == '') return()
  	if( is.null(input$spawn) ) return()
    if( input$spawn==0 ) return()
    session$sendCustomMessage("jsAlert", 'Spawning new server session, it may take awhile.')
  	if( Sys.getenv('web') != '' ) setwd(Sys.getenv('web'))
    system('touch restart.txt')
    session$sendCustomMessage("jsExec", "location.reload(true)")

  })
  
  observe({
      if( !input$genomes_uninstall ) return()
      isolate({
          progress <- shiny::Progress$new(session, min=1, max=3)
          on.exit(progress$close())
          progress$set('Uninstalling packages: ', paste0(input$inst_genomes, collapse = '  '), value = 2)
          sapply(.libPaths(), function(lib) 
              try(remove.packages(input$inst_genomes, lib = lib))
          )
          values$GENOMES <- updateGenomes()
      })
  })
  
  observe({
      if( is.null(input$genomes_file) ) return()
      isolate({
          progress <- shiny::Progress$new(session, min=1, max=3)
          on.exit(progress$close())
          progress$set('Installing packages from file ', value = 2)
          install.packages(
              input$genomes_file$datapath, repos = NULL, 
              lib=file.path(Sys.getenv('root'), 'genomes'), type='source'
          )
          values$GENOMES <- updateGenomes()
      })
  })
  
  observe({
      if( !input$genomes_install ) return()
      isolate({
          progress <- shiny::Progress$new(session, min=1, max=3)
          on.exit(progress$close())
          progress$set('Installing packages: ', paste0(input$avil_geneomes, collapse = '  '), value = 2)
          BiocInstaller::biocLite(
              input$avil_geneomes, suppressUpdates=TRUE, ask=FALSE, 
              lib=file.path(Sys.getenv('root'), 'genomes')
          )
          updateSelectInput(session, 'avil_geneomes', selected = '')
          values$GENOMES <- updateGenomes()
      })
  })
  
  #Exit button logic
  observe({
    if( Sys.getenv("SHINY_SERVER_VERSION") != '') return()
    if( is.null(input$stopapp) ) return()
    if( input$stopapp==0 ) return()
    if( is.null( input$exitconfirmed )) {
      session$sendCustomMessage("jsExec", 'confirm("Are you sure you want to exit!?") ? Shiny.shinyapp.sendInput({"exitconfirmed":true}) : console.log("Exit canceled")')
    } else { 
      session$sendCustomMessage("jsExec", "Shiny.shinyapp.$socket.onclose = null;")
      session$sendCustomMessage("jsExec", "window.onbeforeunload = function(){}; window.open('','_self').close();")
      stopApp(returnValue = 'Stopped by user!' )
    }
  })

  #Server  Query String action
  observe({
    query <- parseQueryString(clientData$url_search)
    if(length(query$genome)){
      #updateSelectInput(session, "file_genome", "Genmoe:", GENOMES, selected = query$genome)
      #session$sendCustomMessage("jsAlert", sprintf('genome: [%s]', query$genome) )
      session$sendCustomMessage("jsExec", sprintf("$('#file_genome').children().removeAttr('selected').filter('[value=%s]').attr('selected', 'selected')", query$genome))
    }
    
    for(n in names(query)[!names(query) %in% c('load', 'select', 'genome')] ){
        session$sendInputMessage(n, list(
            value = unlist(strsplit(query[[n]], ',')) 
        ) )
        
    }
    
    if(length(query$load)){
      #session$sendCustomMessage("jsAlert", sprintf('loading file: [%s]', file.path('publicFiles', query$load)) )
      values$grfile <- get(load( file.path('publicFiles', query$load) ))
      updateSelectizeInput(session, 'publicRdata', choices = c( '', dir('publicFiles')), selected =  query$load)
      #session$sendCustomMessage("jsExec", sprintf("$('#publicRdata').val('%s').change()", query$load))
    }
    

    if( is.character(query$select) ) {
      #session$sendCustomMessage("jsAlert", sprintf('Selecting plots: [%s]', query$select) )
      sel <- do.call( rbind, strsplit(strsplit(query$select, ';')[[1]], ',') )
      class(sel) <- 'integer'
      urlSetup$select <- sel
      #jqcmd <- sprintf("$( '%s' ).click()", paste0('input[value="[', strsplit(query$select, ';')[[1]], ']"]', collapse=',') )
      session$sendCustomMessage("jsExec", "$('#replot').click()")
    }
    #strsplit(strsplit("1,1;3,2", ';')[[1]], ',')
    # paste(names(reactiveValuesToList(input)), reactiveValuesToList(input), sep = "=", collapse="&")
  })
  
  output$nselected <- renderText({
      paste(length(input$trackDT_rows_selected), 'track(s) selected')
  })
  
  observe({
      if(input$selFilt==0) return()
      isolate({
          proxy <- DT::dataTableProxy('trackDT')
          DT::selectRows(proxy, NULL)
          DT::selectRows(proxy, input$trackDT_rows_all)
      })
  })
  
  observe({
      if(input$selPage==0) return()
      isolate({
          proxy <- DT::dataTableProxy('trackDT')
          DT::selectRows(proxy, union(input$trackDT_rows_selected, input$trackDT_rows_current))
      })
  })
  
  observe({
      if(input$selNone==0) return()
      isolate({
          proxy <- DT::dataTableProxy('trackDT')
          DT::selectRows(proxy, NULL)
      })
  })
  
  #######
  
  output$nselectedFT <- renderText({
      paste(length(input$featureDT_rows_selected), 'feature(s) selected')
  })
  
  observe({
      if(input$selFiltFT==0) return()
      isolate({
          proxy <- DT::dataTableProxy('featureDT')
          DT::selectRows(proxy, NULL)
          DT::selectRows(proxy, input$featureDT_rows_all)
      })
  })
  
  observe({
      if(input$selPageFT==0) return()
      isolate({
          proxy <- DT::dataTableProxy('featureDT')
          DT::selectRows(proxy, union(input$featureDT_rows_selected, input$featureDT_rows_current))
      })
  })
  
  observe({
      if(input$selNoneFT==0) return()
      isolate({
          proxy <- DT::dataTableProxy('featureDT')
          DT::selectRows(proxy, NULL)
      })
  })

##Turn off experimental

#   output$chart1 <- renderChart({
#     
#     if( !input$interactiveLinePlot ) stop('Loading...')
#     if( is.null(input$plot_this) ) stop('Nothing to plot')
#     
# 
#     pl <- lapply( lapply(input$plot_this, function(x) fromJSON(x)) , function(x) values$grfile[[x[2]]][[x[1]]] )
#     
#     x <- pl[[1]]$all_ind
#     a <- data.frame( sapply(pl, '[[', 'means') )
#     a <- cbind(a, x=x)
#     
#     if (input$cust_col) {
#       cols <- sapply( lapply(input$plot_this, function(x) fromJSON(x)) , function(x) eval(substitute(input$b, list(b = paste0('col_',x[1],'x', x[2]) ))) )
#       cols[ grepl('#ffffff', cols) ] <- c("darkblue", "darkgreen", "darkred", "darkmagenta", "darkgray", "darkorange", "darkcyan", "black", rainbow(length(pl)-8))[ grepl('#ffffff', cols) ]
#     } else {
#       cols <- c("darkblue", "darkgreen", "darkred", "darkmagenta", "darkgray", "darkorange", "darkcyan", "black", rainbow(length(pl)-8))	
#     }
#  
#     n1=nPlot(value ~ x, group = 'variable', data = reshape2::melt(a, id='x'), type = input$chart1Type)
#     #p$chart(margin=list(top= 30, right= 20, bottom= 50, left= 250))
#     n1$xAxis(axisLabel='GenomicPosition')
#     n1$yAxis(axisLabel='Signal')
#     n1$chart(color = cols)
#     n1$set(dom = "chart1")
#     #n1$addControls("type", value = "lineWithFocusChart", values = c('lineWithFocusChart', 'stackedAreaChart') )
#     
#     return(n1)
#   })
 		#outputOptions(output, "featuretable", suspendWhenHidden = FALSE)
 })
