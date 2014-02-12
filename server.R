# SeqPlots server file - initial cleaning
# 
# Author: PS
###############################################################################

require(parallel)
library(shiny)
library(rtracklayer)
require(RJSONIO)
require(RSQLite)
require(BSgenome)
require(seqnames.db)

#options("xtable.sanitize.text.function" = identity)
options("shiny.maxRequestSize" = -1)
options("bitmapType" = "cairo")
#options(shiny.reactlog = FALSE)



##Turn off experimental
#require(rCharts)
#options(RCHART_WIDTH = 800)


sourceDir <- function(path, trace = TRUE, ...) {
  for (nm in list.files(path, pattern = "[.][RrSsQq]$")) {
    if(trace) cat(nm,":")
    source(file.path(path, nm), ...)
    if(trace) cat("\n")
  }
}

sqlite <- dbDriver("SQLite")
if(Sys.getenv('root') !='') {
  con <- dbConnect(sqlite, dbname = file.path(Sys.getenv('root'),'files.sqlite'))
} else if( file.exists('server_config.R')  ) {
  source('server_config.R')
  con <- dbConnect(sqlite, dbname = file.path(Sys.getenv('root'),'files.sqlite'))
} else {
  con <- dbConnect(sqlite, dbname = 'files.sqlite')
}

shinyServer(function(input, output, clientData, session) {
	
  if( Sys.getenv('web') != '' ) setwd(Sys.getenv('web'))
  sourceDir('functions')
	
  

  
  if( Sys.getenv('root') != '' ) setwd(Sys.getenv('root'))
	addResourcePath(prefix='files', directoryPath='./files')
	cat3 <- function(x) { parallel:::sendMaster(c('calcMsg1', as.character(x))) }
	cat4 <- function(x) { parallel:::sendMaster(c('calcMsg2', as.character(x))) }
	
	#Debug code: Testing eval statement
	output$summary <- renderPrint({
				eval(parse(text=input$caption))
			})
  
	#Reactive values definition
	values <- reactiveValues( grfile=NULL, calcID=NULL, calcMsg1=NULL, calcMsg2=NULL, plotMsg=NULL, 
                            refFileGrids=NULL, proc=NULL, im=NULL, clusters=NULL, include=NULL, SFsetup=list(), plotHistory=list() )
	
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
  
  #Multicore calculations definictions 
  observe( mcCalcStart, quoted = TRUE, label = 'BigCalc')
  observe( mcDoParallel, quoted = TRUE, label = 'Plotting')
  
  #Multicore calculations text outputs and cancel logic
  output$summary2 <- renderPrint({ values$calcMsg1 })
  output$summary3 <- renderPrint({ values$calcMsg2 })
  observe({
    if(input$cancel==0) return()
    parallel:::mckill( isolate(values$proc), signal = 9L )
  })
	
	#Plot message output
	output$plot_message 	<- renderUI({ if( !is.null(values$plotMsg) ) values$plotMsg })
	
	#Rendering plot table	
	observe({
		if( is.null(input$publicRdata) ) { return() }		
		if( input$publicRdata == ' ' & is.null(values$calcID) )   { values$grfile <- NULL; return() }
		if( input$publicRdata == ' ' )   { return() }
		values$grfile <- get(load( file.path('publicFiles', input$publicRdata )))
		values$calcID <- NULL
	})
	output$htmltab <- reactive({
		if( is.null( values$grfile ) )	return('')	
		return( renderHTMLgrid(values$grfile, TRUE, NULL, addcls=digest::digest(input$publicRdata)) )					
	})
	
	#Determined if plot and dataset save menu shoud be visible
	output$showplot <- reactive({ !is.null(input$plot_this) })
	outputOptions(output, "showplot", suspendWhenHidden = FALSE)
	
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
  
	#Heatmap plotting function
	plotHeatmap <- function(pl, title=input$title) {
		if( length(pl) > 10 ) stop('Heatmap plotting: Select less than 10 checkboxes!')
		if( is.null(pl[[1]]$heatmap) ) stop('Heatmap plotting: No heatmap data avilabe! Re-run with "Calculate Heatmap" option selected.')
		Hclc <- do.call(cbind, lapply(pl[values$include], '[[', 'heatmap')) 
    
		pl <- pl[order(values$priors, decreasing=TRUE)]
		H <- do.call(cbind, lapply(pl, '[[', 'heatmap'))
    
		if ( input$scale_signal == "Do not transform" ) {
			#Do Nothing
		} else if ( input$scale_signal ==  "Log2 transform" ) {
			H <- log2(H)
		} else if ( input$scale_signal == "Z-score transform" ) {
			H <- scale(H)
		}
		#H[is.na(H)] <- 0
		if(input$img_sort) { H <- H[order(rowMeans(Hclc, na.rm=TRUE)),] }
		if(input$img_clusters > 1) {
			Hcl <- Hclc; Hcl[is.na(Hcl)] <- 0
			k<-kmeans(Hcl, input$img_clusters) #OPTIONAL: Change this line for differnt number of clusters
			kcenter_sum <- apply(k$centers,1,sum)
			orderkcenter <- order(kcenter_sum)
			orderindex <- order(orderkcenter)
			k1_new <- orderindex[k$cluster]# new class id sorted by center.
			orderk<-order(k1_new)
			k$size <- k$size[orderkcenter]
      H <- H[orderk,]
			#session$sendInputMessage('clusters', list(value=orderk))
			session$sendCustomMessage("jsExec", paste0("$('#clusters').val('",toJSON(k1_new),"').change()"))
      clusts <- k$size
		} else {
		  values$clusters <- NULL
		  clusts <- NULL
		}
		lab <- sapply(pl, '[[', 'desc')
    new_lab <- values$lables[ order(values$priors, decreasing=TRUE) ]
    lab[new_lab!=''] <- new_lab[new_lab!='']
    #lab <- lab[order(values$priors, decreasing=TRUE)]

    o_min <- as.numeric( values$override_min[order(values$priors, decreasing=TRUE)] )
    o_max <- as.numeric( values$override_max[order(values$priors, decreasing=TRUE)] )
    
		runGalaxy( H, clusts, wigcount=length(pl), 
		           bins=pl[[1]]$all_ind, 
               titles=lab, e=pl[[1]]$e, 
               xlim=if(!input$xauto) NULL else c(input$xmin1, input$xmin2), 
               ylabel=input$ylabel,
		           lfs=input$labels_font_size, 
               afs=input$axis_font_size, 
               xlabel=input$xlabel, 
               Leg = input$legend, 
               lgfs=input$legend_font_size,
		           autoscale=!input$heatmapzauto, 
               zmin=input$zmin1, 
               zmax=input$zmin2, 
               ln.v=input$lnv, 
               indi=input$indi, 
               s=input$hsccoef,
               o_min=o_min, 
               o_max=o_max)
		par(cex=input$title_font_size)
		title(input$title, outer = TRUE)
	}
	
	#Rendering the image
	output$image <- renderImage({
	  if(is.null(values$im)) return(list(src = '',contentType = 'image/png',alt = "No image to plot just yet"))
	  list(src = values$im,
	       contentType = 'image/png',
	       width = 1240,
	       height = 720,
	       alt = "This is alternate text")
	}, deleteFile = FALSE)
  
	#renderin data dependant plot controles
  #outputOptions(output, "plotUI", suspendWhenHidden = FALSE)
	observe({
				 if(!is.null(values$grfile)) {
				   
				   rn <- range(values$grfile[[1]][[1]]$all_ind)
				   updateNumericInput(session, 'xmin1', value = rn[1], min = rn[1], max = rn[2], step = 1L)
				   updateNumericInput(session, 'xmin2', value = rn[2], min = rn[1], max = rn[2], step = 1L)
				   #updateNumericInput(session, 'xmin1', value = 100, min = 1, max = 200, step = 1L)
					 #rn <- range(values$grfile[[1]][[1]]$all_ind)
					 #sliderInput('xlim', 'X-axis limits:', min=rn[1], max=rn[2], value=c(rn[1], rn[2]), step=1)
				 }
	})
	

  ## Download handlers
  
	#Legend download handler
	output$downloadLegend <- downloadHandler(
		filename = function() {
			paste('Legend_', gsub(' ', '_', Sys.time()), '.pdf', sep='')
		},
			content = function(file) {
			co <- lapply(input$plot_this, function(x) fromJSON(x))
			pl <- lapply(co, function(x) values$grfile[[x[2]]][[x[1]]] )
			
			if (input$cust_col) {
				cols <- sapply( co, function(x) eval(substitute(input$b, list(b = paste0('col_',x[1],'x', x[2]) ))) )
				cols[ grepl('#ffffff', cols) ] <- c("darkblue", "darkgreen", "darkred", "darkmagenta", "darkgray", "darkorange", "darkcyan", "black", rainbow(length(pl)-8))[ grepl('#ffffff', cols) ]
			} else {
				cols <- c("darkblue", "darkgreen", "darkred", "darkmagenta", "darkgray", "darkorange", "darkcyan", "black", rainbow(length(pl)-8))	
			}
			legendText <- sapply(pl, function(x) x$desc) 
		
			pdf(file, width = 10.0, height = 10.0, onefile = FALSE, paper = "special") #, encoding = "TeXtext.enc")
				plot.new()
				if(input$ee) { legend("topleft", c("Mean\n(solid line)","Standard error\n(dark filed)", "95% CI\n(light field)"), pch=c(-1, 0, 0),  title="Fields and lines legend", lwd=c(3,15,15), lty=c(1,0,0), col=rgb(0,0,0, c(1,0.5, 0.3)), bg=rainbow(1, alpha=0), bty="n", cex=1, y.intersp=2, inset=0.0, seg.len=2, title.adj=c(2, 2)) }
				legend("topright", legendText, col=cols, bg=rainbow(1, alpha=0),  bty="n", cex=1, y.intersp=2, inset=0.0, seg.len=2, title.adj=c(2, 2), lwd=15, pch=0, lty=0)
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
      pdf(file, width = 16.0, height = 10.0, onefile = TRUE) #, encoding = "TeXtext.enc")
      nc <- length(values$grfile[[1]]) 
      nr <- length(values$grfile)
      if(input$batch_how=="rows") {
        for(n in 1:nc) {
          pl <- lapply(1:nr, function(x) values$grfile[[x]][[n]] )
          t1 <- sapply(pl, '[[', 'desc') 
          title <- gsub('_ce10', '', unique( Map('[[', strsplit(t1, '\n@'), 1) ))
          if (input$batch_what == "lineplots") {
            plotLineplot(pl, title=title) 
          } else {
            plotHeatmap(pl, title=title) 
          } 
        }
      } else if(input$batch_how=="columns") {
        for(n in 1:nr) {
          pl <- lapply(1:nc, function(x) values$grfile[[n]][[x]] )
          t1 <- sapply(pl, '[[', 'desc') 
          title <- gsub('_ce10', '', unique( Map('[[', strsplit(t1, '\n@'), 1) ))
          if (input$batch_what == "lineplots") {
            plotLineplot(pl, title=title) 
          } else {
            plotHeatmap(pl, title=title) 
          } 
        }
      } else if(input$batch_how=="single")  {
        for(n in 1:nr) {
          for(m in 1:nc) {
            pl <- list(values$grfile[[n]][[m]])
            title <- '' #pl$desc
            if (input$batch_what == "lineplots") {
              plotLineplot(pl, title=title) 
            } else {
              plotHeatmap(pl, title=title) 
            } 
          }  
        }
      }
      dev.off()
    },
    contentType = 'application/pdf'
  )
	
# 	#main PDF plotting function
# 	plot.pdf <- function(ff='res/out3.pdf') { 
# 		co <- lapply(input$plot_this, function(x) fromJSON(x))
# 		pl <- lapply(co, function(x) values$grfile[[x[2]]][[x[1]]] )
# 		
# 		if (input$cust_col) {
# 			cltab <- sapply( co, function(x) eval(substitute(input$b, list(b = paste0('col_',x[1],'x', x[2]) ))) )
# 		} else {
# 			cltab <- NULL
# 		}
# 		
# 		if ( input$scale_signal == "Do not transform" ) {
# 			plotScale <-  'linear'
# 		} else if ( input$scale_signal ==  "Log2 transform" ) {
# 			plotScale <-  'log2'
# 		} else if ( input$scale_signal == "Z-score transform" ) {
# 			plotScale <-  'zscore'
# 		}
# 		
# 		message('Plotting pdf...')
# 		pdf(ff, width = as.integer(input$pdf_x_size), height = as.integer(input$pdf_y_size))
# 		plotMext(pl, x1=input$xlim[1], x2=input$xlim[2], y1=if(input$yauto) NULL else input$ymin1, y2=if(input$yauto) NULL else input$ymin2,
# 				title=input$title, Xtitle = input$xlabel, Ytitle = input$ylabel, colvec = cltab, plotScale = plotScale, EE = input$ee, Leg = input$legend,
# 				cex.axis = input$axis_font_size, cex.lab = input$labels_font_size, cex.main = input$title_font_size, cex.legend = input$legend_font_size, 
# 				ln.v=input$lnv, ln.h=if(input$lnh) input$lnh_pos else NULL)
# 		dev.off()
# 		Sys.sleep(1)
# 		message('done.')
# 	}
	
	#Lineplot PDF download handler
	output$downloadPlot <- downloadHandler(
		filename = function() {
			paste('Plot_', gsub(' ', '_', Sys.time()), '.pdf', sep='')
		},
		content = function( file ) {			
		  co <- lapply(input$plot_this, function(x) fromJSON(x))
		  pl <- lapply(co, function(x) values$grfile[[x[2]]][[x[1]]] )
		  pdf(file, width = as.integer(input$pdf_x_size), height = as.integer(input$pdf_y_size))
		    plotLineplot(pl=pl)		
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
				#jpeg(file, width = as.integer(input$pdf_x_size)*80, height = as.integer(input$pdf_y_size)*80)
				pdf(file, width = as.integer(input$pdf_x_size), height = as.integer(input$pdf_y_size))
					plotHeatmap(pl=pl)				
				dev.off()
			}
	)
  
	#Clusters download handler
	output$downloadClusters <- downloadHandler(
	  filename = function() {
	    paste('Clusters_', gsub(' ', '_', Sys.time()), '.txt', sep='')
	  },
	  content = function( file ) {
      if(!nchar(input$clusters)) stop('Plot heatmap with clusters first!')
	    cat(fromJSON(input$clusters), sep='\n', file=file)
	  }
	)
	
  
  ## File operations
	
	#Adding a file to the server
	observe({			
		if( is.null(input$TR_addFile) ) return()
    warning('Processing file: ', input$TR_addFile$name, input$TR_addFile$jobID)
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
		      session$sendCustomMessage("jsExec", sprintf( '$("#%s").html(\' <span class="label label-important">ERROR</span>\')', 
		                                                   input$TR_addFile$jobID ))
		      session$sendCustomMessage("jsAlert", geterrmessage() )
          
		      #values$refFileGrids <- runif(1)
		  })	
		})
	})
	
  #Get the list of save datasets
  updateSelectInput(session, 'publicRdata', 'Load public file', c( ' ', dir('publicFiles')), ' ')
  
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
			updateSelectInput(session, 'publicRdata', 'Load public file', c( ' ', dir('publicFiles')), ' ')
		})
	})
	
	#Remove dataset file logic
	observe({
		if( input$RdataRemoveButton == 0 ) return()
		isolate({
			file.remove( file.path('publicFiles', input$publicRdata) )
			message(paste('File removed: ',input$publicRdata))
			session$sendCustomMessage("jsAlert", sprintf("File removed: %s", input$publicRdata) )
			updateSelectInput(session, 'publicRdata', 'Load public file', c( ' ', dir('publicFiles')), ' ')
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
      rmf <- function(x) {
        sql_string <- paste0("DELETE FROM files WHERE name = '", x , "'")
        row_aff <- dbGetRowsAffected(dbSendQuery(con, sql_string))
        moved <- file.rename(file.path('files',  x), file.path('removedFiles', x))
        if(row_aff & moved) return(TRUE) else return(FALSE)
      }
      res <- sapply( input$f_delate, rmf)
      session$sendCustomMessage("jsAlert", sprintf("Db=%i; Mv=%i; OK", sum(res), sum(res)) )
      values$refFileGrids <- runif(1)	
    })
  })

  #ColorButtons
  observe({ 
    if( input$cust_col )
      session$sendCustomMessage("jsExec", "$('input[type=color]').show(0)")
    if( !input$cust_col )
      session$sendCustomMessage("jsExec", "$('input[type=color]').hide(0)")
  })

  
  #Generating feature/track tables
  #TODO: merge in one observer
  
	#Generate file table for tracks and features
  observe({
    values$refFileGrids; input$reloadgrid; input$files; input$TR_delfile; input$upload; input$TR_addFile; input$delFileVar;
    session$sendCustomMessage("jsExec", "$('#tracktable').html('Loading...')")
    tab <- dbGetQuery(con, paste0("SELECT * FROM files WHERE type='track' AND name LIKE('%",input$filter_all,"%')"))[,c(-1,-4)]
    if( nrow(tab) < 1 ) {return(p('No files found!'))} 
    ex <- as.matrix(tab); rownames(ex) <- NULL; colnames(ex) <- NULL
    session$sendCustomMessage("jsCreatedDT", list(tab=ex, id='tracktable'))
  })
  #Generate file table for features
	observe({
		values$refFileGrids; input$reloadgrid; input$files; input$TR_delfile; input$upload; input$TR_addFile;
		session$sendCustomMessage("jsExec", "$('#featuretable').html('Loading...')")
		tab <- dbGetQuery(con, paste0("SELECT * FROM files WHERE type='feature' AND name LIKE('%",input$filter_all,"%')"))[,c(-1,-4)]
		if( nrow(tab) < 1 ) {return(p('No files found!'))}
		ex <- as.matrix(tab); rownames(ex) <- NULL; colnames(ex) <- NULL
		session$sendCustomMessage("jsCreatedDT", list(tab=ex, id='featuretable'))

	})
  
  #Set up subplots option engine on new plotset and show plotUI (checkboxes)
	observe({
	  if(!is.null(input$plot_this)) { 
      isolate({
	      pl <- lapply(lapply(input$plot_this, function(x) fromJSON(x)), function(x) values$grfile[[x[2]]][[x[1]]] )
	      nam <- sapply(pl, '[[', 'desc')
	      values$lables <- vector(mode='character', length=length(pl))
	      values$priors <- vector(mode='integer', length=length(pl))
	      values$include <- rep(TRUE, length(pl))
	      values$override_min <- rep(NA, length(pl))
	      values$override_max <- rep(NA, length(pl))
	      names(values$lables) <- names(values$priors) <- names(values$include) <- names(values$override_min) <- names(values$override_max) <- nam
	      updateSelectInput(session, inputId='img_ch', choices=nam)
	      #outputOptions(output, "plotUI", suspendWhenHidden = FALSE)
      })
	  }
	}, priority = 1)
  
  #Update subplots options inputs on selection change
	observe({
	  if(input$img_ch != '') { 
      isolate({
      updateTextInput(session,     'img_lab',     value = as.character( values$lables[input$img_ch] ))
      updateNumericInput(session,  'img_prior',   value = as.numeric( values$priors[input$img_ch] ))
      updateCheckboxInput(session, 'img_include', value = as.logical( values$include[input$img_ch] ))
      updateNumericInput(session,  'img_o_min',   value = as.numeric( values$override_min[input$img_ch] ))
      updateNumericInput(session,  'img_o_max',   value = as.numeric( values$override_max[input$img_ch] ))
	   })
	  }
	})
  
  #Update subplots rective values on inputs change
  observe({
    input$img_lab; input$img_prior; input$img_include; input$img_o_min; input$img_o_max
    isolate({
      if(input$img_ch == '') return()
      values$lables[input$img_ch] <- input$img_lab
      values$priors[input$img_ch] <- input$img_prior
      values$include[input$img_ch] <- input$img_include
      values$override_min[input$img_ch] <- input$img_o_min
      values$override_max[input$img_ch] <- input$img_o_max
    })
  })
  
  #Hint on Independent color scaling
  observe({
    if(!is.na(input$img_o_min) || !is.na(input$img_o_max) ) {
      if( !input$indi ) session$sendCustomMessage("jsAlert", 'Select "Independent color scaling for heatmaps" on "Axis" tab to activate sub-plot overrides.')
    }

  })
  
  #Server initiation actions
  observe({
  	session$sendCustomMessage("jsExec", "Shiny.shinyapp.$socket.onclose = function () { $(document.body).addClass('disconnected'); alert('Connection to server lost!'); location.reload('true'); }")
    session$sendCustomMessage("jsExec", "$('.load_div').fadeOut(100);")
    session$sendCustomMessage("jsExec", "animateTitle();")
    #Session elem:  "clientData","input","isClosed","onFlush","onFlushed","onSessionEnded","output","request","sendCustomMessage","sendInputMessage" 
    #sapply(ls(session$request), function(x) session$request[[x]])
  	#sapply(names(session$clientData), function(x) session$clientData[[x]])
  	#str(as.list(session$clientData))
    message(Sys.time(), ' -> Running at ', session$request$HTTP_ORIGIN, session$clientData$url_hostname, ' [', session$request$HTTP_SEC_WEBSOCKET_KEY, ']')
  })
  session$onSessionEnded(function() { message(Sys.time(), ' -> Client connection closed', ' [', session$request$HTTP_SEC_WEBSOCKET_KEY, ']' ) })
  
  #Server reset action
  observe({
  	if( Sys.getenv("SHINY_SERVER_VERSION") == '') return()
    if(input$spawn==0) return()
    session$sendCustomMessage("jsAlert", 'Spawning new server session, it may take awhile.')
  	if( Sys.getenv('web') != '' ) setwd(Sys.getenv('web'))
    system('touch restart.txt')
    session$sendCustomMessage("jsExec", "location.reload(true)")

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
