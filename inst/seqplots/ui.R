# Author: przemol
###############################################################################

# require(rCharts)
# options(RCHART_LIB = 'nvd3')

hlp <- function(target, top=-10) {
  return(tags$a(class='pull-right', href=paste0("help/help.html#", target), target="_blank", 
    tags$i(class="icon-question-sign icon-2x", style=sprintf('color:orange; margin-right:5px; position: relative; top: %ipx;', top))
  ))
}

# Define UI
shinyUI(
		bootstrapPage(
      
###############################################################################
# Head      
###############################################################################
				tags$head(
						# JS error message
						singleton(tags$script('var error = false; window.onerror =  function() { if (!error) {alert("JavaScript error! Some elements might not work proprely. Please reload the page."); error=true;} }')),		
						
            # JS alert handle
						#singleton(tags$script('Shiny.addCustomMessageHandler("jsAlert", function(message) {alert(JSON.stringify(message));});')),
						singleton(tags$script('Shiny.addCustomMessageHandler("jsAlert", function(message) {alert(message);});')),
            
						# JS exec handle
						singleton(tags$script('Shiny.addCustomMessageHandler("jsExec", function(message) {eval(message);});')),
            
						# JS exec handle
						singleton(tags$script('Shiny.addCustomMessageHandler("jsAssign", function(message) {M = message;});')),
            
						# CSS impprt						
            # singleton(tags$link(rel="stylesheet", type="text/css", href="http://netdna.bootstrapcdn.com/bootstrap/3.0.0/css/bootstrap.min.css")),
						singleton(tags$link(rel="stylesheet", type="text/css", href="css/style.css")),
						singleton(tags$link(rel="stylesheet", type="text/css", href="css/DT_bootstrap.css")),
						#singleton(tags$link(rel="stylesheet", type="text/css", href="http://cdn.datatables.net/plug-ins/28e7751dbec/integration/bootstrap/2/dataTables.bootstrap.css")),
						singleton(tags$link(rel="stylesheet", type="text/css", href="css/TableTools.css")),	
            #singleton(tags$link(rel="stylesheet", type="text/css", href="http://cdn.datatables.net/tabletools/2.2.1/css/dataTables.tableTools.css")),

						singleton(tags$link(rel="stylesheet", type="text/css", href="css/font-awesome.min.css")),
						
						# JS import
						## miscellaneous libraries
						singleton(tags$script(src = "js/jquery.ba-throttle-debounce.js")),
						singleton(tags$script(src = "js/jquery.lettering.js")),
						singleton(tags$script(src = "js/tmpl.min.js")),
						singleton(tags$script(src = "js/jquery.cookie.js")),
						singleton(tags$script(src = "color/jscolor.js")),
						## DataTable libraries
						singleton(tags$script(src = "js/DataTables/jquery.dataTables.js")),
						singleton(tags$script(src = "js/DataTables/DT_bootstrap.js")),
						singleton(tags$script(src = "js/DataTables/dataTables.tableTools.min.js")),
						singleton(tags$script(src = "js/DataTables/DT_filter.js")),
						
						## My scripts
						singleton(tags$script(src = "js/color.js")),
						singleton(tags$script(src = "js/js_addons.js")),
						singleton(tags$script(src = "js/load_finished.js")),
						
						# Title
						tags$title('SeqPlots')
				),
###############################################################################
# Modals      
###############################################################################
        #Page loading modal and mask, pops out just after page loads, removed after all JS loads, stays on JQuery error
        includeHTML( file.path(Sys.getenv("web", '.'), 'ui/loadModal.html') ),     

        #Calculation progress modal
        div(id="progressModal", class="modal hide", 'data-backdrop'="false", 'data-keyboard'="false", tabindex=-1,
            div(class="modal-header", tags$h3(id="progressModalLabel", 'Calculating...')),
            div(class="modal-body", verbatimTextOutput("summary2"), verbatimTextOutput("summary3"), actionButton('cancel', 'Cancel'))
        ),

        ##File management modal
        eval(parse( file.path(Sys.getenv("web", '.'), 'ui/FileManagementModal.R') )),

		#Animated header
		tags$div(id="letter-container", class="letter-container", HTML('<h2><a href="#">SeqPlots</a></h2>')),
				
			
###############################################################################
#Siadebar      
###############################################################################
				#Sidebar panel definitions
				sidebarPanel(
				  
          #0) PLOT PANEL
					conditionalPanel(condition = "output.showplot",
						p(class='pull-left', HTML("Download: &nbsp;"),
              div(class="btn-toolbar", div(class="btn-group",
								downloadLink('downloadPlot', 	tags$span(tags$i(class="icon-picture icon-large icon-white"), 'Line plot'),   class="btn btn-small btn-success"),
                downloadLink('downloadLegend', tags$span(tags$i(class="icon-info icon-large")), class="btn btn-small btn-success") #Legend
              ),
						  div(class="btn-group",
						      downloadLink('downloadHeatmap', tags$span(tags$i(class="icon-th icon-large icon-white"), 'Heatmap'), class="btn btn-small btn-info"),
						      downloadLink('downloadClusters', tags$span(tags$i(class="icon-sitemap icon-large")), class="btn btn-small btn-info") #'Clusters indicates'
						    
						  )),
						  hlp("Plotting", top=0),
						    #div(class="hidden", actionButton('plotHmap', 'Plot')),
                #div(class="img hidden", plotOutput(outputId = "plot", width = "1240px", height = "720px") ),
						    div(class="img", imageOutput(outputId = "image", width = "1169px", height = "782px") )
				
						),
						conditionalPanel(condition = "!input.reactive", div(class='row-fluid',
						                                                    div(class='span2', 'Preview '),
						                                                    div(class='span4', tags$a(id='replotL', onClick="$('#img_heatmap').prop('checked', false).change(); $('#replot').click();", class='btn btn-normal', tags$span(tags$i(class="icon-picture"), 'Line plot' ))),
						                                                    div(class='span4', tags$a(id='replotH', onClick="$('#img_heatmap').prop('checked', true ).change(); $('#replot').click();", class='btn btn-normal', tags$span(tags$i(class="icon-th"), 'Heatmap' ))), 
						                                                    div(class='span2', actionButton('replot', tags$span(tags$i(class="icon-refresh icon-large")) )),
                                                                tags$hr()
						))
					),
			
					
					tabsetPanel(id='ctltabs',
					#1) NEW PLOT SET PANEL
								tabPanel(value = 'panel1', title=tags$i(class="icon-rocket icon-large icon-blcak", 'data-placement'="right", 'data-toggle'="tooltip", title="New plot set/Upload files") #, "New"
										,h5('Upload files:', hlp("Addingandmanagingfiles"))
										,helpText( "Add signal tracks (bigWig, wig or bedGraph) and feature files (GFF and BED) to the file collection.") # TIP: You can add multiple files at once.
		  								,HTML('<a href="#fileUploadModal" role="button" class="btn btn-success" data-toggle="modal"><i class="icon-cloud-upload icon-large icon-white"></i> Add files</a>')
                      ,conditionalPanel("false", selectInput("file_genome", "Genmoe:", GENOMES, selected = 'ce10', selectize = FALSE)) #This should stay for clonning, unless I can figure out something better using JS
                    ,tags$hr()
                    
										,h5('Create new plot array:', hlp("Runningtheplot-setjobs"))
										,helpText( "Choose signal tracks and feature files from the collection to use for calculating average plots and heat maps.")
										,a(href="#myModal", role="button", class="btn btn-primary", 'data-toggle'="modal", tags$i(class="icon-magic icon-large icon-white"), "New plot set")
										,uiOutput('plot_message')
										,if (Sys.getenv("SHINY_SERVER_VERSION") != '') {div(
											tags$hr(), h5('Soft restart the server session', hlp("Advancedoptions")),
											helpText( "The button will perform soft reset of the server. 
												This means a new session will be created for you and other active users will use their existing session(s) until they close the web browser.
												Use this option if you experience performance issues or errors."),
                      	actionButton('spawn', tags$span(tags$i(class="icon-bolt icon-large"), HTML('Restart server!') ))
								    	)} else {div(
								    	  tags$hr(), helpText( "Stop and exit the web interface:"),
								    	  actionButton('stopapp', tags$span(tags$i(class="icon-off icon-large"), HTML('Exit SeqPlots') ))
								    	)},
										tags$hr(),
										tags$a(class='', tags$span(class="label label-success", 'Help'), href=paste0("help/help.html"), target="_blank", 'Read SeqPlots documentation'),
                    ' or press ', tags$i(class="icon-question-sign icon-large", style='color:orange'), 
                    ' button to get help on specific controls. Also available as ', tags$a(class='', href=paste0("help/SeqPlots.pdf"), target="_blank", 'print ready PDF file.')
										
								),
					#3) TITLES AND AXIS PANEL
								tabPanel(value = 'panel3', title=tags$i(class="icon-font icon-large icon-blcak", 'data-placement'="right", 'data-toggle'="tooltip", title="Title and axis"), #"Axis", 
                         #conditionalPanel(condition = "output.showplot", 	
								    h5(tags$u('Title and axis'), hlp("Titlesandaxispanel")),
										div(class='row-fluid', 
												div(class='span4', textInput("title", "Title:", ""),         
                            sliderInput("title_font_size", "Title font size:",   1, 48, 20, 1) ),
												div(class='span4', textInput("xlabel", "X-axis label:", ""), 
                            sliderInput("labels_font_size", "Labels font size:", 1, 48, 16, 1) ),
												div(class='span4', textInput("ylabel", "Y-axis label:", ""), 
                            sliderInput("axis_font_size", "Axis font size:",     1, 48, 14, 1) )
										),
										#uiOutput("plotUI"),
                    tags$hr(),
									  checkboxInput("xauto", "Set X-axis limits", FALSE),
									  conditionalPanel( condition = "input.xauto == true",
									    p( numericInput("xmin1", "-> ", 0), numericInput("xmin2", "-", 0) )
									  ),               
										checkboxInput("yauto", "Set Y-axis limits", FALSE),
										conditionalPanel( condition = "input.yauto == true",
											p( numericInput("ymin1", "-> ", -1), numericInput("ymin2", "-", 10) )
										)
									#)
								),
					  #4) Guide lines, and data scaling
								tabPanel(value = 'panel4', title=tags$i(class="icon-tags icon-large icon-blcak", 'data-placement'="right", 'data-toggle'="tooltip", title="Guide lines and data scaling"),# "Setup", 
								         h5(tags$u('Guide lines and data scaling'), hlp("Guidelinesanddatascaling")),
                         selectInput('scale_signal', 'Transform signal:', c( 'Do not transform', 'Log2 transform')), #, 'Z-score transform')),
												checkboxInput("lnv", "Show vertical guide line", TRUE),
								        div(class='row-fluid',  
								            div(class='span8',checkboxInput("lnh", "Show horizontal guide line", FALSE)),
								            div(class='span4',conditionalPanel( condition = "input.lnh == true",numericInput("lnh_pos", "-> position:", 0)))
                        ),
												checkboxInput("ee", "Show error estimates", TRUE)
								),
					#5) Subplot options/Labels and colours               
					tabPanel(value = 'panel5', title=tags$i(class="icon-list-ol icon-large icon-blcak", 'data-placement'="right", 'data-toggle'="tooltip", title="Keys, labels and colors"), #"Subplots",
					         h5(tags$u('Keys, labels and colors'), hlp("Keys,labelsandcolorspanel")),
                   checkboxGroupInput('subplot_options', 'Set sub-plot specific:', c('Colors'='color', 'Label'='label', 'Priority/Order'='prior'), selected = NULL),
					         tags$hr(),
                   div(class='row-fluid',  
					             div(class='span8',checkboxInput("legend", "Show plot key", TRUE)),
					             div(class='span4',conditionalPanel( condition = "input.legend == true", selectInput("legend_pos", "-> position:", c("bottomright", "bottom", "bottomleft", "left", "topleft", "top", "topright", "right", "center"), 'topright' )))
					         ),
					         div(class='row-fluid', 
					             div(class='span8',checkboxInput("legend_ext", "Show error estimate key", FALSE)),
					             div(class='span4',conditionalPanel( condition = "input.legend_ext == true", selectInput("legend_ext_pos", "-> position:", c("bottomright", "bottom", "bottomleft", "left", "topleft", "top", "topright", "right", "center"),  "topleft" )))
					         ),
					         sliderInput("legend_font_size", "Legend font size:", 1, 48, 12, 1, ticks = TRUE)
					),
					#6) HEATMAP SPECIFIC OPTIONS
								tabPanel(value = 'panel6', title=tags$i(class="icon-th icon-large icon-blcak", 'data-placement'="right", 'data-toggle'="tooltip", title="Heatmap setup"), #("Sizes", 
								  h5(tags$u('Heatmap setup'), hlp("Heatmapsetuptab")),
								  div( class='hidden', checkboxInput("img_heatmap", "Preview heatmap [Ctrl+H]") ), 
									checkboxInput("img_sort", "Sort heatmap rows by mean signal"),
									div(class='row-fluid',
									  div(class='span6', selectInput("img_clstmethod", 'Clustering algorithm', c('K-means'='kmeans', 'Hierarchical'='hclust', 'SuperSOM'='ssom', 'do not cluster'='none'))),
									  div(class='span6',   
                        conditionalPanel( condition = "input.img_clstmethod != 'none' && input.img_clstmethod != 'ssom'", numericInput("img_clusters", "Number of clusters", 5, min=1)),
									      conditionalPanel( condition = "input.img_clstmethod == 'ssom'", 
									                        div(class='row-fluid',
									                            div(class='span1'), div(class='span3', numericInput("img_ssomt1", "topo_x", 2, min=1) ),
									                            div(class='span2'), div(class='span3', numericInput("img_ssomt2", "topo_y", 2, min=1) )
                                          )
                        )
									  )
									), 
									checkboxInput('heat_include', 'Choose individual heatmaps for sorting/clustering', FALSE),
                  tags$br(),
                  
									checkboxInput("indi", "Heatmaps have individual color keys", TRUE),
									checkboxInput("heatmapzauto", "Set default color key limits", FALSE),
									conditionalPanel( condition = "input.heatmapzauto == false",
									  div(class='row-fluid',
									                      div(class='span5', tags$br(), "Color key scaling:"),
									                      div(class='span7', sliderInput("hsccoef", "Color key saturation:", 0, 0.1, 0.01, 0.001, ticks = TRUE)									                      )
									                  )                
									   
									),
									conditionalPanel( condition = "input.heatmapzauto == true",
									    p( numericInput("zmin1", "-> ", -1), numericInput("zmin2", "-", 10) )
									),
									conditionalPanel( condition = "input.indi == true",
									                  checkboxInput('heat_min_max', 'Set individual color key limits', FALSE)
									),
									checkboxInput('heat_colorspace', 'Set default colorspace', FALSE),
									conditionalPanel( condition = "input.heat_colorspace == true",
									                  div(class='row-fluid', 
									                      div(class='span4', HTML('Min: <input type="color" class="color {hash:true}" id="heat_csp_min" value="#FFFFFF" style="width:40px;" title=""/>')),
									                      div(class='span4', HTML('Mid: <input type="color" class="color {hash:true}" id="heat_csp_mid" value="#87CEFA" style="width:40px;" title=""/>')),
									                      div(class='span4', HTML('Max: <input type="color" class="color {hash:true}" id="heat_csp_max" value="#00008B" style="width:40px;" title=""/>'))
									                  )
									)
									
								),
					#6) SAVE/LOAD PLOT SET PANEL
					      tabPanel(value = 'panel2', title=tags$i(class="icon-save icon-large icon-blcak",  'data-placement'="right", 'data-toggle'="tooltip", title="Load/manage saved plotset"), #"Saved",										
					         h5(tags$u('Load or save plotset'), hlp("Savingandloadingplotsets")),
                   selectInput('publicRdata', 'Load saved plot set:', ' ', ' '),
					         conditionalPanel("input.publicRdata !== ' '", 
					                          actionButton('RdataRemoveButton', 'Remove dataset', icon=icon('trash-o')) ,
					                          downloadButton('RdataDoenloadButton', 'Download dataset') 
					         ), tags$hr(),
					         textInput('RdataSaveName', 'Save current plot set:', ''), 
					         conditionalPanel("input.RdataSaveName !== ''", actionButton('RdataSaveButton', 'Save', icon=icon('save')) ) 
					      ),
					#7) BATCH
						    tabPanel(tags$i(class="icon-gears icon-blcak icon-large", 'data-placement'="right", 'data-toggle'="tooltip", title="Batch operations and setup"), #"Batch", 
						      
                  h5('Output PDF paper type or size [inches]:', hlp("PDFoutputsize")), 
						      div(class='row-fluid', 
						          div(class='span8',  selectInput('paper', '', choices=c('A4 rotated'="a4r", 'Custom size...'="special", 'Legal rotated'="USr", 'A4'="a4", 'Letter'="letter", 'Legal'="US", 'Executive'="executive") ))
						      ),
						      conditionalPanel( condition = 'input.paper == "special"', div(class='form-inline', 
						        numericInput("pdf_x_size", "", 16) ,  
						        numericInput("pdf_y_size", "[in] x ", 10) , '[in]'
						      )),
                  tags$hr(),
						      conditionalPanel( condition = 'false',
						        checkboxInput('recordHistory', 'Record plot history', FALSE),      
						        downloadLink('downloadHistory',   tags$span(tags$i(class="icon-time icon-large"), 'Get plot history'),   class="btn btn-small btn-success"),
                    tags$hr()
						      ),
						      h5('Batch operations:', hlp("Batchoperations")),   
						      div(class='form-inline', 
                    selectInput('batch_what', 'Plot', c('lineplots', 'heatmaps') ),  
                    selectInput('batch_how', 'by', c('single', 'rows', 'columns') )                                
						      ),
						      div(class='form-inline', 
						          numericInput("grid_x_size", "Multi-plot grid: ", 1) ,  
						          numericInput("grid_y_size", " x ", 1) 
						      ), 
						      div(class='form-inline', textInput('multi_name_flt', 'Filter names') ),
						      downloadLink('downloadBatchColLineplot', tags$span(tags$i(class="icon-align-justify icon-rotate-90"), tags$i(class="icon-double-angle-right icon-large"),  tags$i(class="icon-picture icon-large icon-white"), 'Get PDF'),   class="btn btn-small btn-success"),
                  tags$hr(),
						      #checkboxInput('setup_multithread', 'Use multithreading for calculations', (Sys.getenv("SHINY_SERVER_VERSION") != ''))
                  h5('Advanced options:', hlp("Advancedoptions")),
						      checkboxInput('pty_batch', 'Keep 1:1 aspect ratio in batch mode', TRUE),
						      checkboxInput('pty', 'Always keep 1:1 aspect ratio', FALSE),
						      checkboxInput("reactive", "Reactive plotting [ctrl+R]", FALSE),
						      conditionalPanel( condition = tolower(as.character(Sys.getenv("SHINY_SERVER_VERSION") == '')),
						        checkboxInput('setup_multithread', 'Use multithreading for calculations', .Platform$OS.type != 'windows')
						      )
						      
						                     
						    )

						),
					  
				    div( class='hidden', textInput('clusters', 'Clusters'), textInput('sortingord', 'Sorting'), textInput('finalord', 'Sorting') )
				),
				mainPanel(
					#uiOutput('reactiveScripts')
					 singleton(tags$script(src = "upload/js/vendor/jquery.ui.widget.js"))
					,singleton(tags$script(src = "upload/js/jquery.iframe-transport.js"))
					,singleton(tags$script(src = "upload/js/jquery.fileupload.js"))		
					,singleton(tags$script(src = "upload/js/jquery.fileupload-fp.js"))
					,singleton(tags$script(src = "upload/js/jquery.fileupload-ui.js"))
					,singleton(tags$script(src = "upload/js/md5.js"))
					,singleton(tags$script(src = "upload/js/main.js"))
					,singleton(tags$link(rel="stylesheet", type="text/css", href="upload/css/jquery.fileupload-ui.css"))
					,includeHTML(file.path(Sys.getenv("web", '.'), 'www/upload/upload.html')),
          
# 					tabsetPanel(
# 					  tabPanel("Plots selection", 
					           tags$br(),tags$br(),tags$br(),tags$br(),
					           div(class="control-group", uiOutput("htmltab") )
#             ),
# 					  tabPanel("Interactive lineplot", 
#                      checkboxInput('interactiveLinePlot', 'Enable intercative plot', FALSE),
#                      conditionalPanel(condition = "input.interactiveLinePlot", 
#                         selectInput('chart1Type', label='Select type', choices=c('lineWithFocusChart','lineChart','stackedAreaChart','scatterChart', 'multiBarChart'), selected='lineWithFocusChart'),              
# 					              showOutput("chart1", "nvd3")
#                      )
#             )
# 					)

					#Debug code
					,if( Sys.getenv("seqplots_debug", FALSE) ) {
              div( class='', id='debug', tags$hr(),
					    'Debug console: ', tags$br(), tags$textarea(id='debug_cmd', rows=4, style='width:88%'),
					    actionButton('debug_submit', 'Submit'), verbatimTextOutput("debug_out")
            )
					}
				)
  )
)
