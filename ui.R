# Author: przemol
###############################################################################

# require(rCharts)
# options(RCHART_LIB = 'nvd3')



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
						singleton(tags$link(rel="stylesheet", type="text/css", href="css/style.css")),
						singleton(tags$link(rel="stylesheet", type="text/css", href="css/DT_bootstrap.css")),
						singleton(tags$link(rel="stylesheet", type="text/css", href="css/TableTools.css")),
						singleton(tags$link(rel="stylesheet", type="text/css", href="css/font-awesome.min.css")),
						
						
						# JS import
						singleton(tags$script(src = "js/color.js")),
						singleton(tags$script(src = "js/jquery.ba-throttle-debounce.js")),
						singleton(tags$script(src = "js/jquery.lettering.js")),
						singleton(tags$script(src = "js/jquery.dataTables.js")),
						singleton(tags$script(src = "js/DT_bootstrap.js")),
						singleton(tags$script(src = "js/DT_filter.js")),
						singleton(tags$script(src = "js/js_addons.js")),
						singleton(tags$script(src = "js/load_finished.js")),
						singleton(tags$script(src = "js/tmpl.min.js")),
						singleton(tags$script(src = "js/TableTools.min.js")),
						singleton(tags$script(src = "js/dataTables.scroller.min.js")),
						singleton(tags$script(src = "js/jquery.cookie.js")),
						singleton(tags$script(src = "color/jscolor.js")),
						
						
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
				#tags$div(id="letter-container", class="letter-container", HTML('<h2><a href="#">GFplots</a></h2>')), # v1.0b
				tags$div(id="letter-container", class="letter-container", HTML('<h2><a href="#">SeqPlots</a></h2>')), # v1.0b
				
				#Banner
        eval(parse( file.path(Sys.getenv("web", '.'), 'ui/banner.R') )),
				
			
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
						    #div(class="hidden", actionButton('plotHmap', 'Plot')),
                #div(class="img hidden", plotOutput(outputId = "plot", width = "1240px", height = "720px") ),
						    div(class="img", imageOutput(outputId = "image", width = "1240px", height = "720px") )
				
						)
					),
					div(class='row-fluid', 
              div(class='span5',conditionalPanel(condition = "!input.reactive", actionButton('replot', tags$span(tags$i(class="icon-refresh icon-large"), HTML('<b>PLOT</b> [&crarr;]') )))),
					    div(class='span7',checkboxInput("reactive", "Reactive plotting [ctrl+R]", FALSE)), tags$hr()
					),
					
					tabsetPanel(id='ctltabs',
					#1) NEW PLOT SET PANEL
								tabPanel(value = 'panel1', title=tags$i(class="icon-rocket icon-large icon-blcak", 'data-placement'="right", 'data-toggle'="tooltip", title="New plotset/Upload files") #, "New"
										,h5('Upload files:')
										,helpText( "This panel is used to add track (BigWig, Wiggle or BedGraph) and feature (GFF and BED) files to file collection.
												  	Please provide your user ID (initials, eg JS fot John Smith) and genome specify version.
													You can drag-and-drop the files to browser window. Comments are optional.") # TIP: You can add multiple files at once.
		  								,HTML('<a href="#fileUploadModal" role="button" class="btn btn-success" data-toggle="modal"><i class="icon-cloud-upload icon-large icon-white"></i> Add files</a>')
                      ,conditionalPanel("false", selectInput("file_genome", "Genmoe:", GENOMES, selected = 'ce10', selectize = FALSE)) #This should stay for clonning, unless I can figure out something better using JS
                    ,tags$hr()
                    
										,h5('Create new plot array:')
										,helpText( "This panel allows to calculate new plot array. Each track will be summarized using each feature file.
													Subsequently the plot arry will be shown to indicate which pairs should be plotted.")
										,a(href="#myModal", role="button", class="btn btn-primary", 'data-toggle'="modal", tags$i(class="icon-magic icon-large icon-white"), "New plot set")
										,uiOutput('plot_message')
										,if (Sys.getenv("SHINY_SERVER_VERSION") != '') {div(
											tags$hr(), h5('Soft restart the server session'),
											helpText( "The button will perform soft reset of the server. 
													This means a new session will be created for you and other active users will use their existing session(s) until they close the web browser.
													Use this option if you experience performance issues or errors."),
                      						actionButton('spawn', tags$span(tags$i(class="icon-bolt icon-large"), HTML('Restart server!') ))
								    	)} else {
                      						tags$br()
								    	}
								),
					#2) SAVE/LOAD PLOT SET PANEL
								tabPanel(value = 'panel2', title=tags$i(class="icon-save icon-large icon-blcak",  'data-placement'="right", 'data-toggle'="tooltip", title="Load/manage saved plotset"), #"Saved",										
                    selectInput('publicRdata', 'Load public file:', ' ', ' '),
										conditionalPanel("input.publicRdata !== ' '", 
                                     actionButton('RdataRemoveButton', 'Remove dataset', icon=icon('trash-o')) ,
										                 downloadButton('RdataDoenloadButton', 'Download dataset') 
                    ), tags$hr(),
										textInput('RdataSaveName', 'Save current plot set as:', ''), 
										conditionalPanel("input.RdataSaveName !== ''", actionButton('RdataSaveButton', 'Save', icon=icon('save')) ) 
								),
					#3) TITLES AND AXIS PANEL
								tabPanel(value = 'panel3', title=tags$i(class="icon-font icon-large icon-blcak", 'data-placement'="right", 'data-toggle'="tooltip", title="Title and axis"), #"Axis", 
                         #conditionalPanel(condition = "output.showplot", 										
										div(class='row-fluid', 
												div(class='span4', textInput("title", "Title:", ""),         
                            sliderInput("title_font_size", "Title font size:",   0.5, 10, 2.0, 0.5, ticks = TRUE, animate = TRUE) ),
												div(class='span4', textInput("xlabel", "X-axis label:", ""), 
                            sliderInput("labels_font_size", "Labels font size:", 0.5, 10, 2.0, 0.5, ticks = TRUE, animate = TRUE) ),
												div(class='span4', textInput("ylabel", "Y-axis label:", ""), 
                            sliderInput("axis_font_size", "Axis font size:",     0.5, 10, 2.0, 0.5, ticks = TRUE, animate = TRUE) )
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
					  #4) Legends, guide lines, and data scaling
								tabPanel(value = 'panel4', title=tags$i(class="icon-tags icon-large icon-blcak", 'data-placement'="right", 'data-toggle'="tooltip", title="Legends, guide lines, and data scaling"),# "Setup", 
												selectInput('scale_signal', 'Transform signal:', c( 'Do not transform', 'Log2 transform')), #, 'Z-score transform')),
												checkboxInput("lnv", "Show vertical guiding line", TRUE),
								        div(class='row-fluid',  
								            div(class='span8',checkboxInput("lnh", "Show horizontal guiding line", FALSE)),
								            div(class='span4',conditionalPanel( condition = "input.lnh == true",numericInput("lnh_pos", "-> position:", 0)))
                        ),
												checkboxInput("ee", "Show error estimates", TRUE),
								        div(class='row-fluid',  
								            div(class='span8',checkboxInput("legend", "Show plot legend", TRUE)),
								            div(class='span4',conditionalPanel( condition = "input.legend == true", selectInput("legend_pos", "-> position:", c("bottomright", "bottom", "bottomleft", "left", "topleft", "top", "topright", "right", "center"), 'topright' )))
                        ),
								        div(class='row-fluid', 
								            div(class='span8',checkboxInput("legend_ext", "Show error estimate legend", FALSE)),
								            div(class='span4',conditionalPanel( condition = "input.legend_ext == true", selectInput("legend_ext_pos", "-> position:", c("bottomright", "bottom", "bottomleft", "left", "topleft", "top", "topright", "right", "center"),  "topleft" )))
								        ),
												sliderInput("legend_font_size", "Legend font size:", 0.5, 10, 1.5, 0.5, ticks = TRUE, animate = TRUE)
								),
					#5) Subplot options/Labels and colours               
					tabPanel(value = 'panel5', title=tags$i(class="icon-list-ol icon-large icon-blcak", 'data-placement'="right", 'data-toggle'="tooltip", title="Labels and colours"), #"Subplots",
					         checkboxGroupInput('subplot_options', 'Set sub-plot specific:', c('Colors'='color', 'Label'='label', 'Priority/Order'='prior'), selected = NULL)
					),
					#6) HEATMAP SPECIFIC OPTIONS
								tabPanel(value = 'panel6', title=tags$i(class="icon-th icon-large icon-blcak", 'data-placement'="right", 'data-toggle'="tooltip", title="Heatmap setup"), #("Sizes", 
			
									checkboxInput("img_heatmap", "Preview heatmap [Ctrl+H]"), 
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
									checkboxInput('heat_include', 'Exclude individual heatmaps from sorting/clustering', FALSE),
                  tags$br(),
                  
									checkboxInput("indi", "Heatmaps have individual color keys", TRUE),
									checkboxInput("heatmapzauto", "Set default color key limits", FALSE),
									conditionalPanel( condition = "input.heatmapzauto == false",
									  div(class='row-fluid',
									                      div(class='span5', tags$br(), "Color key saturation:"),
									                      div(class='span7', sliderInput("hsccoef", "Color key saturation:", 0, 0.1, 0.01, NULL, ticks = TRUE, animate = TRUE)									                      )
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
									                      div(class='span4', HTML('Min: <input type="color" class="color {hash:true}" ="heat_csp_min" value="#FFFFFF" style="width:40px;" title=""/>')),
									                      div(class='span4', HTML('Mid: <input type="color" class="color {hash:true}" id="heat_csp_mid" value="#87CEFA" style="width:40px;" title=""/>')),
									                      div(class='span4', HTML('Max: <input type="color" class="color {hash:true}" id="heat_csp_max" value="#00008B" style="width:40px;" title=""/>'))
									                  )
									)
									
								),
		
					#7) BATCH
						    tabPanel(tags$i(class="icon-gears icon-large icon-blcak", 'data-placement'="right", 'data-toggle'="tooltip", title="Batch operations and setup"), #"Batch", 
						            
						      div(class='form-inline', 
						        numericInput("pdf_x_size", "PDF output size: ", 16) ,  
						        numericInput("pdf_y_size", " x ", 10) 
						      ),      
                  tags$hr(),     
						      checkboxInput('recordHistory', 'Record plot history', FALSE),      
						      downloadLink('downloadHistory',   tags$span(tags$i(class="icon-time icon-large"), 'Get plot history'),   class="btn btn-small btn-success"),
                  tags$hr(),
						      div(class='form-inline', 
                    selectInput('batch_what', 'Plot', c('lineplots', 'heatmaps') ),  
                    selectInput('batch_how', 'by', c('rows', 'columns', 'single') )                                
						      ),
						      div(class='form-inline', 
						          numericInput("grid_x_size", "Multi-plot grid: ", 1) ,  
						          numericInput("grid_y_size", " x ", 1) 
						      ), 
                  textInput('multi_name_flt', 'Filter names'),
						      downloadLink('downloadBatchColLineplot', tags$span(tags$i(class="icon-align-justify icon-rotate-90"), tags$i(class="icon-double-angle-right icon-large"),  tags$i(class="icon-picture icon-large icon-white"), 'Get PDF'),   class="btn btn-small btn-success"),
                  tags$hr(),
						      #checkboxInput('setup_multithread', 'Use multithreading for calculations', (Sys.getenv("SHINY_SERVER_VERSION") != ''))
						      conditionalPanel( condition = tolower(as.character(Sys.getenv("SHINY_SERVER_VERSION") == '')),
						        checkboxInput('setup_multithread', 'Use multithreading for calculations', .Platform$OS.type != 'windows')
						      )
						                     
						    )

						),
				    div( class='hidden', textInput('clusters', 'Clusters'), textInput('sortingord', 'Sorting'), textInput('finalord', 'Sorting') )
				),
				mainPanel(
					uiOutput('reactiveScripts')
					,singleton(tags$script(src = "upload/js/vendor/jquery.ui.widget.js"))
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
          #,div(HTML(' <input type="text" id="prr" style="width:40px" placeholder="Prior" value=0 /> '), class="zezol", title='Something')
					,div( tags$hr(),textInput("caption", "EVAL!:", ""),
					      textInput("tt1", "tt1", ""),
					      
                verbatimTextOutput("summary"), actionButton('ab1', 'Test'),

                
          class='hidden', id='debug')
#					,verbatimTextOutput("timer")
#					actionButton('parast', 'PS'), ,
#					fileInput('file1', 'Choose File')
				)
		))
