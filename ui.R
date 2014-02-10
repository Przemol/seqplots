# Author: przemol
###############################################################################

# require(rCharts)
# options(RCHART_LIB = 'nvd3')


# Define UI
shinyUI(
		bootstrapPage(
				
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
						
						
						# Title
						tags$title('SeqPlots')
				),
				
				#Page loading modal and mask, pops out just after page loads, removed after all JS loads, stays on JQuery error
				div(HTML('<div id="load_modal" class="modal" data-backdrop="static" data-show="true" tabindex="-1" role="dialog" aria-labelledby="myModalLabel" aria-hidden="false">
							<div class="modal-header">
								<h3 id="myModalLabel">Loading...</h3>
							</div>
							<div class="modal-body">
								<div class="progress progress-striped active">
									<div class="bar" style="width: 100%;"></div>
								</div>
							</div>
							<div class="modal-footer">
								<a href="." class="btn btn-primary">Reload</a>
							</div>
						</div>'), class='load_div'),

				#Calculation progress modal
				div(id="progressModal", class="modal hide", 'data-backdrop'="false", 'data-keyboard'="false", tabindex=-1,
					div(class="modal-header", tags$h3(id="progressModalLabel", 'Calculating...')),
					div(class="modal-body", verbatimTextOutput("summary2"), verbatimTextOutput("summary3"), actionButton('cancel', 'Cancel'))
				),
				
				#Animated header
				#tags$div(id="letter-container", class="letter-container", HTML('<h2><a href="#">GFplots</a></h2>')), # v1.0b
				tags$div(id="letter-container", class="letter-container", HTML('<h2><a href="#">SeqPlots</a></h2>')), # v1.0b
				
				#Banner
									HTML('<div class="row" style="margin-left:0px;"><div style="width:318px" class="alert alert-info alert-block span4">
											<button type="button" class="close" data-dismiss="alert">&times;</button>
  											<h4>Heads up!</h4>
  											The desktop version of SeqPlot is available. Easy installation, no R or terminal. <br />Download link:
  											<strong><a href="SeqPlots.dmg" target="_blank">SeqPlots.dmg</a></strong>
									</div></div>'),
			
				
				
				#Dynamically injected scripts output
				#uiOutput('scripts'),
				
				#File management modal
				tagList(
				  singleton(tags$head(tags$script(src = "js/modal_events.js"))),	
				  div(id="myModal", class="modal container hide fade", tabindex="-1", role="dialog", 'aria-labelledby'="myModalLabel", 'aria-hidden'="true",
				      div( class="modal-body",
				           tabsetPanel(
				             tabPanel("Tracks", 		div(class='fileMoodalInnerDiv', div("Loading...",id="tracktable")	)),
				             tabPanel("Features", 	div(class='fileMoodalInnerDiv', div("Loading...",id="featuretable")	)),
				             tabPanel("Sequence features",   div(class='row', div(class='span4', wellPanel(class='SFform', 
				                                                       selectInput("SFgenome", "Reference sequence (genmoe)", GENOMES),
				                                                       textInput(inputId='SFpattern', label='DNA motif'),
				                                                       numericInput(inputId='SFbin', label='Sliding window size in base pairs [bp]', value=200, min=10, step=10),
				                                                       textInput(inputId='SFname', label='Display name'),
				                                                       checkboxInput(inputId='SFadvanced', label="Plot heatmap or error estimates", value=TRUE),
				                                                       checkboxInput(inputId='SFrevcomp', label="Match reverse complement as well", value=FALSE),
                                                               actionButton('SFadd', 'Add'), actionButton('SFreset', 'Reset All'))),
				                                                       div(class='span7', style='height:450px; overflow:auto;', 'Motifs to add:', verbatimTextOutput("SFsetup") )
				                                                       
				                                                       
				             ))
				           )
				      ),
				      div( class="modal-footer",
				           div(id='modalToolbar', class='row-fluid',
				               div(class="span2", 
                          numericInput("BWbin", "Bin track @ [bp]: ", 10)
				               ),
				               div(class="span2", radioButtons('plot_type', 'Choose the plot type', c( 'Point Features', 'Midpoint Features', 'Anchored Features' ) ) ),
				               div(class="span2", p("Additional options:"), 
				                   #conditionalPanel( condition = "input.plot_type != 'Anchored Features'", 
				                   checkboxInput("ignore_strand", "Ignore strand", FALSE),
				                   #), 
				                   checkboxInput("rm0", "Remove zeros", FALSE),
				                   checkboxInput("add_heatmap", "Calculate Heatmap", TRUE)
				               ),
				               div(class="span1", style="text-align:right;", 'Plotting distances in [bp]:'),
				               div(class="span1", numericInput("plot_upstream", "Upstream:", 1000)),
				               div(class="span1" ,conditionalPanel( condition = "input.plot_type == 'Anchored Features'", id='anchoredHidabeDiv', numericInput("anchored_downstream", "Anchored:", 1000)) ),
				               div(class="span1", numericInput("plot_downstream", "Downstream:", 1000))
				           ),
				           div( class="btn-toolbar",
				                tags$button(tags$span(tags$i(class="icon-off"), "Close"),	class="btn", 'data-dismiss'="modal"),
				                tags$button(tags$span(tags$i(class="icon-refresh icon-white"), 'Refresh'),  class="btn action-button btn-success", id="reloadgrid"),
				                tags$button(tags$span(tags$i(class="icon-trash icon-white"), 'Remove selected files'), 	class='btn btn-danger', onClick='rmSelctedFiles()'),
				                tags$button(tags$span(tags$i(class="icon-play icon-white"), 'Run calculation'), 			class='btn btn-primary', onClick='sendToCalc()'	)
				           )
				           
				      )
				  )
				),
				
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
						    div(class="hidden", actionButton('plotHmap', 'Plot')),
                div(class="img hidden", plotOutput(outputId = "plot", width = "1240px", height = "720px") ),
						    div(class="img", imageOutput(outputId = "image", width = "1240px", height = "720px") ),
								div(class='form-inline', 
                    checkboxInput("cust_col", "Colors"), HTML(' &#8226; '), 
										checkboxInput("img_heatmap", "Heatmap"), 
								    conditionalPanel(condition = "input.img_heatmap", style="display: inline; margin-top",           
                      HTML(' &#8226; '),numericInput("img_clusters", "k =", 5, min=1), HTML(' &#8226; '),
										  checkboxInput("img_sort", "Sort")
								    )
								)
						)
					),
					div(class='row-fluid', div(class='span6',conditionalPanel(condition = "!input.reactive", actionButton('replot', tags$span(tags$i(class="icon-refresh icon-large"), HTML('Replot [&crarr;]') )))),
					    div(class='span6',checkboxInput("reactive", "Reactive plotting [F5]", FALSE)), tags$hr()
					),
					
					tabsetPanel(id='ctltabs',
					#1) NEW PLOT SET PANEL
								tabPanel(value = 'panel1', title=tags$i(class="icon-rocket icon-large icon-blcak", 'data-placement'="right", 'data-toggle'="tooltip", title="New plotset/Upload new files") #, "New"
										,h5('Upload files:')
										,helpText( "This panel is used to add track (BigWig, Wiggle or BedGraph) and feature (GFF and BED) files to file collection.
												  	Please provide your user ID (initials, eg JS fot John Smith) and genome specify version.
													You can drag-and-drop the files to browser window. Comments are optional.") # TIP: You can add multiple files at once.
		  								,HTML('<a href="#fileUploadModal" role="button" class="btn btn-success" data-toggle="modal"><i class="icon-cloud-upload icon-large icon-white"></i> Add files</a>')
                      ,conditionalPanel("false", selectInput("file_genome", "Genmoe:", GENOMES)) #This should stay for conning, unless I can figure out something better using JS
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
										conditionalPanel("input.publicRdata !== ' '", actionButton('RdataRemoveButton', 'Remove this dataset') ),
										tags$hr(),
										textInput('RdataSaveName', 'Save current plot set as:', ''), 
										conditionalPanel("input.RdataSaveName !== ''", actionButton('RdataSaveButton', 'Save') ) 
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
					  #4) Legands, guiding lines, and data scaling
								tabPanel(value = 'panel4', title=tags$i(class="icon-tags icon-large icon-blcak", 'data-placement'="right", 'data-toggle'="tooltip", title="Legands, guiding lines, and data scaling"),# "Setup", 
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
            
										#)
								),
					#5) HEATMAP SPECIFIC OPTIONS
								tabPanel(value = 'panel5', title=tags$i(class="icon-th icon-large icon-blcak", 'data-placement'="right", 'data-toggle'="tooltip", title="Heatmap setup"), #("Sizes", 
									#conditionalPanel(condition = "output.showplot", 
										
									checkboxInput("heatmapzauto", "Set heatmap colors limits", FALSE),
									conditionalPanel( condition = "input.heatmapzauto == true",
									    p( numericInput("zmin1", "-> ", -1), numericInput("zmin2", "-", 10) )
									),
									conditionalPanel( condition = "input.heatmapzauto == false",
									    sliderInput("hsccoef", "Heatmap color scaling coefficient:", 0, 0.1, 0.01, NULL, ticks = TRUE, animate = TRUE)
									)
				                               
									
										
										
									#)
								),
					#6) Subplot options               
						    tabPanel(value = 'panel6', title=tags$i(class="icon-list-ol icon-large icon-blcak", 'data-placement'="right", 'data-toggle'="tooltip", title="Subplot options"), #"Subplots", 
						        selectInput(inputId='img_ch', label='Choose sub-plot:', ''),
						        textInput(inputId='img_lab', label='Sub-plot label:', ''),
						        numericInput(inputId='img_prior', label='Sub-plot priority:', 0),
						        HTML('<br /><span class="label label-info " >Heatmap setup</span>'),
						        div(class="alert alert-info text-center",
						          checkboxInput("indi", "Independent color scaling for heatmaps", FALSE),
						          checkboxInput(inputId='img_include', label='Include for sorting/clustering', TRUE),
						          conditionalPanel(condition = "input.indi",
						            numericInput(inputId='img_o_min', label='Sub-plot override min value:', NA),
						            numericInput(inputId='img_o_max', label='Sub-plot override max value:', NA)
						          )
						        )
						    ),
					#7) BATCH
						    tabPanel(tags$i(class="icon-gears icon-large icon-blcak", 'data-placement'="right", 'data-toggle'="tooltip", title="Batch operations"), #"Batch", 
						            
						      div(class='form-inline', 
						        numericInput("pdf_x_size", "PDF output size:", 16) ,  
						        numericInput("pdf_y_size", "x", 10) 
						      ),      
                  tags$hr(),     
						      checkboxInput('recordHistory', 'Record plot history', FALSE),      
						      downloadLink('downloadHistory',   tags$span(tags$i(class="icon-time icon-large"), 'Get plot history'),   class="btn btn-small btn-success"),
                  tags$hr(),
						      div(class='form-inline', 
                    selectInput('batch_what', 'Plot', c('lineplots', 'heatmaps') ),  
                    selectInput('batch_how', 'by', c('rows', 'columns', 'single') )                                
						      ),
						      downloadLink('downloadBatchColLineplot', tags$span(tags$i(class="icon-align-justify icon-rotate-90"), tags$i(class="icon-double-angle-right icon-large"),  tags$i(class="icon-picture icon-large icon-white"), 'Get PDF'),   class="btn btn-small btn-success")  
						                     
						    )

						),
				    div(class='hidden', textInput('clusters', 'Clusters'))
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
					,includeHTML('www/upload/upload.html'),
          
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
					,div( tags$hr(),textInput("caption", "EVAL!:", ""),
					      textInput("tt1", "tt1", ""),
                verbatimTextOutput("summary"), actionButton('ab1', 'Test'),
					      radioButtons("dist", "Distribution type:",
					                   c("Normal" = "norm",
					                     "Uniform" = "unif",
					                     "Log-normal" = "lnorm",
					                     "Exponential" = "exp")),
                
          class='hidden', id='debug')
#					,verbatimTextOutput("timer")
#					actionButton('parast', 'PS'), ,
#					fileInput('file1', 'Choose File')
				)
		))
