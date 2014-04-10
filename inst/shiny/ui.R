# Author: przemol
###############################################################################

# actionButton <- function(inputId, label) {
# 	tagList(
# 			singleton(tags$head(tags$script(src = 'js/actionbutton.js'))),
# 			tags$button(id=inputId, type="button", class="btn action-button", label)
# 	)
# }
#source('functions/files_modal.R')
GENOMES <- BSgenome:::installed.genomes(splitNameParts=TRUE)$provider_version
names(GENOMES) <- gsub('^BSgenome.', '', BSgenome:::installed.genomes())
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
						singleton(tags$link(rel="stylesheet", type="text/css", href="css/bootstrapSwitch.css")),
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
						#singleton(tags$script(src = "js/bootstrapSwitch.js")),
						singleton(tags$script(src = "js/tmpl.min.js")),
						singleton(tags$script(src = "js/TableTools.min.js")),
						singleton(tags$script(src = "js/dataTables.scroller.min.js")),
						
						
						
						# Title
						tags$title('GFPlot')
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
				tags$div(id="letter-container", class="letter-container", HTML('<h2><a href="#">GFplots</a></h2>')), # v1.0b
				
				#Dynamically injected scripts output
				uiOutput('scripts'),
				
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
				                                                       #selectInput(inputId='SFgenome', label='Reference sequence', choices=''),
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
				                   selectInput('algo_type', 'Choose method', c( 'Quick [track-at-once]', 'Iterative [loci-by-chromosome]' ) ), 
				                   conditionalPanel( class="form-inline", condition = "input.algo_type == 'Quick [track-at-once]'", numericInput("BWbin", "Bin track @ [bp]: ", 10))
				               ),
				               div(class="span2", radioButtons('plot_type', 'Choose the plot type', c( 'Point Features', 'Midpoint Features', 'Anchored Features' ) ) ),
				               div(class="span2", p("Additional options:"), 
				                   #conditionalPanel( condition = "input.plot_type != 'Anchored Features'", 
				                   checkboxInput("ignore_strand", "Ignore strand", FALSE),
				                   #), 
				                   checkboxInput("rm0", "Remove zeros", FALSE),
				                   checkboxInput("add_heatmap", "Calculate Heatmap", FALSE)
				               ),
				               div(class="span1", style="text-align:right;", 'Plotting distances in [bp]:'),
				               div(class="span1", numericInput("plot_upstream", "Upstream:", 1000)),
				               div(class="span1" ,conditionalPanel( condition = "input.plot_type == 'Anchored Features'", id='anchoredHidabeDiv', numericInput("anchored_downstream", "Anchored:", 1000)) ),
				               div(class="span1", numericInput("plot_downstream", "Downstream:", 1000))
				           ),
				           div( class="btn-toolbar",
				                tags$button(tags$span(tags$i(class="icon-off"), "Close"),	class="btn", 'data-dismiss'="modal"),
				                tags$button(tags$span(tags$i(class="icon-refresh icon-white"), 'Refresh'),  class="btn action-button btn-success", id="reloadgrid"),
				                #tags$button(tags$span(tags$i(class="icon-trash icon-white"), 'Remove selected files'), 	class='btn btn-danger', onClick='rmSelctedFiles()'),
				                tags$button(tags$span(tags$i(class="icon-play icon-white"), 'Run calculation'), 			class='btn btn-primary', onClick='sendToCalc()'	)
				           )
				           
				      )
				  )
				),
				
				#Sidebar panel definitions
				sidebarPanel(
				  
					conditionalPanel(condition = "output.showplot",
						p("Get:",
								downloadLink('downloadPlot', 	tags$span(tags$i(class="icon-bar-chart icon-large icon-white"), 'Plot'),   class="btn btn-small btn-success"),
								downloadLink('downloadLegend', tags$span(tags$i(class="icon-tag icon-white"), 'Legend'), class="btn btn-small btn-info"),
								downloadLink('downloadHeatmap', tags$span(tags$i(class="icon-fire icon-white"), 'Heatmap'), class="btn btn-small btn-inverse"),
						    downloadLink('downloadClusters', tags$span(tags$i(class="icon-fire icon-white"), 'Clst'), class="btn btn-small btn-inverse"),
						    div(class="hidden", actionButton('plotHmap', 'Plot')),
                div(class="img hidden", plotOutput(outputId = "plot", width = "1240px", height = "720px") ),
						    div(class="img", imageOutput(outputId = "image", width = "1240px", height = "720px") ),
								div(class='form-inline', 
                    checkboxInput("cust_col", "Colors"), HTML(' &#8226; '), 
										checkboxInput("img_heatmap", "Heatmap"), HTML(' &#8226; '), 
										numericInput("img_clusters", "k =", 5, min=1), HTML(' &#8226; '),
										checkboxInput("img_sort", "Sort")
								)
						)
					),
					div(class='row-fluid', div(class='span7',conditionalPanel(condition = "!input.reactive", actionButton('replot', 'Refresh/Replot'))),
					    div(class='span5',checkboxInput("reactive", "Reactive plotting", TRUE), tags$br())
					),
						tabsetPanel(id='ctltabs',
								tabPanel("New" 
										
										,h5('Upload files:')
										,helpText( "This panel is used to add track (BigWig, Wiggle or BedGraph) and feature (GFF and BED) files to file collection.
												  	Please provide your user ID (initials, eg JS fot John Smith) and genome version in UCSC format (e.g. 'ce10', 'mm9', 'hg19').
													You can drag-and-drop the files to browser window. Comments are optional.") # TIP: You can add multiple files at once.
		  								,HTML('<a href="#fileUploadModal" role="button" class="btn btn-success" data-toggle="modal"><i class="icon-cloud-upload icon-large icon-white"></i> Add files</a>')
                      ,conditionalPanel("false", selectInput("file_genome", "Genmoe:", GENOMES))
# 										,HTML(' Upload form: <div class="switch" id="old_style_switch"><input type="checkbox"></div>')
# 										#,checkboxInput('old_style', '')						
# 										,div(class="alert alert-info", style="margin-top:10px; display:none;", id='oldff'
# 											#
# 											#,conditionalPanel("true", textInput("file_genome", "Genmoe (e.g. 'ce10', 'mm9', 'hg19'):", ""))
# 											,conditionalPanel("true", selectInput("file_genome", "Genmoe:", GENOMES))
# 											,conditionalPanel("input.file_genome !== ''", textInput("file_user", "User name", ""))
# 											,conditionalPanel("input.file_user !== ''", conditionalPanel("! input.file_comment_skip", textInput("file_comment", "Comments", "")), checkboxInput('file_comment_skip', "Skip comments", TRUE))
# 											,conditionalPanel("((input.file_comment !== '') | (input.file_comment_skip)) & input.file_user !== '' & input.file_genome !== '' & input.file_type !== 'none'", 
# 													fileInput("files", "", multiple=FALSE) #Server
# 											#actionButton("upload", "Choose a file with GUI") #Local
# 											)
# 											
# #											,HTML('	<div id="fileprogressdiv" style="display: none;"> <lable>File upload/conversion progress:</label> <div class="progress progress-striped active">
# #															<div class="bar" style="width: 0%;"></div>
# #															</div></div>')
# #											,tableOutput("filetable"), uiOutput('GUIfile')
# 											,conditionalPanel("true", tags$button(id = 'fileFormClearBtn', class = "btn btn-warning", type = "button", tags$i(class="icon-remove-sign icon-white"), 'Reset file form.'))
# 										)
										,uiOutput('FileUploadScripts'), tags$hr()
										,h5('Create new plot array:')
										,helpText( "This panel allows to calculate new plot array. Each track will be summarized using each feature file.
													Subsequently the plot arry will be shown to indicate which pairs should be plotted.")
										,a(href="#myModal", role="button", class="btn btn-primary", 'data-toggle'="modal", tags$i(class="icon-rocket icon-large icon-white"), "New plot set")
										,uiOutput('plot_message')
                    ,tags$hr()
                         
				
								),
								tabPanel("Saved",										
										#uiOutput('GUIloadRdata'),
								    selectInput('publicRdata', 'Load public file:', ' ', ' '),
										conditionalPanel("input.publicRdata !== ' '", actionButton('RdataRemoveButton', 'Remove this dataset') ),
											uiOutput('GUIrmRdata'),
										tags$hr(),
										conditionalPanel("input.publicRdata == ' ' & output.showsaveGUI", 
											textInput('RdataSaveName', 'Save current dataset name:', ''), 
											conditionalPanel("input.RdataSaveName !== ''", actionButton('RdataSaveButton', 'Save') ) ),
											uiOutput('GUIsaveRdata')
								),
								tabPanel("Axis", 
									conditionalPanel(condition = "output.showplot", 										
										div(class='row-fluid', 
												div(class='span4', textInput("title", "Title:", "")),
												div(class='span4', textInput("xlabel", "X-axis label:", "")),
												div(class='span4', textInput("ylabel", "Y-axis label:", ""))
										),
										uiOutput("plotUI"),
									                 
										checkboxInput("yauto", "Y-axis/Heatmap colors autoscale", TRUE),
										conditionalPanel( condition = "input.yauto != true",
											p(numericInput("ymin1", "Y-axis limits:", -1), numericInput("ymin2", "-", 10))
										),
									  conditionalPanel( condition = "input.yauto == true",
									    sliderInput("hsccoef", "Heatmap color scaling coefficient:", 0, 0.1, 0.01, NULL, ticks = TRUE, animate = TRUE)
									  ),
									  checkboxInput("indi", "Independent color scaling for heatmaps", FALSE)
									)
								),
								tabPanel("Setup", 
										conditionalPanel(condition = "output.showplot",
												selectInput('scale_signal', 'Transform signal:', c( 'Do not transform', 'Log2 transform')), #, 'Z-score transform')),
												checkboxInput("lnv", "Show vertical guiding line", TRUE),
												checkboxInput("lnh", "Show horizontal guiding line", FALSE),
												conditionalPanel( condition = "input.lnh == true",
														p( numericInput("lnh_pos", "Position of h-line:", 0) )
												),
												checkboxInput("ee", "Show error estimates", TRUE),
												checkboxInput("legend", "Show plot legend", TRUE),
										    HTML('<span class="label label-info " >Heatmap setup</span>'),
										    div(class="alert alert-info text-center",
										           selectInput(inputId='img_ch', label='Choose sub-plot:', ''),
										           textInput(inputId='img_lab', label='Sub-plot label:', ''),
										           numericInput(inputId='img_prior', label='Sub-plot priority:', 0),
										           checkboxInput(inputId='img_include', label='Include for sorting/clustering', TRUE),
										           numericInput(inputId='img_o_min', label='Sub-plot override min value [Independent color scaling ONLY]:', NA),
										           numericInput(inputId='img_o_max', label='Sub-plot override max value [Independent color scaling ONLY]:', NA)
										     )
            
										)
								),
								tabPanel("Sizes", 
									conditionalPanel(condition = "output.showplot", 
										sliderInput("title_font_size", "Title font size:", 0.1, 10, 3.0, 0.1, ticks = TRUE, animate = TRUE),
										sliderInput("labels_font_size", "Labels font size:", 0.1, 10, 2.0, 0.05, ticks = TRUE, animate = TRUE),
										sliderInput("axis_font_size", "Axis font size:", 0.1, 5, 2, 0.05, ticks = TRUE, animate = TRUE),
										sliderInput("legend_font_size", "Legend font size:", 0.1, 5, 1.5, 0.05, ticks = TRUE, animate = TRUE),
										numericInput("pdf_x_size", "PDF X size:", 16),
										numericInput("pdf_y_size", "PDF Y size:", 10)
									)
								)
						),
				    div(class='hidden', textInput('clusters', 'Clusters'))
				),
				mainPanel(
					uiOutput('reactiveScripts'),
					tags$br(),tags$br(),tags$br(),tags$br(),
					div(class="control-group", uiOutput("htmltab") )
					,singleton(tags$script(src = "http://blueimp.github.com/JavaScript-Templates/tmpl.min.js"))
					,singleton(tags$script(src = "upload/js/vendor/jquery.ui.widget.js"))
					,singleton(tags$script(src = "upload/js/jquery.iframe-transport.js"))
					,singleton(tags$script(src = "upload/js/jquery.fileupload.js"))		
					,singleton(tags$script(src = "upload/js/jquery.fileupload-fp.js"))
					,singleton(tags$script(src = "upload/js/jquery.fileupload-ui.js"))
					,singleton(tags$script(src = "upload/js/md5.js"))
					,singleton(tags$script(src = "upload/js/main.js"))
					,singleton(tags$link(rel="stylesheet", type="text/css", href="upload/css/jquery.fileupload-ui.css"))
					,includeHTML('www/upload/upload.html')
			
					#Debug code
					,div( tags$hr(),textInput("caption", "EVAL!:", ""),
					      textInput("tt1", "tt1", ""),
                verbatimTextOutput("summary"), actionButton('ab1', 'Test'),
          class='hidden', id='debug')
#					,verbatimTextOutput("timer")
#					actionButton('parast', 'PS'), ,
#					fileInput('file1', 'Choose File')
				)
		))
