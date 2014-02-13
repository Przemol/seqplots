###############################################################################
#
# Author: Przemyslaw Stempor
###############################################################################

actionButton <- function(inputId, label) {
	tagList(
			singleton(tags$head(tags$script(src = 'js/actionbutton.js'))),
			tags$button(id=inputId, type="button", class="btn action-button", label)
	)
}

files_modal <- function() {
	tagList(
		singleton(tags$head(tags$script(src = "js/modal_events.js"))),	
		div(id="myModal", class="modal container hide fade", tabindex="-1", role="dialog", 'aria-labelledby'="myModalLabel", 'aria-hidden'="true",
# 			div( class="modal-header", 
# 				tags$button( type="button", class="close", 'data-dismiss'="modal", 'aria-hidden'="true",'x'),
# 				tags$div(class="navbar-form pull-right input-prepend", style="margin-right:10px", checkboxInput("rendergrid", "Render grid/table", TRUE) ),
# 				tags$h3('New plots set:')
# #				tags$h3(id="myModalLabel", HTML('New plots set:
# #					<div class="input-prepend pull-right">
# #						
# #						<label class="checkbox inline" style="margin-left:10px;margin-right:10px;"> Render grid/table <input id="rendergrid" type="checkbox" checked="checked"/></label>
# #					</div>'))
# 				
# 			),
					div( class="modal-body",
							tabsetPanel(
									tabPanel("Tracks", 		div(class='fileMoodalInnerDiv', div("Loading...",id="tracktable")	)),
									tabPanel("Features", 	div(class='fileMoodalInnerDiv', div("Loading...",id="featuretable")	)),
									tabPanel("Sequence features",   wellPanel(class='SFform', 
									    selectInput("SFgenome", "Reference sequence (genmoe)", GENOMES),
                      #selectInput(inputId='SFgenome', label='Reference sequence', choices=''),
                      textInput(inputId='SFpattern', label='DNA motif'),
                      numericInput(inputId='SFbin', label='Sliding window size in base pairs [bp]', value=200, min=10, step=10),
									    textInput(inputId='SFname', label='Display name'),
                      checkboxInput(inputId='SFadvanced', label="Plot heatmap or error estimates", value=TRUE)
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
	)
}

#<span class="add-on"><i class="icon-search"></i></span>
#<input type="text" id="filter_all" class="" placeholder="Search..."/>