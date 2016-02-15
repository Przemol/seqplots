#File management modal
tagList(
    singleton(tags$head(tags$script(src = "js/modal_events.js"))),	
    div(id="calcModal", class="modal fade", tabindex="-1", role="dialog", div(class="modal-dialog", div(
        class="modal-content", div( 
            class="modal-body",
            tags$p(tags$span(class="label label-info", 'Info'), 'Choose file by clicking on file name.  Chosen files will be highlighted. Click file name again to cancel choice. At least one signal track or motif and one feature file must be selected.'),
            tabsetPanel(
                tabPanel(
                    "Tracks",   
                    div(id='TrackSelCtl', 
                        ' | ', textOutput('nselected', inline = TRUE),  ' | ', 
                        actionButton('selFilt', 'Select filtered'),
                        actionButton('selPage', 'Add visible on page'),
                        actionButton('selNone', 'Select none')
                    ),
                    
                    DT::dataTableOutput('trackDT') 
                ),
                tabPanel(
                    "Features", 
                    div(id='FeatureSelCtl', 
                        ' | ', textOutput('nselectedFT', inline = TRUE),  ' | ', 
                        actionButton('selFiltFT', 'Select filtered'),
                        actionButton('selPageFT', 'Add visible on page'),
                        actionButton('selNoneFT', 'Select none')
                    ),
                    
                    DT::dataTableOutput('featureDT')
                    ),
                tabPanel("Sequence features", tags$br(), div(class='row', div(class='col-md-4', wellPanel(
                    class='SFform', 
                    #selectInput("SFgenome", "Reference sequence (genmoe)", GENOMES),
                    textInput(inputId='SFpattern', label='DNA motif'),
                    numericInput(inputId='SFbin', label='Sliding window size in base pairs [bp]', value=200, min=10, step=10),
                    textInput(inputId='SFname', label='Display name'),
                    checkboxInput(inputId='SFadvanced', label="Plot heatmap or error estimates", value=TRUE),
                    checkboxInput(inputId='SFrevcomp', label="Match reverse complement as well", value=FALSE),
                    actionButton('SFadd', 'Add'), actionButton('SFreset', 'Reset All'))),
                    div(class='col-md-7', style='height:450px; overflow:auto;', 'Motifs to add:', verbatimTextOutput("SFsetup") )
                ))
            )
        ),
        div( class="modal-footer", div(
            id='modalToolbar', class='row',
            div(class="col-md-2", 
                numericInput("BWbin", "Bin track @ [bp]: ", 10),
                radioButtons('stat', 'Statistic:', c('mean', 'median'), 'mean')
            ),
            div(class="col-md-2", radioButtons('plot_type', 'Choose the plot type', c( 'Point Features', 'Midpoint Features', 'Endpoint Features', 'Anchored Features' ) ) ),
            div(class="col-md-2", p("Additional options:"), 
                #conditionalPanel( condition = "input.plot_type != 'Anchored Features'", 
                checkboxInput("ignore_strand", "Ignore strand", FALSE),
                #), 
                checkboxInput("rm0", "Remove zeros", FALSE),
                checkboxInput("add_heatmap", "Calculate Heatmap", TRUE)
            ),
            div(class="col-md-1", style="text-align:right;", 'Plotting distances in [bp]:'),
            div(class="col-md-1", numericInput("plot_upstream", "Upstream:", 1000)),
            div(class="col-md-1" ,conditionalPanel( condition = "input.plot_type == 'Anchored Features'", id='anchoredHidabeDiv', numericInput("anchored_downstream", "Anchored:", 1000)) ),
            div(class="col-md-1", numericInput("plot_downstream", "Downstream:", 1000))
        ),
        div(class="pull-right", style="margin-top:25px",
            tags$button(tags$span(tags$i(class="icon-off"), "Close"),	class="btn", 'data-dismiss'="modal"),
            tags$button(tags$span(tags$i(class="icon-refresh icon-white"), 'Refresh'),  class="btn action-button btn-success", id="reloadgrid"),
            #actionButton(inputId = 'rmfiles', label = 'Remove selected files', icon = icon('trash'), class='btn-danger' ),
            
            tags$button(tags$span(tags$i(class="icon-trash icon-white"), 'Remove selected files'), 	class='btn btn-danger', onClick='rmSelctedFiles()'),
            #tags$button(tags$span(tags$i(class="icon-play icon-white"), 'Run calculation'), 			class='btn btn-primary', onClick='sendToCalc()'	)
            actionButton(inputId = 'runcalc', label = 'Run calculation', icon = icon('play'), class='btn-primary' )
        )
        
        )
    )))
)
