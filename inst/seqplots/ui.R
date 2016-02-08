# FUNCTION DEF #################################################################

# require(rCharts)
# options(RCHART_LIB = 'nvd3')

hlp <- function(target, top=-10) {
  return(tags$a(class='pull-right', href=paste0("help/help.html#", target), target="_blank", 
    tags$i(class="icon-question-sign icon-2x", style=sprintf('color:orange; margin-right:5px; position: relative; top: %ipx;', top))
  ))
}


# HEAD #########################################################################

head <- tags$head(
    # JS error message
    singleton(tags$script('var error = false; window.onerror =  function() { if (!error) {alert("JavaScript error! Some elements might not work proprely. Please reload the page."); error=true;} }')),
    
    # CSS impprt						
    # singleton(tags$link(rel="stylesheet", type="text/css", href="http://netdna.bootstrapcdn.com/bootstrap/3.0.0/css/bootstrap.min.css")),
    singleton(tags$link(rel="stylesheet", type="text/css", href="css/style.css")),
    #singleton(tags$link(rel="stylesheet", type="text/css", href="css/DT_bootstrap.css")),
    #singleton(tags$link(rel="stylesheet", type="text/css", href="http://cdn.datatables.net/plug-ins/28e7751dbec/integration/bootstrap/2/dataTables.bootstrap.css")),
    #singleton(tags$link(rel="stylesheet", type="text/css", href="css/TableTools.css")),	
    singleton(tags$link(rel="stylesheet", type="text/css", href="css/TableTools.css")),
    
    singleton(tags$link(rel="stylesheet", type="text/css", href="css/font-awesome.min.css")),
    singleton(tags$link(rel="stylesheet", type="text/css", href="upload/css/jquery.fileupload-ui.css")),
    
    # JS import
    ## miscellaneous libraries
    singleton(tags$script(src = "js/jquery.ba-throttle-debounce.js")),
    singleton(tags$script(src = "js/jquery.lettering.js")),
    singleton(tags$script(src = "js/tmpl.min.js")),
    singleton(tags$script(src = "js/jquery.cookie.js")),
    singleton(tags$script(src = "color/jscolor.js")),
    ## DataTable libraries
    #singleton(tags$script(src = "js/DataTables/jquery.dataTables.js")),
    #singleton(tags$script(src = "js/DataTables/DT_bootstrap.js")),
    #singleton(tags$script(src = "js/DataTables/dataTables.tableTools.min.js")),
    #singleton(tags$script(src = "js/DataTables/DT_filter.js")),
    
    
    ## My scripts
    singleton(tags$script(src = "js/color.js")),
    singleton(tags$script(src = "js/js_addons.js")),
    singleton(tags$script(src = "js/load_finished.js")),
    
    singleton(tags$script(src = "upload/js/vendor/jquery.ui.widget.js"))
    ,singleton(tags$script(src = "upload/js/jquery.iframe-transport.js"))
    ,singleton(tags$script(src = "upload/js/jquery.fileupload.js"))      
    ,singleton(tags$script(src = "upload/js/jquery.fileupload-fp.js"))
    ,singleton(tags$script(src = "upload/js/jquery.fileupload-ui.js"))
    ,singleton(tags$script(src = "upload/js/jquery.fileupload-validate.js"))
    ,singleton(tags$script(src = "upload/js/md5.js"))
    ,singleton(tags$script(src = "http://mozilla.github.io/pdf.js/build/pdf.js"))
    ,singleton(tags$script(src = "upload/js/main.js"))
    ,singleton(tags$script(src = "js/tutorial.js")),
    
    
    
    
    # Title
    tags$title('SeqPlots')
)

# 0) Plot panel ############################################################
plotPanel <- conditionalPanel(
    condition = "input.showplot",
    div(
        HTML('<canvas class="pdf-output" id="thecanvas"></canvas>'),
        id='preview-pdf-div'
    ),
    div(
          class="btn-toolbar",
          tags$button(id='replotL', onClick="$('#img_heatmap').prop('checked', false).change(); $('#replot').click();", class='btn btn-success', tags$span(icon("line-chart", "fa-lg"), 'Profile' )),
          tags$button(id='replotH', onClick="$('#img_heatmap').prop('checked', true ).change(); $('#replot').click();", class='btn btn-info', tags$span(icon("th", "fa-lg"), 'Heatmap' )), 
          actionButton('replot', tags$span(tags$i(class="icon-refresh icon-large"))), 
          htmlOutput('pdfLink', inline=TRUE),
          hlp("plotting", 3)
          
    ), 
    tags$hr()
)

# 1) New plot panel ############################################################
newPlotPanel <-  tabPanel(
    value = 'panel1', title=tags$i(class="icon-rocket icon-large icon-blcak", 'data-placement'="right", 'data-toggle'="tooltip", title="New plot set/Upload files") #, "New"
    ,h5('Upload files:', hlp("adding-and-managing-files"))
    ,helpText( "Add signal tracks (bigWig, wig or bedGraph) and feature files (GFF and BED) to the file collection.") # TIP: You can add multiple files at once.
    ,HTML('<button type="button" data-toggle="modal" data-target="#fileUploadModal" class="btn btn-success"><i class="icon-cloud-upload icon-large icon-white"></i> Add files</button>')
    ,conditionalPanel("false", selectInput("file_genome", "Genmoe:", NULL, selectize = FALSE)) #This should stay for clonning, unless I can figure out something better using JS
    ,tags$hr()
    
    ,h5('Create new plot array:', hlp("running-the-plot-set-jobs"))
    ,helpText( "Choose signal tracks and feature files from the collection to use for calculating average plots and heat maps.")
    ,HTML('<button type="button" data-toggle="modal" data-target="#calcModal" class="btn btn-primary"><i class="icon-magic icon-large icon-white"></i> New plot set</button>')
    ,uiOutput('plot_message')
    ,if (Sys.getenv("SHINY_SERVER_VERSION") != '') {div(
        tags$hr(), h5('Soft restart the server session', hlp("advanced-options")),
        helpText( "The button will perform soft reset of the server. 
                                             This means a new session will be created for you and other active users will use their existing session(s) until they close the web browser.
                                             Use this option if you experience performance issues or errors."),
        actionButton('spawn', tags$span(tags$i(class="icon-bolt icon-large"), HTML('Restart server!') ))
    )} else {div(
        tags$hr(), helpText( "Stop and exit the web interface:"),
        actionButton('stopapp', tags$span(tags$i(class="icon-off icon-large"), HTML('Exit SeqPlots') ))
    )},
    tags$hr(),
    tags$a(class='', tags$span(class="label label-success", 'Help'), href=paste0("help/help.html"), target="_blank", 'Read documentation'),
    ' or press ', tags$i(class="icon-question-sign icon-large", style='color:orange'), 
    ' button to get help on specific controls.', #'Also available as ', tags$a(class='', href=paste0("http://przemol.github.io/seqplots/SeqPlots.pdf"), target="_blank", 'print ready PDF file.'),
    ' To run tutorial click ', a(href='?tutorial', 'here.'),
    tags$em(paste0('SeqPlots v', packageVersion('seqplots'), '.'))
    
)

# 2) Titles and axis panel #####################################################
titlesAndAxis <- tabPanel(
    value = 'panel3', 
    title=tags$i(class="icon-font icon-large icon-blcak", 'data-placement'="right", 'data-toggle'="tooltip", title="Title and axis"),
    h5(tags$u('Title and axis'), hlp("titles-and-axis-panel"), style='margin-bottom:15px'),
    div(class='row', 
        div(class='col-md-4', textInput("title", "Title:", ""),         
            numericInput("title_font_size", "Title size:",    min=1, max=48, value=20, step=1) ), #sliderInput
        div(class='col-md-4', textInput("xlabel", "X-axis label:", ""), 
            numericInput("labels_font_size", "Labels size:",  min=1, max=448, value=16, step=1) ), #sliderInput
        div(class='col-md-4', textInput("ylabel", "Y-axis label:", ""), 
            numericInput("axis_font_size", "Axis font size:", min=1, max=448, value=14, step=1) ) #sliderInput
    ),
    checkboxInput("xauto", "Set X-axis limits", FALSE),
    conditionalPanel( condition = "input.xauto == true", fluidRow(
        column(4, numericInput("xmin1", "From: ", 0), offset = 1), 
        column(4, numericInput("xmin2", "To: ", 0))
    )),               
    checkboxInput("yauto", "Set Y-axis limits", FALSE),
    conditionalPanel( condition = "input.yauto == true", fluidRow(
        column(4, numericInput("ymin1", "From: ", -1), offset = 1),
        column(4, numericInput("ymin2", "To: ", 10) )
    ))
)

# 3) Guide lines, and data scaling #############################################
guideLinesAndDataScaling <- tabPanel(
    value = 'panel4', 
    title=tags$i(class="icon-tags icon-large icon-blcak", 'data-placement'="right", 'data-toggle'="tooltip", title="Guide lines and data scaling"),# "Setup", 
    h5(tags$u('Guide lines and data scaling'), hlp("guide-lines-and-data-scaling")),
    selectInput('scale_signal', 'Transform signal:', c( 'Do not transform', 'Log2 transform')), #, 'Z-score transform')),
    checkboxInput("lnv", "Show vertical guide line", TRUE),
    div(class='row',  
        div(class='col-md-8',checkboxInput("lnh", "Show horizontal guide line", FALSE)),
        div(class='col-md-4',conditionalPanel( condition = "input.lnh == true",numericInput("lnh_pos", "-> position:", 0)))
    ),
    checkboxInput("ee", "Show error estimates", TRUE)
)

# 4) Keys, labels and colors  ##################################################       
keysLabelsAndColors <- tabPanel(
    value = 'panel5', 
    title=tags$i(class="icon-list-ol icon-large icon-blcak", 'data-placement'="right", 'data-toggle'="tooltip", title="Keys, labels and colors"), #"Subplots",
    h5(tags$u('Keys, labels and colors'), hlp("keys-labels-and-colors-panel")),
    checkboxGroupInput('subplot_options', 'Set sub-plot specific:', c('Colors'='color', 'Label'='label', 'Priority/Order'='prior'), selected = NULL),
    
    div(class='row',  
        div(class='col-md-8',checkboxInput("legend", "Show plot key", TRUE)),
        div(class='col-md-4',conditionalPanel( condition = "input.legend == true", selectInput("legend_pos", "-> position:", c("bottomright", "bottom", "bottomleft", "left", "topleft", "top", "topright", "right", "center"), 'topright' )))
    ),
    div(class='row', 
        div(class='col-md-8',checkboxInput("legend_ext", "Show error estimate key", FALSE)),
        div(class='col-md-4',conditionalPanel( condition = "input.legend_ext == true", selectInput("legend_ext_pos", "-> position:", c("bottomright", "bottom", "bottomleft", "left", "topleft", "top", "topright", "right", "center"),  "topleft" )))
    ),
    div(class='row',
        div(class='col-md-6', 
            numericInput("legend_font_size", "Legend font size:", min=1, max=48, value=12, step=1) #sliderInput"
        )
    )
)

#5) Heatmap options panel ######################################################   
heatmapPanel <- tabPanel(
    value = 'panel6', 
    title=tags$i(class="icon-th icon-large icon-blcak", 'data-placement'="right", 'data-toggle'="tooltip", title="Heatmap setup"), #("Sizes", 
    h5(tags$u('Heatmap setup'), hlp("heatmaps")),
    div( class='hidden', checkboxInput("img_heatmap", "Preview heatmap [Ctrl+H]") ), 
    #checkboxInput("img_sort", "Sort heatmap rows by mean signal"),
    selectInput("img_sort", "Sort heatmap rows by mean signal", c("decreasing", "increasing", 'do not sort'), 
                selected = NULL, multiple = FALSE, selectize = TRUE, width = NULL),
    div(class='row',
        div(class='col-md-6', selectInput("img_clstmethod", 'Clustering algorithm', c('K-means'='kmeans', 'Hierarchical'='hclust', 'SuperSOM'='ssom', 'do not cluster'='none'))),
        div(class='col-md-6',   
            conditionalPanel( 
                condition = "input.img_clstmethod != 'none' && input.img_clstmethod != 'ssom'", 
                numericInput("img_clusters", "Number of clusters", 5, min=1)),
            conditionalPanel( 
                condition = "input.img_clstmethod == 'ssom'", 
                div(class='row',
                    div(class='col-md-6', numericInput("img_ssomt1", "Grid-X", 2, min=1) ),
                    div(class='col-md-6', numericInput("img_ssomt2", "Grid-Y", 2, min=1) )
                )
            )
        )
    ),
    checkboxInput('heat_seed', 'Make cluster calculation repeatable', FALSE),
    conditionalPanel( 
        condition = "input.heat_seed == true", {
            selectInput(inputId = 'heat_subclust', label = 'Plot cluster only selected', choices = 'All clusters')  
        }),
    checkboxInput('heat_include', 'Choose individual heatmaps for sorting/clustering', FALSE),
    tags$br(),
    
    checkboxInput("indi", "Heatmaps have individual color keys", TRUE),
    checkboxInput("heatmapzauto", "Set default color key limits", FALSE),
    conditionalPanel( 
        condition = "input.heatmapzauto == false",
        div(class='row',
            div(class='col-md-6', "Color key scaling:"),
            div(class='col-md-4', numericInput("hsccoef", NULL , min=0, max=0.1, value=0.01, step=0.001)) #sliderInput   								                      )
        )                
        
    ),
    conditionalPanel( 
        condition = "input.heatmapzauto == true",
        div(class='form-inline', 
            numericInput("zmin1", "-> ", -1), 
            numericInput("zmin2", "-", 10)
        )
    ),
    conditionalPanel( 
        condition = "input.indi == true",
        checkboxInput('heat_min_max', 'Set individual color key limits', FALSE)
    ),
    div(class='row',
        div(class='col-md-9',
            selectizeInput(
                'heat_colorspace', 'Set colorspace', c(
                    '', 'Custom', rownames(RColorBrewer::brewer.pal.info),
                    'Jet colors'='jet', 'Topo colors'='topo.colors',
                    'Terrain colors'='terrain.colors', 'Heat colors'='heat.colors'
                ), selected = NULL, options = list(placeholder = 'Select colorspace (type to filter)')
            )
        ),
        div(class='col-md-3',
            tags$br(), checkboxInput('heat_colorspace_rev', 'Reverse', FALSE)
        )
    ),
    conditionalPanel( 
        condition = "input.heat_colorspace == 'Custom'",
        div(class='row', 
            div(class='col-md-4', HTML('Min: <input type="color" class="color {hash:true}" id="heat_csp_min" value="#FFFFFF" style="width:40px;" title=""/>')),
            div(class='col-md-4', HTML('Mid: <input type="color" class="color {hash:true}" id="heat_csp_mid" value="#87CEFA" style="width:40px;" title=""/>')),
            div(class='col-md-4', HTML('Max: <input type="color" class="color {hash:true}" id="heat_csp_max" value="#00008B" style="width:40px;" title=""/>'))
        )
    ),
    HTML('Click <a href="http://przemol.github.io/plotsHg19/ColorMapTests">here</a> to see example heatmaps plotted with different color palettes.')
    
)

# 6) Save/load panel ################################################## 
loadSavePanel <- tabPanel(value = 'panel2', title=tags$i(class="icon-save icon-large icon-blcak",  'data-placement'="right", 'data-toggle'="tooltip", title="Load/manage saved plotset"), #"Saved",										
         h5(tags$u('Load or save plotset'), hlp("saving-and-loading-plotsets")),
         selectizeInput(
             'publicRdata', 'Load saved plot set:', '', 
             options=list(
                 placeholder = 'Select dataset (type to filter)'
             )
         ),
         conditionalPanel("input.publicRdata !== ''", 
                          actionButton('RdataRemoveButton', 'Remove dataset', icon=icon('trash-o')) ,
                          downloadButton('RdataDoenloadButton', 'Download dataset') 
         ), tags$hr(),
         textInput('RdataSaveName', 'Save current plot set:', ''), 
         conditionalPanel("input.RdataSaveName !== ''", actionButton('RdataSaveButton', 'Save', icon=icon('save')) ) 
)

#7) Batch opts panel  ##################################################
batchPanel <- tabPanel(
    tags$i(class="icon-gears icon-blcak icon-large", 'data-placement'="right", 'data-toggle'="tooltip", title="Batch operations and setup"), #"Batch", 
    
    h5(tags$u('Output PDF paper type or size:'), hlp("pdf-output-size")), 

    selectInput(
        'paper', NULL, width=200,
        choices=c('A4 rotated'="a4r", 'Custom size...'="special", 'Legal rotated'="USr", 'A4'="a4", 'Letter'="letter", 'Legal'="US", 'Executive'="executive") 
    ),

    conditionalPanel( 
        condition = 'input.paper == "special"', div(
            class='form-inline', 
            numericInput("pdf_x_size", NULL, 16) ,  
            numericInput("pdf_y_size", "[in] x ", 10) , '[in]'
        )
    ),
    tags$hr(),
    conditionalPanel( 
        condition = 'false',
        checkboxInput('recordHistory', 'Record plot history', FALSE),      
        downloadLink('downloadHistory',   tags$span(tags$i(class="icon-time icon-large"), 'Get plot history'),   class="btn btn-small btn-success"),
        tags$hr()
    ),
    h5(tags$u('Batch operations:'), hlp("batch-operations")),   
    div(class='row',  
        div(class='col-md-5', selectInput('batch_what', 'Plot', c('lineplots', 'heatmaps')) ),  
        div(class='col-md-5', selectInput('batch_how', 'by', c('single', 'rows', 'columns')) )                        
    ),
    div(class='form-inline', 
        numericInput("grid_x_size", "Multi-plot grid: ", 1) ,  
        numericInput("grid_y_size", " x ", 1) 
    ), tags$br(),
    div(class='form-inline', 
        textInput('multi_name_flt', 'Filter names: '),
        downloadLink('downloadBatchColLineplot', 
            tags$span(
                tags$i(class="icon-align-justify icon-rotate-90"), 
                tags$i(class="icon-double-angle-right icon-large"),  
                tags$i(class="icon-picture icon-large icon-white"), 
                'Get PDF'),   
        class="btn btn-sm btn-success")
    ),
    tags$hr(style='margin-top:10px;'),
    #checkboxInput('setup_multithread', 'Use multithreading for calculations', (Sys.getenv("SHINY_SERVER_VERSION") != ''))
    h5(tags$u('Advanced options:'), hlp("advanced-options")),
    checkboxInput('pty_batch', 'Keep 1:1 aspect ratio in batch mode', TRUE),
    checkboxInput('pty', 'Always keep 1:1 aspect ratio', FALSE),
    checkboxInput("reactive", "Reactive plotting [ctrl+R]", FALSE),
    checkboxInput("raster", "Use raster bitmap to plot heatmaps", TRUE),
    checkboxInput("ggplot", "Use ggplot2 graphics package for heatmaps [EXPERIMENTAL]", FALSE),
    div(class='hidden', checkboxInput("showplot", "Show plot buttons", FALSE)),
    conditionalPanel(
        condition = tolower(as.character(Sys.getenv("SHINY_SERVER_VERSION") == '')),
        checkboxInput('setup_multithread', 'Use multithreading for calculations', .Platform$OS.type != 'windows')
    )
    
)

#8) Genomes panel  ##################################################
genomesPanel <- tabPanel(
    tags$i(class="fa fa-paw fa-lg", 'data-placement'="right", 'data-toggle'="tooltip", title="Manage reference genomes"), #"Batch", 
    h5(tags$u('Manage reference genomes'), hlp("genomes-managment")), 
    
    checkboxGroupInput('inst_genomes', 'Installed genomes:', installed.genomes()),
    actionButton('genomes_uninstall', label = 'Uninstall selected', icon = icon('remove'), class='btn-danger'),
    tags$hr(),
    selectizeInput(
        'avil_geneomes', "Available genomes:", available.genomes(), 
        multiple = TRUE, options = list(
            plugins = list('remove_button'),
            placeholder = 'Select genomes (type to filter)'
        ) 
    ),
    # plugins = list('remove_button', 'drag_drop') => includeScript("www/jquery-ui.js")
    actionButton('genomes_install', label = 'Install selected', icon = icon('plus'), class='btn-success'),
    tags$hr(),
    fileInput('genomes_file', 'Install from file:')
    
)

# DOWNLOAD BUTTONS  ##################################################

btnToolbar <- conditionalPanel(
    condition = "input.showplot",
    div("Download:", class='pull-left', style="margin-top: 6px"),
    div(class="btn-toolbar", 
        div(
            class="btn-group",
            downloadLink('downloadPlot',   tags$span(icon("line-chart", "fa-lg"), 'Profile' ), class="btn btn-small btn-success"),
            downloadLink('downloadLegend', tags$span(tags$i(class="icon-info icon-large")), class="btn btn-small btn-success") #Legend
        ),
        div(
            class="btn-group",
            downloadLink('downloadHeatmap', tags$span(tags$i(class="icon-th icon-large icon-white"), 'Heatmap'), class="btn btn-small btn-info"),
            downloadLink('downloadClusters', tags$span(tags$i(class="icon-sitemap icon-large")), class="btn btn-small btn-info") #'Clusters indicates'
        ),
        hlp("getting-pdfs-and-cluster-info", 3)
    )
)

# SIDEBAR  ##################################################
sidebar <- wellPanel(
    plotPanel,
    tabsetPanel(
        id='ctltabs',
        newPlotPanel,
        titlesAndAxis,
        guideLinesAndDataScaling,
        keysLabelsAndColors,
        heatmapPanel,
        loadSavePanel,
        batchPanel,
        genomesPanel
    ),
    tags$hr(),
    btnToolbar,
    div( class='hidden', 
         textInput('clusters', 'Clusters'), 
         textInput('sortingord', 'Sorting'), 
         textInput('finalord', 'finalord'),
         textInput('rowmeans', 'rowmenas')
         )
)

# MAIN ##################################################

mainPanel <- div(
    style="overflow-x: auto; overflow-y: auto; padding-top:200px",
#     tabsetPanel(
#         tabPanel(
#           "Plots selection", 
            div(class="form-group", uiOutput("htmltab") )
#         ),
#         tabPanel(
#             "Interactive lineplot", 
#             checkboxInput('interactiveLinePlot', 'Enable intercative plot', FALSE),
#             conditionalPanel(condition = "input.interactiveLinePlot", 
#                              selectInput('chart1Type', label='Select type', choices=c('lineWithFocusChart','lineChart','stackedAreaChart','scatterChart', 'multiBarChart'), selected='lineWithFocusChart'),              
#                              showOutput("chart1", "nvd3")
#             )
#         )
#     )
    #Debug code
    ,if( Sys.getenv("seqplots_debug", FALSE) ) {
        div( 
            class='', id='debug', tags$hr(),
             'Debug console: ', tags$br(), tags$textarea(id='debug_cmd', rows=4, style='width:88%'),
             actionButton('debug_submit', 'Submit'), verbatimTextOutput("debug_out")
        )
    }
)	



# LAYOUT  ##################################################

shinyUI(
    fluidPage(
        head,
        fluidRow(
            column(4, 
                #Animated header
                div(
                    id="letter-container", class="letter-container", 
                    HTML('<h2><a href="#">SeqPlots</a></h2>')
                ),
                sidebar
            ),
            column(8, mainPanel)
        ),
        
        #Page loading modal and mask, pops out just after page loads, removed after all JS loads, stays on JQuery error
        includeHTML( file.path(Sys.getenv("web", '.'), 'ui/loadModal.html') ), 
        
        #Calculation progress modal
         div(id="progressModal", class="modal", 'data-backdrop'="false", 'data-keyboard'="false", tabindex=-1, div(class="modal-dialog", div(
             class="modal-content",
             div(class="modal-header", tags$h3(id="progressModalLabel", 'Calculating...')),
             div(class="modal-body", verbatimTextOutput("summary2"), verbatimTextOutput("summary3"), actionButton('cancel', 'Cancel'))
         ))),
        
        ##File management modal
        eval(parse( file.path(Sys.getenv("web", '.'), 'ui/FileManagementModal.R') )),
        
        #File upload modal
        includeHTML( file.path(Sys.getenv("web", '.'), 'www/upload/upload.html') )
        
    )
)
