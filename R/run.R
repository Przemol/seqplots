#' SeqPlots innitiation
#'
#' This function initaite SeqPlots and oppens 
#' web browser with graphical user interface (GUI).
#' 
#' @param root the path to data directory, it will be created if not existing
#' @param debug run the SeqPlots in debug mode, i.e. with R console embedded in 
#'  web interface
#' @param ... arguments sent to \code{\link[shiny]{runApp}} function
#' @return 
#' Normally returns nothing (\code{NULL}), returns an error if one accrued.
#' Usage messages are shown in R console.
#' 
#' @details
#' The default data directory is "~/SeqPlots_data".
#'
#' @author Przemyslaw Stempor
#' @keywords seqplots
#' @export 
#' 
#' @examples
#' \donttest{
#' run()
#' }

run <- function(
        root = file.path(path.expand("~"), "SeqPlots_data"), debug = FALSE, ...
    ) {
    
    message('Starting...')
    oldwd <- getwd()
    on.exit( {
        suppressWarnings(rm(list=c(
            "doFileOperations", "GENOMES", "mcCalcStart", "mcDoParallel", 
            "renderHTMLgrid"
        ), envir=.GlobalEnv))
        setwd(oldwd) 
    })
    
    Sys.setenv(
        root = root,
        web = system.file('seqplots', package = 'seqplots'),
        seqplots_debug = debug
    )
    
    if ( !file.exists(root) | any( !file.exists(file.path(root, c(
        'files.sqlite', 'removedFiles','files','publicFiles', 'tmp', 'genomes'
    ))) ) ) {
        
        dir.create(root)
        setwd(root)
        
        sqlite <- RSQLite::SQLite()
        con <- dbConnect(sqlite, dbname = 'files.sqlite')
        if ( !length(dbListTables(con)) ) {
            dbGetQuery(con, paste(
                'CREATE TABLE files (id INTEGER PRIMARY KEY ASC, name TEXT UNIQUE,',
                'ctime TEXT, type TEXT, format TEXT, genome TEXT, user TEXT,',
                'comment TEXT)'
            ))
            if ( !length(dbListTables(con)) ) warning('Database not created!')
        }
        dbDisconnect(con)
        
        if (!all(sapply(
            c('removedFiles','files','publicFiles', 'tmp', 'genomes'), dir.create
        ))) warning('Folders not created!')
    }
    
    #.libPaths(c( .libPaths(), file.path(root, 'genomes') ))
    
    message('\nData loaction: ', root)
    message( shiny::runApp(Sys.getenv('web'), ...) )
    
    return(invisible(NULL))
}
