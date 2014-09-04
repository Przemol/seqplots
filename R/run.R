#' SeqPlots innitiation
#'
#' This function is called to initaite SeqPlots and call web browser graphical user interface.
#' 
#' @param root the path to data directory, it will be created if not existing
#' @param debug run the SeqPlots in debyug mode, i.e. with web interface R console
#' @param ... arguments sent to \code{\link[shiny]{runApp}} function
#' @return 
#' Normally returns nothing (NULL), returns an error if one accrued.
#' Usage messages are shown in R console.
#' 
#' @details
#' The default home dir is ~/SeqPlots data
#'
#' @references https://bitbucket.org/przemol/seqplots/wiki/Home
#' @author Przemyslaw Stempor
#' @keywords seqplots
#' @export 
#' 
#' @examples
#' \dontrun{
#' run()
#' }

run <- function(root = file.path(path.expand("~"), "SeqPlots_data"), debug = FALSE, ...) {
    
  message('Starting...')
  oldwd <- getwd()
  on.exit( {
      rm(list=c("doFileOperations", "GENOMES", "getSF", "heatmapPlotWrapper", 
                "imPlot2", "mcCalcStart", "mcDoParallel", "plotMext", 
                "procQuick", "renderHTMLgrid" ), envir=.GlobalEnv)
      
      setwd(oldwd) 
  })

  Sys.setenv(root=root, web=system.file('seqplots', package='seqplots'), seqplots_debug=debug)
  if ( !file.exists(root) | any( !file.exists(file.path(root, c('files.sqlite', 'removedFiles','files','publicFiles', 'tmp'))) ) ) {
    dir.create(root)
    setwd(root)
    sqlite <- RSQLite::SQLite()
    con <- dbConnect(sqlite, dbname = 'files.sqlite')
    dbGetQuery(con, 'CREATE TABLE files (id INTEGER PRIMARY KEY ASC, name TEXT UNIQUE, ctime TEXT, type TEXT, format TEXT, genome TEXT, user TEXT, comment TEXT)')
    if (!dbListTables(con) == "files") warning('Database not created!')
    dbDisconnect(con)
    if(!all( sapply(c('removedFiles','files','publicFiles', 'tmp'), dir.create) )) warning('Folders not created!')
  }
  message('\nData loaction: ', root)
  
  message( shiny::runApp(Sys.getenv('web'), ...) )
  
  return(invisible(NULL)) 
}
