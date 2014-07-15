#' SeqPlots innitiation
#'
#' This function is called to initaite SeqPlots and call web browser graphical user interface.
#' 
#' @param root the path to data directory, it will be created if not existing
#' @param debug run the SeqPlots in debyug mode, i.e. with web interface R console
#' @return NULL
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
#' run()

run <- function(root = file.path(path.expand("~"), "SeqPlots_data"), debug = FALSE) {
    
  message('Starting...')
  oldwd <- getwd()
  Sys.setenv(root=root, web=system.file('seqplots', package='seqplots'), seqplots_debug=debug)
  
  suppressPackageStartupMessages( require(RSQLite) )
  suppressPackageStartupMessages( require(shiny) )
  if ( !file.exists(root) | any( !file.exists(file.path(root, c('files.sqlite', 'removedFiles','files','publicFiles', 'tmp'))) ) ) {
    dir.create(root)
    setwd(root)
    require(RSQLite)
    sqlite <- dbDriver("SQLite")
    con <- dbConnect(sqlite, dbname = 'files.sqlite')
    dbGetQuery(con, 'CREATE TABLE files (id INTEGER PRIMARY KEY ASC, name TEXT UNIQUE, ctime TEXT, type TEXT, format TEXT, genome TEXT, user TEXT, comment TEXT)')
    if (!dbListTables(con) == "files") warning('Database not created!')
    dbDisconnect(con)
    if(!all( sapply(c('removedFiles','files','publicFiles', 'tmp'), dir.create) )) warning('Folders not created!')
  }
  message('\nData loaction: ', root)
  
  shiny::runApp(Sys.getenv('web'), launch.browser=TRUE)
  
  setwd(oldwd)
  
}
