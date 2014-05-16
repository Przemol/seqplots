seqplots <- function(root = file.path(path.expand("~"), "GFplots_data"), debug = FALSE) {
  
  message('Starting...')
  oldwd <- getwd()
  Sys.setenv(root=root, web=system.file('seqplots', package='seqplots'))
  
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
  
}