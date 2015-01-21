#' Prepares the instance of SeqPlots, that can be deployed to shinyapps.io
#' 
#' See http://shiny.rstudio.com/articles/shinyapps.html for further info.
#'
#' @param server The directory where server instance should be created.
#' @param data the data storige directory, should be a sub-dir of "server" for 
#'   shinyapps
#' 
#' @return \code{NULL}
#'
#' @keywords internal
#' 
deployServerInstance <- function(
    server=getwd(), data=file.path(server, "DATA")
) {
    
    dir.create(server)
    setwd(server)
    file.copy( 
        file.path(system.file("seqplots", package = "seqplots"), '/'), '.', 
        recursive = TRUE
    )
    file.remove('server_config.R')
    conf <- c(
        "Sys.setenv('root'= file.path(getwd(), 'DATA'))",
        "Sys.setenv('web' = getwd() )"
    )
    writeLines(conf, con = 'server_config.R')
    
    root <- data
    if ( !file.exists(root) | any( !file.exists(file.path(root, c(
        'files.sqlite', 'removedFiles','files','publicFiles', 'tmp'
    ))) ) ) {
        dir.create(root)
        setwd(root)
        sqlite <- RSQLite::SQLite()
        con <- dbConnect(sqlite, dbname = 'files.sqlite')
        dbGetQuery(con, paste(
            'CREATE TABLE files (id INTEGER PRIMARY KEY ASC, name TEXT UNIQUE,',
            'ctime TEXT, type TEXT, format TEXT, genome TEXT, user TEXT,',
            'comment TEXT)'
        ))
        if (!dbListTables(con) == "files") warning('Database not created!')
        dbDisconnect(con)
        if(!all( 
            sapply(c('removedFiles','files','publicFiles', 'tmp'), dir.create) 
        )) warning('Folders not created!')
    }
}
