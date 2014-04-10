gfplots <- function(debug = FALSE) {
	oldwd <- getwd()
	#on.exit(setwd(oldwd))
	root <- file.path(path.expand("~"), "GFplots_data")
	Sys.setenv(root=root, web=system.file('shiny', package='GFplots'))
	require(RSQLite)
	if ( !file.exists(root) ) {
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
	setwd(root)
	message('Data loaction: ', root)
	#setwd(system.file('Shiney', package='GFplots'))
	shiny::runApp( system.file('shiny', package='GFplots'))
}