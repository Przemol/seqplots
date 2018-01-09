#' Add files to local SeqPlots installation
#'
#' @param x file path, GRanges, SimpleRleList or BigWigFile object
#' @param name neme of the file
#' @param file_genome genome version
#' @param file_user user name
#' @param file_comment comment
#' @param root local installation directory
#'
#' @return NULL
#' @export
#'
add_local_file <- function(
    x, name=deparse(substitute(x)),
    file_genome = 'ce11', file_user = 'user', file_comment = '',
    root=file.path(path.expand("~"), "SeqPlots_data")
) {
    source(system.file('seqplots/functions/doFileOperations.R', package = 'seqplots'))
    require(RSQLite)
    require(BSgenome)
    require(rtracklayer)
    
    owd <- getwd(); on.exit(setwd(owd))
    setwd(root)
    con <- RSQLite::dbConnect(RSQLite::SQLite(), dbname = 'files.sqlite')
    on.exit(dbDisconnect(con))
    
    if( class(x)=="GRanges" ) {
        out <- paste0( file.path(tempdir(), name), '.bed')
        message(out)
        export.bed(x, out)
    } else if(class(x)=="SimpleRleList") {
        out <- paste0( file.path(tempdir(), name), '.bw')
        message(out)
        export.bw(x, out)
    } else if(class(x)=="BigWigFile") {
        out <- file.path(tempdir(), basename(path(x)))
        message(out)
        file.copy(path(x), out)
    } else if(file.exists(x)){
        out <- file.path(tempdir(), x)
        file.copy(x, out)
    }
    doFileOperations(out, final_folder='files', file_genome = file_genome, file_user = file_user, file_comment = file_comment, con=con)
    
    #if(file.exists(out)) file.remove(out)
}
