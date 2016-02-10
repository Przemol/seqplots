doFileOperations <- function(x, final_folder='files', file_genome, file_user, file_comment, con=NULL) {
  
  #   corrrectCeChroms <- function(tss) {
  #     chrnames <- c("chrI","chrII","chrIII","chrIV","chrV","chrX","chrM")
  # 		col <- sapply( names(sort(unlist( sapply(c('.*([^I]|^)I$', '.*([^I]|^)II$', '.*III$', '.*IV$', '.*([^I]|^)V$', '.*X$', 'M'), function(x) {grep(x, seqlevels(tss), perl=T)}) ))), function(x) {grep(x, chrnames)})
  # 		seqlevels(tss) <- chrnames[col]
  # 		return(tss)
  # 	}
  
  normalizeName <- function(x) {
    newname <- file.path(dirname(x), gsub("[^[:alnum:]\\.\\-]", "_", basename(x)))
    if( file.rename(x, newname) ) return(newname) else stop('File cannot be renamed.', call. = FALSE)
  }
  
  testChromosomeNames <-  function(tss, gnm, ret=FALSE) {
    if( !all(seqlevels(tss) %in% seqlevels(gnm)) ) { 
      try( seqlevelsStyle(tss) <- seqlevelsStyle(gnm) )
      if( !all(seqlevels(tss) %in% seqlevels(gnm)) & ret ) {
        seqlevels(tss) <- as.character(as.roman( gsub('^chr', '', gsub('.*(M|m).*', 'M', seqlevels(tss)), ignore.case = TRUE) ))
        try( seqlevelsStyle(tss) <- seqlevelsStyle(gnm) )
      }
      if( !all(seqlevels(tss) %in% seqlevels(gnm)) ) 
        stop('Chromosome names provided in the file does not match ones defined in reference genome. \nINPUT: [', 
             paste(seqlevels(tss)[!seqlevels(tss) %in% seqlevels(gnm)], collapse=', '), "]\nGENOME: [", paste(head(seqlevels(gnm), 5), collapse=', '), ', ...]', call. = FALSE) 
    }
    if(ret) return(tss)
  }
  testFeatureFile <-  function(PATH, gnm){
    fcon <- file(PATH); tss <- try( rtracklayer::import( fcon ), silent = FALSE ); close(fcon);
    if (class(tss) == "try-error") {
    try({   nfields <- count.fields(PATH, comment.char = '', skip = 1)
            problem <- which(nfields != median( head(nfields, 1000) ))+1
    })
    err <- paste('ERROR:', attr(tss, 'condition')$message)
    if(is.integer(problem)) problem <- paste(err, '\n Possible problem with line ', problem, ': "\n', readLines(PATH, n=problem)[problem], '.')
    stop(err, call. = FALSE)
    }
    testChromosomeNames(tss, gnm)
  }

  if ( dbGetQuery(con, paste0("SELECT count(*) FROM files WHERE name = '",basename(x),"'")) > 0 )
    stop('File already exists, change the name or remove old one.', call. = FALSE)
  
  #File does not have correct genome
  gnm <- SeqinfoForBSGenome(file_genome); if( is.null(gnm) ) { 
    stop('Unknown genome name/genome not installed! Use UCSC compatible or contact administrator.', call. = FALSE) 
  }
  
  #session$sendCustomMessage("jsAlert", sprintf("adding file: %s", x))
  
  #File does not exist
  if( !file.exists(x) ) stop('Cannot add, file not on the server!')
  x <- normalizeName(x)
  
  if( grepl('.(gff|gff.gz|gtf|gtf.gz)$', x, ignore.case = TRUE) ) {
    type <- 'feature'; file_type <- 'GFF';
    testFeatureFile(x, gnm);
    
  } else if( grepl('.(bed|bed.gz)$', x, ignore.case = TRUE) ) {
    type <- 'feature'; file_type <- 'BED';
    testFeatureFile(x, gnm);
    
  } else if( grepl('.(bw|bigWig|bigWiggle)$', x, ignore.case = TRUE) ) {
    type <- 'track'; file_type <- 'BigWiggle';
    testChromosomeNames(seqinfo(BigWigFile(x)), gnm)
    
  } else if( grepl('.(wig|wig.gz|bdg|bdg.gz|bedGraph|bedGraph.gz)$', x, ignore.case = TRUE) ){
    pth <- gsub('.(wig|wig.gz|bdg|bdg.gz|bedGraph|bedGraph.gz)$', '.bw', x, ignore.case = TRUE);
    try_result <- try({ 
      #stop('test'); pth <- path(wigToBigWig(file.path('files', x), gnm)); 
      .Call(  get('BWGFile_fromWIG', environment(wigToBigWig)), x, seqlengths(gnm), pth )
    }) 
    if(is(try_result, 'try-error')) {
      try_result2 <<- try({	
        fcon=file(x); wig <- rtracklayer::import.wig( fcon ); close(fcon);
        if( grepl('list', class(wig), ignore.case = TRUE) ) wig <- unlist(wig, use.names=FALSE)
        wig <- testChromosomeNames(wig , gnm, ret=TRUE)
        seqlengths(wig) <- seqlengths(gnm)[seqlevels(wig)];
        export.bw(coverage(wig, weight='score'), pth);
      })
      if(is(try_result2, 'try-error')) { stop('Error in adding wiggle: ', as.character(try_result2)) }
    } 
    
    file.remove( x )
    x <- pth; type <- 'track'; file_type <- 'Wiggle';
    if( !all(seqlevels(BigWigFile(x)) %in% seqlevels(gnm)) ) { stop('Unknown chr names in Wiggle file, use UCSC compatible!', call. = FALSE) }
    
  } else {
    stop('Unknown file format!')
  }
  
  file.rename( x, file.path(final_folder, basename(x)) )
  
  sql_string <- paste0("INSERT INTO files (name, ctime, type, format, genome, user, comment) VALUES (", paste0("'",c(basename(x), as.character(Sys.time()), type, file_type, file_genome, file_user, file_comment), "'", collapse=", "),")") 
  dbBegin(con)
  res <- dbSendQuery(con, sql_string )
  
  if ( file.exists(file.path(final_folder, basename(x))) ) {
    dbCommit(con)
    message('File added.')
  } else {
    dbRollback(con)
    stop('File was not moved to final directory.', call. = FALSE)
  }
}

