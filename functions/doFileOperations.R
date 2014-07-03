doFileOperations <- function(x, final_folder='files', file_genome, file_user, file_comment, con=NULL) {
  
  #   corrrectCeChroms <- function(tss) {
  #     chrnames <- c("chrI","chrII","chrIII","chrIV","chrV","chrX","chrM")
  # 		col <- sapply( names(sort(unlist( sapply(c('.*([^I]|^)I$', '.*([^I]|^)II$', '.*III$', '.*IV$', '.*([^I]|^)V$', '.*X$', 'M'), function(x) {grep(x, seqlevels(tss), perl=T)}) ))), function(x) {grep(x, chrnames)})
  # 		seqlevels(tss) <- chrnames[col]
  # 		return(tss)
  # 	}
  testChromosomeNames <-  function(tss, gnm, ret=FALSE) {
    if( !all(seqlevels(tss) %in% seqlevels(gnm)) ) { 
      try( seqnameStyle(tss) <- seqnameStyle(gnm) )
      if( !all(seqlevels(tss) %in% seqlevels(gnm)) & ret ) {
        seqlevels(tss) <- as.character(as.roman( gsub('^chr', '', gsub('.*(M|m).*', 'M', seqlevels(tss)), ignore.case = TRUE) ))
        try( seqnameStyle(tss) <- seqnameStyle(gnm) )
      }
      if( !all(seqlevels(tss) %in% seqlevels(gnm)) ) 
        stop('Chromosome names provided in the file does not match ones defined in reference genome. \nINPUT: [', 
             paste(seqlevels(tss)[!seqlevels(tss) %in% seqlevels(gnm)], collapse=', '), "]\nGENOME: [", paste(head(seqlevels(gnm), 5), collapse=', '), ', ...]', call. = FALSE) 
    }
    if(ret) return(tss)
  }
  testFeatureFile <-  function(PATH, gnm){
    tss <- try(import( file(PATH) ), silent = FALSE)
    if (class(tss) == "try-error") {
      nfields <- count.fields(PATH, comment.char = '', skip = 1)
      problem <- which(nfields != median( head(nfields, 1000) ))+1
      stop('Problem with line ', problem, ': "', readLines(PATH, n=problem)[problem], '" [',attr(tss, 'condition')$message,']', call. = FALSE)
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
  #import(text=grep('^#', readLines('sample.bed.gz'), invert = TRUE, value=TRUE), format='bed') 
  
  if( grepl('.(gff|GFF|gff.gz|GFF.gz)$', x) ) {
    type <- 'feature'; file_type <- 'GFF';
    testFeatureFile(x, gnm); message('GFF file added', x)
    
  } else if( grepl('.(bed|BED|bed.gz|BED.gz)$', x) ) {
    type <- 'feature'; file_type <- 'BED';
    testFeatureFile(x, gnm); message('BED file added', x)
    
  } else if( grepl('.(bw|BW)$', x) ) {
    type <- 'track'; file_type <- 'BigWiggle';
    testChromosomeNames(seqinfo(BigWigFile(x)), gnm)
    message('BW file added', x)
    
  } else if( grepl('.(wig|WIG|wig.gz|WIG.gz)$', x) ){
    pth <- gsub('.(wig|WIG|wig.gz|WIG.gz)$', '.bw', x);
    try_result <- try({ 
      #stop('test'); pth <- path(wigToBigWig(file.path('files', x), gnm)); 
      .Call(  get('BWGFile_fromWIG', environment(wigToBigWig)), x, seqlengths(gnm), pth )
    }) 
    if(is(try_result, 'try-error')) {
      try_result2 <<- try({	
        wig <- import.wig(file(x));
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
    message('WIG file added', x)
    
  } else {
    stop('Unknown file format!')
  }
  
  file.rename( x, file.path(final_folder, basename(x)) )
  
  sql_string <- paste0("INSERT INTO files (name, ctime, type, format, genome, user, comment) VALUES (", paste0("'",c(basename(x), as.character(Sys.time()), type, file_type, file_genome, file_user, file_comment), "'", collapse=", "),")") 
  dbBeginTransaction(con)
  res <- dbSendQuery(con, sql_string )
  
  if ( file.exists(file.path(final_folder, basename(x))) ) {
    dbCommit(con)
  } else {
    dbRollback(con)
  }
}

