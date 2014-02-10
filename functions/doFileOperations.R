doFileOperations <- function(x, final_folder='files', file_genome, file_user, file_comment, con=NULL) {
  
  #   corrrectCeChroms <- function(tss) {
  #     chrnames <- c("chrI","chrII","chrIII","chrIV","chrV","chrX","chrM")
  # 		col <- sapply( names(sort(unlist( sapply(c('.*([^I]|^)I$', '.*([^I]|^)II$', '.*III$', '.*IV$', '.*([^I]|^)V$', '.*X$', 'M'), function(x) {grep(x, seqlevels(tss), perl=T)}) ))), function(x) {grep(x, chrnames)})
  # 		seqlevels(tss) <- chrnames[col]
  # 		return(tss)
  # 	}
  
  if ( dbGetQuery(con, paste0("SELECT count(*) FROM files WHERE name LIKE('%",gsub('\\.\\w+(|.gz)$', '',basename(x)),"%')")) > 0 )
    stop('File already exists, change the name or remove old one.')
  
  #File does not have correct genome
  gnm <- SeqinfoForBSGenome(file_genome); if( is.null(gnm) ) { stop('Unknown genome name/genome not installed! Use UCSC compatible or contact administrator.') }
  
  #session$sendCustomMessage("jsAlert", sprintf("adding file: %s", x))
  
  #File does not exist
  if( !file.exists(x) ) stop('Cannot add, file not on the server!')
  import_file <- file(x)
  
  if( grepl('.(gff|GFF)$', x) ) {
    type <- 'feature'; file_type <- 'GFF';
    tss <- import(import_file, asRangedData=FALSE)
    if( !all(seqlevels(tss) %in% seqlevels(gnm)) ) { 
      seqnameStyle(tss) <- seqnameStyle(gnm)
      if( !all(seqlevels(tss) %in% seqlevels(gnm)) ) stop('Chromosome names do not exist in selected genome!') 
    }
    message('GFF file added', x)
    
  } else if( grepl('.(bed|BED)$', x) ){
    type <- 'feature'; file_type <- 'BED';
    tss <- import(import_file, asRangedData=FALSE)
    if( !all(seqlevels(tss) %in% seqlevels(gnm)) ) { 
      seqnameStyle(tss) <- seqnameStyle(gnm)
      if( !all(seqlevels(tss) %in% seqlevels(gnm)) ) stop('Chromosome names do not exist in selected genome!') 
    }
    message('BED file added', x)
    
  } else if( grepl('.(bw|BW)$', x) ){
    type <- 'track'; file_type <- 'BigWiggle';
    if( !all(seqlevels(BigWigFile(x)) %in% seqlevels(gnm)) ) { 
      bwinfo <- seqinfo(BigWigFile(x)); seqnameStyle(bwinfo) <- seqnameStyle(gnm)
      if( !all(seqlevels(bwinfo) %in% seqlevels(gnm)) ) { stop('Unable to correct chr names in BigWiggle file!') }
    }
    message('BW file added', x)
    
  } else if( grepl('.(wig|WIG|wig.gz|WIG.gz)$', x) ){
    pth <- gsub('.(wig|WIG|wig.gz|WIG.gz)$', '.bw', x) ;
    try_result <- try({ 
      #stop('test'); pth <- path(wigToBigWig(file.path('files', x), gnm)); 
      .Call(  get('BWGFile_fromWIG', environment(wigToBigWig)), x, seqlengths(gnm), pth )
    }) 
    if(is(try_result, 'try-error')) {
      try_result2 <<- try({	
        wig <- import.wig(import_file, asRangedData=FALSE);
        if( !all(seqlevels(wig) %in% seqlevels(gnm)) ) { 
          seqnameStyle(wig) <- seqnameStyle(gnm)
          if( !all(seqlevels(wig) %in% seqlevels(gnm)) ) stop('Chromosome names do not exist in selected genome!') 
        }
        seqlengths(wig) <- seqlengths(gnm)[seqlevels(wig)];
        export.bw(coverage(wig, weight='score'), pth);
      })
      if(is(try_result2, 'try-error')) { stop('Error in adding wiggle: ', as.character(try_result2)) }
    } 
    
    file.remove( x )
    x <- pth; type <- 'track'; file_type <- 'Wiggle';
    if( !all(seqlevels(BigWigFile(x)) %in% seqlevels(gnm)) ) { stop('Unknown chr names in Wiggle file, use UCSC compatible!') }
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

