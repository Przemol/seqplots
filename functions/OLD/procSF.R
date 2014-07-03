# Old function to process sequence features, now integrated into QuickProc
###############################################################################
procSF <- function(setup, filelist, bin=10L, rm0=FALSE, ignore_strand=FALSE, x1=500, x2=2000, xm=1000, type='Point Features', 
                   add_heatmap=TRUE, cat3=NULL, cat4=NULL) {
  
  GENOMES <- BSgenome:::installed.genomes(splitNameParts=TRUE)$provider_version
  names(GENOMES) <- gsub('^BSgenome.', '', BSgenome:::installed.genomes())
  
  n <- 1
  k <- 1
  TSS <- list()
  for (j in filelist)  {
    cat(n, ") Processing TSS file: ", basename(j), "\n", sep="")
    
    sel <- import.gff(file.path('files',j), asRangedData=FALSE)
    
    proc <- list()
    for(i in 1:length(setup) ) {
      
      pattern <- setup[[i]]$pattern
      genome  <- setup[[i]]$genome
      seq_win <- setup[[i]]$window
      pnam    <- setup[[i]]$name
      revcomp <- setup[[i]]$revcomp
      
      cat("\tProcessing motif: ", pattern, "\n")
      cat3(paste('Processing:', basename(j), '@', pattern, '[', k, '/',length(filelist)*length(setup), ']'))
      cat4("Loading track...")
      
      if (ignore_strand) strand(sel) <- '*'
      
      if ( (type == 'Point Features') | (type == 'Midpoint Features') ) {
        if( type == 'Midpoint Features' ) sel <- resize(sel, 1, fix='center')
        
        cat4("Processing genome...")
        pkg <- paste0('BSgenome.', names(GENOMES[GENOMES %in% genome]))
        require(pkg, character.only = TRUE)
        
        gr <- GenomicRanges::promoters(sel, x1, x2)
        gen <- get(strsplit(pkg, '\\.')[[1]][[2]])
        seqlengths(gr) <- seqlengths(gen)[seqlevels(gr)]
        gr <- trim(gr)
        
        cat4("Searching for motif...")
        M <- getSF(gen, gr, pattern, seq_win, !add_heatmap, revcomp=revcomp)
        
        cat4("Binning the motif...")
        M <-  t(apply(M, 1, function(x) approx(x, n=ceiling(ncol(M)/bin))$y ))        
        all_ind  <- seq(-x1, x2, by=bin )
        
      } else if (type == 'Anchored Features') {
        
        #left
        cat4("Processing upsterem...")
        left_ind <- seq(-x1, -1, by=bin)		
        sum.left <- summary(track, flank(sel, x1, start=TRUE), type='mean', size=length(left_ind))
        if (!ignore_strand) sum.left[as.character(strand(sel))=='-'] <- lapply(sum.left[as.character(strand(sel))=='-'], rev)
        M.left <- matrix(as.numeric(unlist( sum.left )), nrow=length(sel), byrow = TRUE)
        
        #middle
        cat4("Processing middle...")
        mid_ind <- seq(0, xm, by=bin)			
        sum.middle <- summary(track, sel, type='mean', size=length(mid_ind))
        if (!ignore_strand) sum.middle [as.character(strand(sel))=='-'] <- lapply(sum.middle[as.character(strand(sel))=='-'], rev)
        M.middle <- matrix(as.numeric(unlist( sum.middle )), nrow=length(sel), byrow = TRUE)
        
        #right
        cat4("Processing downsteream...")
        right_ind <- seq(xm+1, xm+x2, by=bin)	
        sum.right <- summary(track, flank(sel, x2, start=FALSE), type='mean', size=length(right_ind))
        if (!ignore_strand) sum.right[as.character(strand(sel))=='-'] <- lapply(sum.right[as.character(strand(sel))=='-'], rev)
        M.right <- matrix(as.numeric(unlist( sum.right )), nrow=length(sel), byrow = TRUE)
        
        M <- cbind(M.left, M.middle, M.right)
        all_ind <- c(left_ind, mid_ind, right_ind)
      }
      
      cat4("Calculeating means/stderr/95%CIs...")
      if (rm0) M[M==0] <- NA
      if(!add_heatmap) {
        means 	<- as.vector(M)
        stderror<- rep(0, length(M))
        conint  <- rep(0, length(M))
      } else {
        means 	<- colMeans(M, na.rm=TRUE) 
        stderror<- apply(M, 2, function (n) {sd(n, na.rm=T) / sqrt(length(n[!is.na(n)]))})
        conint  <- apply(M, 2, function (n) {qt(0.975, length(n[!is.na(n)])) * sd(n, na.rm=T) / sqrt(length(n[!is.na(n)]))})
      }
      cat4("Exporting results...")
      proc[[i]] <- list(means=means, stderror=stderror, conint=conint, all_ind=all_ind, e=if (type == 'Anchored Features') xm else NULL,
                        desc=paste(pnam, sub("\\.(gff|GFF)$", "", basename(j)), sep="\n@"),
                        heatmap=if (add_heatmap) M else NULL)
      gc()
      k <- k+1
    }		
    names(proc) <- sapply(setup, '[[', 'name')
    TSS[[n]] <- proc
    
    
    #outname <- plotMext(proc, desc1=sprintf("%i) %s%s", n, basename(j), add), y1=range[1], y2=range[2])
    
    gc()
    n <- n+1
  }
  names(TSS) <- filelist
  return(TSS)
}
