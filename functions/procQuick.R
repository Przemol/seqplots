# TODO: Add comment
###############################################################################
procSF <- function(setup, filelist, rm0=FALSE, ignore_strand=FALSE, x1=500, x2=2000, xm=1000, type='Point Features', 
                      add_heatmap=TRUE, cat3=NULL, cat4=NULL) {
  
  getSF  <- function(genome, gr, pattern, bin, simple=TRUE, revcomp=FALSE) {
    
    sl <- median(width(gr))
    gr <- gr[width(gr)==sl]
    
    grf <- resize(gr, width(gr)+(bin*2), fix='center')
    seqs <- getSeq(genome,  grf)
    
    pattern <- DNAString(pattern)
    
    if (simple) {
      #Simle line plot
      hits <- unlist( vmatchPattern(pattern, seqs, algo="naive-exact") )
      if(revcomp) {
        hits <- c(hits, unlist( vmatchPattern(reverseComplement(pattern), seqs, algo="naive-exact") ))
      }
      vec <- coverage( restrict( resize(hits, bin, fix='center'), 
                                 start=1, end=sl+(2*bin), keep.all.ranges=TRUE, use.names=FALSE) ) / length(grf)
      return( as.numeric( as.numeric(vec)[bin:(bin+sl)] ) )
    } else {
      #Matrix-like results
      rd <- as(vmatchPattern(pattern, seqs, algo="naive-exact"), "CompressedIRangesList")
      if(revcomp) {
        rd.rev <- as(vmatchPattern(reverseComplement(pattern), seqs, algo="naive-exact"), "CompressedIRangesList")
        pos <- NumericList(Map(c, start(rd), start(rd.rev))) + ( floor(nchar(pattern)/2) -1  )
      } else {
        pos <- start(rd) + ( floor(nchar(pattern)/2) -1  )
      }
      
      #Quick
      nviews <- length(seqs[[1]]) - bin + 1L
      idx <- logical(width(seqs)[1])
      
      #ex <- -seq_len( bin - nchar(pattern) + 2L )
      ex <- -seq_len( bin - 1L )
      out <- lapply(pos, function(x) { 
        idx[x] <- TRUE; 
        NLP0 <- cumsum(idx)
        NLP1 <- NLP0[ex]
        length(NLP1) <- nviews
        NLP2 <- c(0L, NLP0)
        length(NLP2) <- nviews
        return(NLP1 - NLP2)
      } )
      M <- do.call(rbind, out)
      return( M[ ,(bin/2):((bin/2)+sl)] )
    }
    
  }
  
  GENOMES <- BSgenome:::installed.genomes(splitNameParts=TRUE)$provider_version
  names(GENOMES) <- gsub('^BSgenome.', '', BSgenome:::installed.genomes())
  
  n <- 1
  k <- 1
  TSS <- list()
  for (j in filelist)	{
    cat(n, ") Processing TSS file: ", basename(j), "\n", sep="")
    
    sel <- import.gff(file.path('files',j), asRangedData=FALSE)
    
    proc <- list()
    for(i in 1:length(setup) ) {
      
      pattern <- setup[[i]]$pattern
      genome  <- setup[[i]]$genome
      bin     <- setup[[i]]$window
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
        #cat4(strsplit(pkg, '\\.')[[1]][[2]]); Sys.sleep(1);
        
        gr <- GenomicRanges::promoters(sel, x1, x2)
        gen <- get(strsplit(pkg, '\\.')[[1]][[2]])
        seqlengths(gr) <- seqlengths(gen)[seqlevels(gr)]
        gr <- trim(gr)
        
        cat4("Searching for motif...")
        M <- getSF(gen, gr, pattern, bin, !add_heatmap, revcomp=revcomp)
        all_ind  <- seq(-x1, x2, by=1 )

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
        means 	<- M
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

# TODO: Add comment
###############################################################################

procQuick <- function(trackfiles, filelist, bin=1L, rm0=FALSE, ignore_strand=FALSE, x1=500, x2=2000, xm=1000, type='Point Features', 
						add_heatmap=FALSE, cat3=NULL, cat4=NULL) {
	n <- 1
	k <- 1
	TSS <- list()
	for (j in filelist)	{
		cat(n, ") Processing TSS file: ", basename(j), "\n", sep="")
		
		sel <- import.gff(file.path('files',j), asRangedData=FALSE)
		
		proc <- list()
		for(i in 1:length(trackfiles) ) {
			cat("\tProcessing ChIP-seq experiment: ", trackfiles[i], "\n")
			cat3(paste('Processing:', basename(j), '@', trackfiles[i], '[', k, '/',length(filelist)*length(trackfiles), ']'))
			cat4("Loading track...")
			
			track <- BigWigFile(file.path('files', trackfiles[i]))
			if (ignore_strand) strand(sel) <- '*'
			
			
			if ( (type == 'Point Features') | (type == 'Midpoint Features') ) {
				if( type == 'Midpoint Features' ) sel <- resize(sel, 1, fix='center')
				
				cat4("Processing BW...")
				all_ind	<- seq(-x1, x2, by= as.numeric(bin) )
				sum <- summary(track, IRanges::promoters(sel, x1, x2+1), type='mean', size=length(all_ind))					
				
				cat4("Processing matrix...")
				if (!ignore_strand){ 
					sum[as.character(strand(sel))=='-'] <- lapply(sum[as.character(strand(sel))=='-'], rev)
				}
				M <- matrix(as.numeric(unlist(sum)), nrow=length(sel), byrow = TRUE)
			
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
			means 	<- colMeans(M, na.rm=TRUE) 
			stderror<- apply(M, 2, function (n) {sd(n, na.rm=T) / sqrt(length(n[!is.na(n)]))})
			conint  <- apply(M, 2, function (n) {qt(0.975, length(n[!is.na(n)])) * sd(n, na.rm=T) / sqrt(length(n[!is.na(n)]))})
			
			cat4("Exporting results...")
			proc[[i]] <- list(means=means, stderror=stderror, conint=conint, all_ind=all_ind, e=if (type == 'Anchored Features') xm else NULL,
					desc=paste(sub("\\.(bw|BW)$", "", basename(trackfiles[i])), sub("\\.(gff|GFF)$", "", basename(j)), sep="\n@"),
					heatmap=if (add_heatmap) M else NULL)
			
			
#			if( grepl("(bw|BW)$", trackfiles[i]) ) {
#				
#				select <- (tss[chrom(tss) %in% seqlevels(bw)]+max(XlimMin,XlimMax)); seqlevels(select) <- seqlevels(select)[seqlevels(select) %in% seqlevels(bw)]
#				start(select)[start( select ) < 1 ] <- 1
#				input.cov <- coverage( import.bw(bw, which = select, asRangedData = FALSE), weight = 'score' )
#			} else {
#				input.cov=get(trackfiles[i])
#			}
#			#browser() 
#			if (type == 'Point Features') {
#				cat('PointFeatuerPlot:', '\n')
#				proc[[i]] <- calc(input.cov=input.cov, d1=XlimMin, d2=XlimMax, tss=tss, rm0=rm0, ignore_strand=ignore_strand, cat4=cat4, 
#						desc=paste(sub("\\.(bw|BW)$", "", basename(trackfiles[i])), sub("\\.(gff|GFF)$", "", basename(j)), sep="\n@"))
#			} else if (type == 'Anchored Features') {
#				cat('AnchoredFeatuerPlot:', '\n')
#				proc[[i]] <- calcAnchored(input.cov=input.cov, d1=XlimMin, d2=XlimMax, e=XlimMid, 
#						desc=paste(sub("\\.(bw|BW)$", "", basename(trackfiles[i])), sub("\\.(gff|GFF)$", "", basename(j)), sep="\n@"), tss=tss, rm0=rm0)
#			} else if (type == 'Midpoint Features') {
#				cat('Mid-PointFeatuerPlot:', '\n')
#				proc[[i]] <- calcMidpoints(input.cov=input.cov, d1=XlimMin, d2=XlimMax, tss=tss, rm0=rm0, ignore_strand=ignore_strand, cat4=cat4, 
#						desc=paste(sub("\\.(bw|BW)$", "", basename(trackfiles[i])), sub("\\.(gff|GFF)$", "", basename(j)), sep="\n@"))
#			}
			gc()
			k <- k+1
		}		
		#if (length(proc)==2) { plotM(proc[[1]], proc[[2]], desc1=sprintf("%i) %s%s", n, basename(j), add), y1=range[1], y2=range[2]) }
		#if (length(proc)==3) { plotM(proc[[1]], proc[[2]], proc[[3]], desc1=sprintf("%i) %s%s", n, basename(j), add), y1=range[1], y2=range[2]) }
		names(proc) <- basename(trackfiles)
		TSS[[n]] <- proc
		
		
		#outname <- plotMext(proc, desc1=sprintf("%i) %s%s", n, basename(j), add), y1=range[1], y2=range[2])
		
		gc()
		n <- n+1
	}
	names(TSS) <- filelist
	return(TSS)
}

