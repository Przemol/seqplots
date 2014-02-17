# Function to process extrack genomic signal from trackfiles and/or motif data, calculate statistics and present the data in nested list format.
###############################################################################

procQuick <- function(trackfiles, filelist, bin=1L, rm0=FALSE, ignore_strand=FALSE, x1=500, x2=2000, xm=1000, type='Point Features', 
						add_heatmap=FALSE, cat3=NULL, cat4=NULL, con=NULL) {
	n <- 1
	k <- 1
	TSS <- list()
  
	for (j in filelist)	{
		cat(n, ") Processing TSS file: ", basename(j), "\n", sep="")
		
		sel <- import(file(file.path('files',j)), asRangedData=FALSE)
		  genome_ind <- dbGetQuery(con, paste0("SELECT genome FROM files WHERE name = '", j, "'"))
		  pkg <- paste0('BSgenome.', names(GENOMES[GENOMES %in% genome_ind]))
		  require(pkg, character.only = TRUE); GENOME <- get(pkg)
		  remap_chr <- gsub(' ', '_',organism(GENOME)) %in% names(supportedSeqnameStyles())
		
		proc <- list()
		for(i in 1:length(trackfiles) ) {
      
			cat("\tProcessing track/motif: ", trackfiles[[i]][[1]], "\n")
			cat3(paste('Processing:', basename(j), '@', trackfiles[[i]][[1]], '[', k, '/',length(filelist)*length(trackfiles), ']'))
      
			if (ignore_strand)                strand(sel) <- '*'
			if( type == 'Midpoint Features' ) sel <- resize(sel, 1, fix='center')
			gr <- GenomicRanges::promoters(sel, x1, x2)
      
      if( class(trackfiles[[i]]) == 'character' ) {
			  cat4("Loading track...")
			  track <- BigWigFile(file.path('files', trackfiles[[i]]))
			  if(remap_chr) { seqnameStyle(gr) <- seqnameStyle(track) }
      
		  } else if ( class(trackfiles[[i]]) == 'list' ) {
        
		    #GENOMES <- BSgenome:::installed.genomes(splitNameParts=TRUE)$provider_version
		    #names(GENOMES) <- gsub('^BSgenome.', '', BSgenome:::installed.genomes())  
		    pattern <- trackfiles[[i]]$pattern
		    # genome  <- trackfiles[[i]]$genome
		    seq_win <- trackfiles[[i]]$window
		    pnam    <- trackfiles[[i]]$name
		    revcomp <- trackfiles[[i]]$revcomp
		    if(remap_chr) { seqnameStyle(gr) <- seqnameStyle(GENOME) }
		        
		  }
			
			if ( (type == 'Point Features') | (type == 'Midpoint Features') ) {
				
        
        all_ind  <- seq(-x1, x2, by=as.numeric(bin) )
        
				if( class(trackfiles[[i]]) == 'character' ) {
				  cat4("Processing BW...")
				  sum <- summary(track, gr, type='mean', size=length(all_ind))					
				  
				  cat4("Processing matrix...")
				  if (!ignore_strand){ 
				    sum[as.character(strand(sel))=='-'] <- lapply(sum[as.character(strand(sel))=='-'], rev)
				  }
				  M <- matrix(as.numeric(unlist(sum)), nrow=length(sel), byrow = TRUE)
				  
				} else if ( class(trackfiles[[i]]) == 'list' ) {

				  cat4("Processing genome...")
				  seqlengths(gr) <- seqlengths(GENOME)[seqlevels(gr)]
				  gr <- trim(gr)
				  
				  cat4("Searching for motif...")
				  M <- getSF(GENOME, gr, pattern, seq_win, !add_heatmap, revcomp=revcomp)
				  
				  cat4("Binning the motif...")
				  M <-  t(apply(M, 1, function(x) approx(x, n=ceiling(ncol(M)/bin))$y ))        
				  
				}
			
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
					desc=paste(sub("\\.(bw|BW)$", "", basename(trackfiles[[i]][[1]])), sub("\\.(gff|GFF)$", "", basename(j)), sep="\n@"),
					heatmap=if (add_heatmap) M else NULL)
			k <- k+1
		}		
		names(proc) <- basename( sapply(trackfiles, '[[', 1) )
		TSS[[n]] <- proc
		n <- n+1
	}
	names(TSS) <- filelist
	return(TSS)
}

