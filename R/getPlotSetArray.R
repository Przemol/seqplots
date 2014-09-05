#' Process genomic signal
#'
#' This function takes genomic cooridnates in BED or GFF format, 
#' and extracts the signal from track files (BigWig) and/or 
#' calculates motif density in these regions. Then it computs the statistics 
#' required for average and heatmap plots, and returns the \code{\link{PlotSetArray}} 
#' class.
#'
#' @param tracks list or vector of track paths (BigWig) and/or motif 
#' setup structers (as \code{\link{list}})
#' @param features vector of feature file paths (BED or GFF)
#' @param refgenome the UCSC cole of refference genome, e.g. 'hg19' hor 
#' Homo sapiens
#' @param bin binning window size in base pairs, defaults t 1L
#' @param rm0 remove 0 from mean/median calculation, defaults t FALSE
#' @param xmin upsterem distance in base pairs, defaults to 200L
#' @param xmax downsteram distance in base pairs, defaults to 2000L
#' @param xanchored distance in base pairs, defaults to 1000L
#' @param type the type of the calculation, "pf" for point features (default),
#' "mf" for midpoint features and 'af' for anchored features
#' @param add_heatmap return the heatmap data, defauls to TRUE
#' @param ignore_strand If TRUE the strand is ignored, that is 
#'  all regardless og annotatin in GFF/BED file all strands are treated 
#'  as undetrmined ("*"), defauls to FALSE
#' 
#' @return \code{\link{PlotSetArray}}
#' 
#' @details
#' Function to process genomic signal from tracks and/or motif data, 
#' calculate statistics. This function shoud be used as the entry point to the
#' seqplots pipeline and fillowed by plotting function(s).
#' 
#' @family plotting functions
#' @export
#' 
#' 
#' @examples
#' \dontrun{
#' ms <- MotifSetup()
#' ms$addMotif('GAGA')
#' ms$addMotif('TATA', window=200L, heatmap=TRUE, 
#'             revcomp=TRUE, genome='ce10', name='TATA box')
#'             
#' bed1 <- system.file("extdata", "Transcripts_ce10_chrI_100Kb.bed", package="seqplots")
#' bed2 <- system.file("extdata", "GSM1208361_chrI_100Kb_PeakCalls.bed", package="seqplots")
#' getPlotSetArray(ms, c(bed1, bed2), 'ce10')
#' }
#' 
getPlotSetArray <- function(tracks, features, refgenome, bin=10L, rm0=FALSE, 
    ignore_strand=FALSE, xmin=2000L, xmax=2000L, xanchored=1000L, type='pf', 
    add_heatmap=TRUE) {
    
    if( class(tracks) == "MotifSetup" ) tracks <- tracks$data
    
    n <- 1; k <- 1;
    TSS <- list(length(features))
    GENOMES <- BSgenome::installed.genomes(splitNameParts=TRUE)$provider_version
    if( length(GENOMES) ) 
        names(GENOMES) <- gsub('^BSgenome.', '', BSgenome::installed.genomes())
    
    extarct_matrix <- function(track, gr, size, ignore_strand) {
        sum <- .Call('BWGFile_summary', path.expand(path(track)),
                     as.character(seqnames(gr)),
                     ranges(gr), recycleIntegerArg(size, "size", length(gr)), "mean",
                     as.numeric(NA_real_), PACKAGE='rtracklayer')
        M <- do.call( rbind, sum )
        if (!ignore_strand) M[as.character(strand(gr))=='-', ] <- M[as.character(strand(gr))=='-', ncol(M):1]
        return(M)
    }
    
    for (j in features)	{
        
        #Set up genome package
        genome_ind <- refgenome
        pkg <- paste0('BSgenome.', names(GENOMES[GENOMES %in% genome_ind]))
        suppressPackageStartupMessages(library(pkg, character.only = TRUE, quietly=TRUE)); GENOME <- get(pkg)
        remap_chr <- gsub(' ', '_',organism(GENOME)) %in% names(genomeStyles())
        
        #Get features to plot
        file_con <- file( normalizePath(j) ); sel <- rtracklayer::import(file_con , asRangedData=FALSE); close(file_con)
        
        proc <- list()
        for(i in 1:length(tracks) ) {
            
            message(paste('Processing:', basename(j), '@', tracks[[i]][[1]], '[', k, '/',length(features)*length(tracks), ']'))
            
            if (ignore_strand)                strand(sel) <- '*'
            if( type == 'mf' ) sel <- resize(sel, 1, fix='center')
            
            if( class(tracks[[i]]) == 'character' ) {
                track <- BigWigFile( normalizePath(tracks[[i]]) )
                if(remap_chr) { seqlevelsStyle(sel) <- seqlevelsStyle(track) }
                
            } else if ( class(tracks[[i]]) == 'list' ) {  
                pattern <- tracks[[i]]$pattern
                seq_win <- tracks[[i]]$window
                pnam    <- tracks[[i]]$name
                revcomp <- tracks[[i]]$revcomp
                if(remap_chr) { seqlevelsStyle(sel) <- seqlevelsStyle(GENOME) }
                
            }
            
            
            
            if ( (type == 'pf') | (type == 'mf') ) {
                
                gr <- GenomicRanges::promoters(sel, xmin, xmax)
                all_ind  <- seq(-xmin, xmax, by=as.numeric(bin) )
                
                if( class(tracks[[i]]) == 'character' ) {
                    M <- extarct_matrix(track, gr, length(all_ind), ignore_strand)
                    
                } else if ( class(tracks[[i]]) == 'list' ) {
                    
                    message("Processing genome...")
                    seqlengths(gr) <- seqlengths(GENOME)[seqlevels(gr)]
                    gr <- trim(gr)
                    
                    message("Searching for motif...")
                    M <- getSF(GENOME, gr, pattern, seq_win, !add_heatmap, revcomp=revcomp)
                    if (!ignore_strand) M[as.character(strand(gr))=='-', ] <- M[as.character(strand(gr))=='-', ncol(M):1]
                    
                    message("Binning the motif...")
                    M <-  t(apply(M, 1, function(x) approx(x, n=ceiling(ncol(M)/bin))$y ))        
                    
                }
                
            } else if (type == 'af') {
                left_ind <- seq(-xmin, -1, by=bin)
                mid_ind <- seq(0, xanchored, by=bin)   
                right_ind <- seq(xanchored+bin, xanchored+xmax, by=bin) 
                all_ind <- c(left_ind, mid_ind, right_ind)
                
                if( class(tracks[[i]]) == 'character' ) {
                    M.left <- extarct_matrix(track, flank(sel, xmin, start=TRUE), length(left_ind), ignore_strand) #left
                    M.middle <- extarct_matrix(track, sel, length(mid_ind), ignore_strand) #middle    
                    M.right <- extarct_matrix(track, flank(sel, xmax, start=FALSE), length(right_ind), ignore_strand)  #right
                    M <- cbind(M.left, M.middle, M.right)   
                    
                } else if ( class(tracks[[i]]) == 'list' ) {
                    #MOTIF - left 
                    message(paste0("MOTIF: Processing upsterem ", pattern, " motif..."))
                    gr <- flank(sel, xmin, start=TRUE); seqlengths(gr) <- seqlengths(GENOME)[seqlevels(gr)];
                    M <- getSF(GENOME, trim(gr), pattern, seq_win, !add_heatmap, revcomp=revcomp)
                    if (!ignore_strand) M[as.character(strand(gr))=='-', ] <- M[as.character(strand(gr))=='-', ncol(M):1]
                    M.left <-  t(apply(M, 1, function(x) approx(x, n=length(left_ind))$y ))
                    
                    #MOTIF - middle
                    message(paste0("MOTIF: Processing middle ", pattern, " motif..."))
                    gr <- sel; seqlengths(gr) <- seqlengths(GENOME)[seqlevels(gr)];		
                    M <- getSF(GENOME, trim(gr), pattern, seq_win, !add_heatmap, revcomp=revcomp)
                    if (!ignore_strand) M[as.character(strand(gr))=='-', ] <- M[as.character(strand(gr))=='-', ncol(M):1]
                    M.middle <-  t(apply(M, 1, function(x) approx(x, n=length(mid_ind))$y ))
                    
                    #MOTIF - right
                    message(paste0("MOTIF: Processing downsteream ", pattern, " motif..."))
                    gr <- flank(sel, xmin, start=FALSE); seqlengths(gr) <- seqlengths(GENOME)[seqlevels(gr)];
                    M <- getSF(GENOME, trim(gr), pattern, seq_win, !add_heatmap, revcomp=revcomp)
                    if (!ignore_strand) M[as.character(strand(gr))=='-', ] <- M[as.character(strand(gr))=='-', ncol(M):1]
                    M.right <-  t(apply(M, 1, function(x) approx(x, n=length(right_ind))$y ))

                    M <- cbind(M.left, M.middle, M.right)                        
                    
                }
            }
            
            #message("Calculeating means/stderr/95%CIs...")
            if (rm0) M[M==0] <- NA
            means 	<- colMeans(M, na.rm=TRUE) 
            stderror<- apply(M, 2, function (n) {sd(n, na.rm=T) / sqrt( sum(!is.na(n)) )})
            conint  <- apply(M, 2, function (n) {qt(0.975, sum(!is.na(n)) ) * sd(n, na.rm=T) / sqrt( sum(!is.na(n)) )})
            
            #message("Exporting results...")
            proc[[i]] <- list(means=means, stderror=stderror, conint=conint, all_ind=all_ind, e=if (type == 'af') xanchored else NULL,
                              desc=paste(sub("\\.(bw|BW)$", "", basename(tracks[[i]][[1]])), sub("\\.(gff|GFF)$", "", basename(j)), sep="\n@"),
                              heatmap=if (add_heatmap) M else NULL)
            k <- k+1
        }		
        names(proc) <- basename( sapply(tracks, '[[', 1) )
        TSS[[n]] <- proc
        n <- n+1
    }
    names(TSS) <- features
    return( PlotSetArray(data=TSS) )
}
