#' Process genomic signal
#'
#' Function to process genomic signal from tracks and/or motif data, 
#' calculate statistics. This function shoud be used as the entry point to the
#' seqplots pipeline and fillowed by plotting function(s).
#'
#' @param tracks Character vector or list of BigWig track paths. For motif density plots
#'  \code{\link{MotifSetup}} class containing motifs setup and and/or BigWig 
#'  track paths (see details)
#' @param features Character vector or list containing feature file paths (BED or GFF).
#' @param refgenome The UCSC code of refference genome, e.g. 'hg19' for 
#'  Homo sapiens (see details)
#' @param bin Binning window size in base pairs, defaults to 1L
#' @param rm0 Remove zeros from mean/error estimate calculation, 0 in track file 
#'  will be treatet as missing data, defaults to FALSE
#' @param xmin Upsterem calcualation distance in base pairs, defaults to 200L
#' @param xmax Downsteram calcualtion distance in base pairs, defaults to 2000L
#' @param xanchored Anchored, feature body pseudo length in base pairs.
#'  The features will be extended or shrunk using linear approximation. 
#'  Used only if /code{type="af"}, defaults to 1000L
#' @param type The type of the calculation, "pf" for point features (default),
#' "mf" for midpoint features and "af" for anchored features, see details
#' @param add_heatmap Add the heatmap data to output, must be on to plot heatmap
#' form output \code{\link{PlotSetArray}} class, defauls to TRUE
#' @param ignore_strand If TRUE the directionality is ignored, that is all features' 
#'  strands, regardless of annotatin in GFF/BED file, are treated 
#'  as undetrmined ("*"), defauls to FALSE
#' 
#' @return \code{\link{PlotSetArray}}
#' 
#' @details
#' This function takes genomic cooridnates in BED or GFF format, 
#' and extracts the signal from track files (BigWig) and/or 
#' calculates motif density in these regions. Then it computs the statistics 
#' required for average and heatmap plots. Returns the \code{\link{PlotSetArray}} 
#' class, which can be further subseted, and used for plotting.
#' 
#' \subsection{Modes of operation}{
#'  The function operate in three modes, determined by \code{type} parameter:
#'  \itemize{
#'      \item \strong{Point Features} - anchor plot on the start of a feature. 
#'      By default, plot will be directional if strand information is present 
#'      (i.e, use start position and plot on positive strand for + strand 
#'      features and use end position and plot on negative strand for minus 
#'      strand features). If strand information is not present in the feature 
#'      file (or if the "ignore strand" option is chosen), plot will use start 
#'      position of feature and be plotted on the positive strand (see explanations). 
#'      User chooses length of upstream and downstream sequence to plot.
#'      \item \strong{Midpoint Features} - similar to point feature, but plot is centered on the midpoint of the feature.
#'      \item \strong{Anchored Features} - features are anchored at start and stop positions and given pseudo-length chosen by the user. Additionally, the user chooses the length of sequence upstream of the start and downstream of the end to plot.
#'  }
#' }
#' 
#' \subsection{Binning the track}{
#'  \code{bin} numeric parameter determines the resolution of data acquisition.
#'  The default value 10 means that 10bp intervals within the plotting range will
#'  be summarized by calculating the mean. Higher values increases the speed of 
#'  calculation and produces smoother plots, but decreases resolution.
#' }
#' 
#' \subsection{DNA motifs}{
#'  The \code{\link{MotifSetup}} class allows to calculate and plot the density 
#'  of any user-defined motif around the chosen genomic feature using the 
#'  reference sequence package. Motif plots can be mixed with track files' signal 
#'  plots. The \code{\link{MotifSetup}} can be initialized in following way:
#' 
#'  \code{
#'  ms <- MotifSetup() \cr
#'  ms$addMotif("TATA", window=200L, heatmap=TRUE, revcomp=TRUE, name=pattern) \cr
#'  ms$addMotif("GAGA", window=100L)$addBigWig("path/to/file.bw") \cr
#'  }
#' 
#'  The \code{addMotiff} methods accepts following parameters:
#'  \tabular{ll}{
#'      \code{motif}    \tab The DNA motif sequence. \cr
#'      \code{window}   \tab Sliding window size in base pairs [bp] - the size 
#'      of the sliding window for motif calculation. The value (number of 
#'      matching motifs within the window) is reported in the middle of the 
#'      window, e.g. if window is set to 200bp, DNA motif is "GC" and there are 
#'      8 CpGs in first 200 bp of the chromosome the value 8 will be 
#'      reported at 100th bp.\cr
#'      \code{name}     \tab Display name - The name of the motif that will be 
#'      shown in key and heatmap labels. Leave blank to use DNA motif value.\cr
#'      \code{heatmap}  \tab Plot heatmap or error estimates - this checkbox 
#'      determines if heatmap matrix and error estimates should be calculated. 
#'      If unchecked much faster algorithm will be used for motif density 
#'      calculation, but only the average plot without the error estimates 
#'      will be available.\cr
#'      \code{revcomp}  \tab Match reverse complement as well - select if 
#'      reverse complement motif should be reported as well. For example the 
#'      TATA motif will report both TATA and ATAT with this option selected.\cr
#'  }
#' }
#' \subsection{Reference genomes}{
#'  The \code{refgenome} parameter determins the reference genome to be used 
#'  for calculation. Reference genome package is needed to establish baseline
#'  chromosome naming convention (e.g. chrX vs. X) and chromosome lengths. 
#'  Also for motif plots the genomic sequence si used to calcualte motif
#'  density tracks. To chack which genomic packages are installed in current R 
#'  session use \code{\link[BSgenome]{installed.genomes}} function.
#'  \code{\link[BSgenome]{available.genomes}} gibes the list of all reference 
#'  genome packages curently supplied by BioConductor. Please refer to 
#'  \code{\link[BSgenome]{BSgenome}} package documentation for isnatlling and 
#'  forging new genome packages.
#' }
#' 
#' @family plotting functions
#' @export
#' 
#' 
#' @examples
#' \dontrun{
#'   
#' # Get the paths of example files                      
#' bed1 <- system.file("extdata", "Transcripts_ce10_chrI_100Kb.bed", package="seqplots")
#' bed2 <- system.file("extdata", "GSM1208361_chrI_100Kb_PeakCalls.bed", package="seqplots")
#' bw1 <- system.file("extdata", "GSM1208360_chrI_100Kb_q5_sample.bw", package="seqplots")
#' 
#' #If required install C. elegans genomic package from Bioconductor
#' if(!"BSgenome.Celegans.UCSC.ce10" %in% BSgenome::installed.genomes()) {
#'     source("http://bioconductor.org/biocLite.R")
#'     biocLite("BSgenome.Celegans.UCSC.ce10")
#' }
#' 
#' #Get getPlotSetArray for track and feature files
#' plotset1 <- getPlotSetArray(bw1, c(bed1, bed2), 'ce10')
#' plot(plotset1) #Average plot
#' plot(plotset1[1,], what='h') #Heatmap
#' 
#' #Get getPlotSetArray for motifs, track and feature files
#' ms <- MotifSetup()
#' ms <- MotifSetup()
#' ms$addMotif('GAGA')
#' ms$addMotif('TATA')
#' ms$addBigWig(bw1)
#' 
#' plotset <- getPlotSetArray(ms, c(bed1, bed2), 'ce10')
#' plot(plotset) #Average plot
#' plot(plotset[1,], what='h') #Heatmap
#'  
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
