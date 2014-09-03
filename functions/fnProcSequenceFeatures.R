getSF  <- function(genome, gr, pattern, bin, simple=TRUE, revcomp=FALSE) {
    
    sl <- median(width(gr))
    
    grf <- trim(resize(gr, width(gr)+(bin*2), fix='center'))
    seqs <- getSeq(genome,  grf)
    
    pattern <- DNAString(pattern)
    
    if (simple) {
        #Simle line plot
        hits <- unlist( vmatchPattern(pattern, seqs, algorithm="naive-exact") )
        if(revcomp) {
            hits <- c(hits, unlist( vmatchPattern(reverseComplement(pattern), seqs, algorithm="naive-exact") ))
        }
        vec <- coverage( restrict( resize(hits, bin, fix='center'), 
                                   start=1, end=sl+(2*bin), keep.all.ranges=TRUE, use.names=FALSE) ) / length(grf)
        vec <- as.numeric( as.numeric(vec)[bin:(bin+sl)] )
        
        return( rbind(vec) )
    } else {
        #Line plot wit error bars and/or heatmap
        npl_count <- function(hits, bin, seqs, sl, pattern) {
            pos <- start(rd) +     ( floor(nchar(as.character(pattern))/2) -1  )
            nviews <- max(width(seqs)) - bin + 1L
            idx <- logical(max(width(seqs)))
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
            M <- M[ ,(bin/2):((bin/2)+sl)] 
            return( M )
        }
        #Matrix-like results
        rd <- as(vmatchPattern(pattern, seqs, algorithm="naive-exact"), "CompressedIRangesList")
        out <- npl_count(rd, bin, seqs, sl, pattern)
        
        if(revcomp) {
            rd.rev <- as(vmatchPattern(reverseComplement(pattern), seqs, algorithm="naive-exact"), "CompressedIRangesList")
            out <- out + npl_count(rd.rev, bin, seqs, sl, pattern)
        }
        return( out )
    }
}
