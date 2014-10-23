getSF  <- function(genome, gr, pattern, bin, nbins, simple=TRUE, revcomp=FALSE) {
    
    grf <- trim(resize(gr, width(gr)+(bin*2), fix='center'))
    seqs <- getSeq(genome,  grf)
    
    pattern <- DNAString(pattern)
    
    hits <- vmatchPattern(pattern, seqs, algorithm="naive-exact")
    hits <- resize( as(hits, "CompressedIRangesList"), bin, fix='center')
    
    if(!revcomp) {
        out <- sapply(coverage(hits, width=width(seqs)), function(x) 
            approx(as.numeric(x)[(bin+1):(length(x) - bin)], n=nbins)$y
        )
    } else {
        rhits <- vmatchPattern(
            reverseComplement(pattern), seqs, algorithm="naive-exact"
        )
        rhits <- resize( as(rhits, "CompressedIRangesList"), bin, fix='center')
        
        cover <- coverage(hits, width=width(seqs)) + coverage(rhits, width=width(seqs))
        out <- sapply(cover, function(x) 
            approx(as.numeric(x)[(bin+1):(length(x) - bin)], n=nbins)$y
        )
    }
    return( t(out) )
}
