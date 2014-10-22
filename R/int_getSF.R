getSF  <- function(genome, gr, pattern, bin, nbins, simple=TRUE, revcomp=FALSE) {
    
    grf <- trim(resize(gr, width(gr)+(bin*2), fix='center'))
    seqs <- getSeq(genome,  grf)
    
    pattern <- DNAString(pattern)
    
    hits <- vmatchPattern(pattern, seqs, algorithm="naive-exact")
    hits <- resize( as(hits, "CompressedIRangesList"), bin, fix='center')
    
    if(!revcomp) {
        out <- mapply(function(h, w) 
            approx(coverage(h, width=w)[(bin+1):(w - bin)], n=nbins)$y
            , hits, width(seqs)
        )
    } else {
        rhits <- vmatchPattern(
            reverseComplement(pattern), seqs, algorithm="naive-exact"
        )
        rhits <- resize( as(rhits, "CompressedIRangesList"), bin, fix='center')
        
        out <- mapply(function(h, hr, w) 
            approx(coverage(c(h, hr), width=w)[(bin+1):(w - bin)], n=nbins)$y
            , hits, rhits, width(seqs)
        )
    }
    return( t(out) )
}
