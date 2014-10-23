getSF  <- function(genome, gr, pattern, bin, nbins, simple=TRUE, revcomp=FALSE) {
    
    grf <- trim(resize(gr, width(gr)+(bin*2), fix='center'))
    seqs <- getSeq(genome,  grf)
    
    pattern <- DNAString(pattern)
    
    hits <- vmatchPattern(pattern, seqs, algorithm="naive-exact")
    hits <- resize( as(hits, "CompressedIRangesList"), bin, fix='center')
    
    if(!revcomp) {
        cover <- coverage(hits, width=width(seqs))
    } else {
        rhits <- vmatchPattern(
            reverseComplement(pattern), seqs, algorithm="naive-exact"
        )
        rhits <- resize( as(rhits, "CompressedIRangesList"), bin, fix='center')
        cover <- coverage(hits, width=width(seqs)) + coverage(rhits, width=width(seqs))
    }
    out <- sapply(cover, function(x) {
        vec <- as.numeric(x)[(bin+1):(length(x) - bin)]
        if(length(vec) < 2) {
            rep(vec, nbins)
        } else {
            approx(vec, n=nbins)$y
        }
    })
    return( t(out) )
}
