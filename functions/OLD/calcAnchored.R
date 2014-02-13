# TODO: Add comment
# 
# Author: przemol
###############################################################################

calcAnchored <- function(input.cov, d1=200, d2=200, e=500, desc="", tss=NULL, rm0=FALSE) {	
	
	MM <- NULL
	for (j in levels(seqnames(tss))) {
		
		tss_chr <- start(tss[seqnames(tss) == j])
		if (length(tss_chr) > 0) {
			tts_chr <- end(tss[seqnames(tss) == j])
			tss_strand <- as.character(strand(tss[seqnames(tss) == j]))
			M <- matrix(NA, ncol=d1+d2+e, nrow=length(tss_chr))
			rownames(M) <- as.character(seqnames(tss[seqnames(tss) == j]))
			
			cov_chr <- input.cov[[j]]
			
			for (i in 1:length(tss_chr)) {
				if(as.logical(tss_strand[i] == "+")) {
					try( M[i,1:(d1)] 				<-  as.numeric(cov_chr[ tss_chr[i] + (-d1:-1) ])				, T )
					try( M[i,(d1+1):(e+d1)] 		<-  approx(as.numeric(cov_chr[ tss_chr[i]:tts_chr[i] ]), n=e)$y	, T )
					try( M[i,(e+d1+1):(e+d1+d2)]  	<-  as.numeric(cov_chr[ tts_chr[i] + (1:d2) ])					, T )
				} else if (as.logical(tss_strand[i] == "-")) {	
					try( M[i,1:(d1)] 				<-  as.numeric(cov_chr[ tts_chr[i] - (-d1:-1) ])				, T )
					try( M[i,(d1+1):(e+d1)] 		<-  approx(as.numeric(cov_chr[ tts_chr[i]:tss_chr[i] ]), n=e)$y	, T )
					try( M[i,(e+d1+1):(e+d1+d2)]  	<-  as.numeric(cov_chr[ tss_chr[i] - (1:d2) ])					, T )
				}
			}
			cat(j, i, length(cov_chr), "\n")
			MM <- rbind(MM, M)
		}
	}
	
	if ( rm0 ) { 
		MMf <- MM[rowSums(is.na(MM)) == 0,]
		MMf <- MMf[rowSums(is.infinite(MMf)) == 0,]
	} else {MMf <- MM}
	if ( rm0 ) { MMf <- MMf[rowSums(MMf == 0) == 0,] }
	means    <- apply(MMf, 2, function (n) {mean(n, na.rm=T)})
	stderror <- apply(MMf, 2, function (n) {sd(n, na.rm=T) / sqrt(length(n[!is.na(n)]))})
	conint   <- apply(MMf, 2, function (n) {qt(0.975, length(n[!is.na(n)])) * sd(n, na.rm=T) / sqrt(length(n[!is.na(n)]))})
	
	return( list(means=means, stderror=stderror, conint=conint, all_ind=c(-d1:(e+d2))[-(d1+1)], desc=desc, e=e) )
}

