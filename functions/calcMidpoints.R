# TODO: Add comment
# 
# Author: przemol
###############################################################################
calcMidpoints <- function(input.cov, d1=2000, d2=3000, desc="", tss=NULL, rm0=FALSE, cat4=NULL, ignore_strand=FALSE) {
	#tss <- import.gff("TSS_all_coding_WS190.gff", genome="ce6", asRangedData=F)
	MM <- NULL
	
	if (ignore_strand) {
		warning('Ignoring te strand')
		for (j in seqlevels(tss)) {
			cat('Starting: ', j, "\n")
			
			mid_chr <- mid(ranges(tss[seqnames(tss) == j]))
			if (length(mid_chr) > 0) {
				M <- matrix(NA, ncol=length(-d1:d2), nrow=length(mid_chr))
				cov_chr <- input.cov[[j]]
				
				for (i in 1:length(mid_chr)) {
					try( M[i,] <-  as.numeric(cov_chr[ mid_chr[i] + (-d1:d2) ]), T )
				}
				cat(j, i, length(cov_chr), "\n")
				cat4(paste(j, ' [',length(cov_chr),'bp]: ', i,  ' features summarized.',sep=''))
				MM <- rbind(MM, M)
			}
		}
		
	} else {
		for (j in seqlevels(tss)) {
			cat('Starting: ', j, "\n")
			
			mid_chr <- mid(ranges(tss[seqnames(tss) == j]))
			if (length(mid_chr) > 0) {
				tss_strand <- as.character(strand(tss[seqnames(tss) == j]))
				M <- matrix(NA, ncol=length(-d1:d2), nrow=length(mid_chr))
				
				cov_chr <- input.cov[[j]]
				
				for (i in 1:length(mid_chr)) {
					if(as.logical(tss_strand[i] == "+")) {
						try( M[i,] <-  as.numeric(cov_chr[ mid_chr[i] + (-d1:d2) ]), T )
					} else if (as.logical(tss_strand[i] == "-")) {
						try( M[i,] <-  as.numeric(cov_chr[ mid_chr[i] - (-d1:d2) ]), T )	
					}
				}
				cat(j, i, length(cov_chr), "\n")
				cat4(paste(j, ' [',length(cov_chr),'bp]: ', i,  ' features summarized.',sep=''))
				MM <- rbind(MM, M)
			}
		}
	}
	
	if ( rm0 ) { MMf <- MM[rowSums(is.na(MM)) == 0,] } else {MMf <- MM}
	if ( rm0 ) { MMf <- MMf[rowSums(MMf == 0) == 0,] }
	means    <- apply(MMf, 2, function (n) {mean(n, na.rm=T)})
	stderror <- apply(MMf, 2, function (n) {sd(n, na.rm=T) / sqrt(length(n[!is.na(n)]))})
	conint   <- apply(MMf, 2, function (n) {qt(0.975, length(n[!is.na(n)])) * sd(n, na.rm=T) / sqrt(length(n[!is.na(n)]))})
	
	return( list(means=means, stderror=stderror, conint=conint, all_ind=c(-d1:d2), desc=desc) )
}
