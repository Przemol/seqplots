# TODO: Add comment
# 
# Author: przemol
###############################################################################

procTSSsimple <- function(trackfiles, filelist, rm0=FALSE, ignore_strand=FALSE, add="", range=NULL, XlimMin=500, XlimMax=2000, XlimMid=1000, type='Point Features', cat3=NULL, cat4=NULL, output=NULL) {
	n <- 1
	k <- 1
	TSS <- list()
	for (j in filelist)	{
		cat(n, ") Processing TSS file: ", basename(j), "\n", sep="")
		
		tss <- import.gff(file.path('files',j), asRangedData=F)
				
		proc <- list()
		for(i in 1:length(trackfiles) ) {
			cat("\tProcessing ChIP-seq experiment: ", trackfiles[i], "\n")
				cat3(paste('Processing:', basename(j), '@', trackfiles[i], '[', k, '/',length(filelist)*length(trackfiles), ']'))
				cat4("Loading track...")
			if( grepl("(bw|BW)$", trackfiles[i]) ) {
				bw <- BigWigFile(file.path('files', trackfiles[i]))
				select <- (tss[chrom(tss) %in% seqlevels(bw)]+max(XlimMin,XlimMax)); seqlevels(select) <- seqlevels(select)[seqlevels(select) %in% seqlevels(bw)]
				start(select)[start( select ) < 1 ] <- 1
				input.cov <- coverage( import.bw(bw, which = select, asRangedData = FALSE), weight = 'score' )
			} else {
				input.cov=get(trackfiles[i])
			}
			#browser() 
			if (type == 'Point Features') {
				cat('PointFeatuerPlot:', '\n')
				proc[[i]] <- calc(input.cov=input.cov, d1=XlimMin, d2=XlimMax, tss=tss, rm0=rm0, ignore_strand=ignore_strand, cat4=cat4, 
						desc=paste(sub("\\.(bw|BW)$", "", basename(trackfiles[i])), sub("\\.(gff|GFF)$", "", basename(j)), sep="\n@"))
			} else if (type == 'Anchored Features') {
				cat('AnchoredFeatuerPlot:', '\n')
				proc[[i]] <- calcAnchored(input.cov=input.cov, d1=XlimMin, d2=XlimMax, e=XlimMid, desc=paste(sub("\\.(bw|BW)$", "", basename(trackfiles[i])), sub("\\.(gff|GFF)$", "", basename(j)), sep="\n@"), tss=tss, rm0=rm0)
			} else if (type == 'Midpoint Features') {
				cat('Mid-PointFeatuerPlot:', '\n')
				proc[[i]] <- calcMidpoints(input.cov=input.cov, d1=XlimMin, d2=XlimMax, tss=tss, rm0=rm0, ignore_strand=ignore_strand, cat4=cat4, 
						desc=paste(sub("\\.(bw|BW)$", "", basename(trackfiles[i])), sub("\\.(gff|GFF)$", "", basename(j)), sep="\n@"))
			}
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
