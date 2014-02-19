toGFF <- function(ex, fname) {
  writeLines(paste(  
    as.character(chrom(ex)), #seqname - The name of the sequence. Must be a chromosome or scaffold.
    '.', #source - The program that generated this feature.
    '.', #feature - The name of this type of feature. Some examples of standard feature types are "CDS", "start_codon", "stop_codon", and "exon".
    start(ex), #start - The starting position of the feature in the sequence. The first base is numbered 1.
    end(ex), #end - The ending position of the feature (inclusive).
    if( is.null(score(ex)) ) '.' else score(ex), #score - A score between 0 and 1000. If the track line useScore attribute is set to 1 for this annotation data set, the score value will determine the level of gray in which this feature is displayed (higher numbers = darker gray). If there is no score value, enter ".".
    as.character(strand(ex)), #strand - Valid entries include '+', '-', or '.' (for don't know/don't care).
    '.', #frame - If the feature is a coding exon, frame should be a number between 0-2 that represents the reading frame of the first base. If the feature is not a coding exon, the value should be '.'.
    '.', #group - All lines with the same group are linked together into a single item.
    sep='\t'), fname) 
}