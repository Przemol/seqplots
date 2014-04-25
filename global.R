require(BSgenome)
GENOMES <- BSgenome:::installed.genomes(splitNameParts=TRUE)$provider_version
if( length(GENOMES) ) 
  names(GENOMES) <- gsub('^BSgenome.', '', BSgenome:::installed.genomes())