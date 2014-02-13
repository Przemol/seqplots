require(BSgenome)
GENOMES <- BSgenome:::installed.genomes(splitNameParts=TRUE)$provider_version
names(GENOMES) <- gsub('^BSgenome.', '', BSgenome:::installed.genomes())