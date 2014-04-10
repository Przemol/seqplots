/* Stuff that should be in ucsc/bwgInternal.h, but wasn't, so
   rtracklayer put it here. */

void bwgCreate(struct bwgSection *sectionList, struct hash *chromSizeHash, 
               int blockSize, int itemsPerSlot, boolean doCompress,
               char *fileName);

struct bbiFile *bigWigFileOpen(char *fileName);

struct bbiInterval *bigWigIntervalQuery(struct bbiFile *bwf, char *chrom,
                                        bits32 start, bits32 end,
                                        struct lm *lm);
