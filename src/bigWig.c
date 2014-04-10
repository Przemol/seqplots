#include "ucsc/common.h"
#include "ucsc/linefile.h"
#include "ucsc/localmem.h"
#include "ucsc/hash.h"
#include "ucsc/bbiFile.h"
#include "ucsc/bigWig.h"
#include "ucsc/bwgInternal.h"
#include "ucsc/_bwgInternal.h"

#include "bigWig.h"
#include "handlers.h"

/* --- .Call ENTRY POINT --- */
SEXP BWGFile_summary_withNA(SEXP r_filename, SEXP r_chrom, SEXP r_ranges,
                     SEXP r_size, SEXP r_type, SEXP r_default_value)
{
  pushRHandlers();
  struct bbiFile * file = bigWigFileOpen((char *)CHAR(asChar(r_filename)));
  enum bbiSummaryType type =
    bbiSummaryTypeFromString((char *)CHAR(asChar(r_type)));
  double default_value = asReal(r_default_value);
  int *start = INTEGER(get_IRanges_start(r_ranges));
  int *width = INTEGER(get_IRanges_width(r_ranges));
  SEXP ans;
  
  PROTECT(ans = allocVector(VECSXP, length(r_chrom)));
  for (int i = 0; i < length(r_chrom); i++) {
    int size = INTEGER(r_size)[i];
    char *chrom = (char *)CHAR(STRING_ELT(r_chrom, i));
    SEXP r_values = allocVector(REALSXP, size);
    double *values = REAL(r_values);
    for (int j = 0; j < size; j++)
      values[j] = default_value;
    SET_VECTOR_ELT(ans, i, r_values);
    bool success = bigWigSummaryArray(file, chrom, start[i] - 1,
                                      start[i] - 1 + width[i], type, size,
                                      values);
    if (!success)
      warning("Failed to summarize range %d (%s:%d-%d)", i, chrom, start[i],
            start[i] - 1 + width[i]);
  }
  popRHandlers();
  UNPROTECT(1);
  return ans;
}


