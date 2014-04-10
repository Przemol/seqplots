#ifndef BIG_WIG_H
#define BIG_WIG_H

#include "rtracklayer.h"

/* The .Call entry points */

SEXP BWGFile_summary_withNA(SEXP r_filename, SEXP r_chrom, SEXP r_ranges,
                     SEXP r_size, SEXP r_type, SEXP r_default_value);

#endif
