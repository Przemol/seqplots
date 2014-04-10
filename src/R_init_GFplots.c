#include "rtracklayer.h"
#include "bigWig.h"


#include <R_ext/Rdynload.h>

#define CALLMETHOD_DEF(fun, numArgs) {#fun, (DL_FUNC) &fun, numArgs}

static const R_CallMethodDef callMethods[] = {
  /* bigWig.c */

  CALLMETHOD_DEF(BWGFile_summary_withNA, 6)


};

void R_init_GFplots(DllInfo *info)
{
  R_registerRoutines(info, NULL, callMethods, NULL, NULL);
}
