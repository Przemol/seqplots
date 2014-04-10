#include "ucsc/common.h"
#include "ucsc/errabort.h"

#include "handlers.h"

#define WARN_BUF_SIZE 512
static void R_warnHandler(char *format, va_list args) {
  char warn_buf[WARN_BUF_SIZE];
  vsnprintf(warn_buf, WARN_BUF_SIZE, format, args);
  warning(warn_buf);
}

static void R_abortHandler() {
  error("UCSC library operation failed");
}

void pushRHandlers() {
  pushAbortHandler(R_abortHandler);
  pushWarnHandler(R_warnHandler);  
}

void popRHandlers() {
  popAbortHandler();
  popWarnHandler();
}
