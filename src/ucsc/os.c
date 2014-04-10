/* Added by rtracklayer to condition on the platform */

#ifdef WIN32
#include "oswin9x.c"
#else
#include "osunix.c"
#endif
