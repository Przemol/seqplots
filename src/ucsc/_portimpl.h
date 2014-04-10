/* Utilities that rtracklayer made common between win9x and unix */

struct fileInfo *newFileInfo(char *name, off_t size, bool isDir, int statErrno, 
                             time_t lastAccess);

int cmpFileInfo(const void *va, const void *vb);
