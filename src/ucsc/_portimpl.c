/* rtracklayer took this utility out of portimpl.c */

#include "common.h"
#include "portable.h"

void makeDirsOnPath(char *pathName)
/* Create directory specified by pathName.  If pathName contains
 * slashes, create directory at each level of path if it doesn't
 * already exist.  Abort with error message if there's a problem.
 * (It's not considered a problem for the directory to already
 * exist. ) */
{

  /* shortcut for paths that already exist */
  if (fileExists(pathName))
    return;

  /* Make local copy of pathName. */
  int len = strlen(pathName);
  char pathCopy[len+1];
  strcpy(pathCopy, pathName);

  /* Tolerate double-slashes in path, everyone else does it. */

  /* Start at root if it's an absolute path name. */
  char *s = pathCopy, *e;
  while (*s++ == '/')
    /* do nothing */;

  /* Step through it one slash at a time 
   * making directory if possible, else dying. */
  for (; !isEmpty(s); s = e)
    {
      /* Find end of this section and terminate string there. */
      e = strchr(s, '/');
      if (e != NULL)
	*e = 0;
      makeDir(pathCopy);
      if (e != NULL)
	*e++ = '/';
    }
}

/* some shared utilities (osunix.c) */

struct fileInfo *newFileInfo(char *name, off_t size, bool isDir, int statErrno, 
                             time_t lastAccess)
/* Return a new fileInfo. */
{
  int len = strlen(name);
  struct fileInfo *fi = needMem(sizeof(*fi) + len);
  fi->size = size;
  fi->isDir = isDir;
  fi->statErrno = statErrno;
  fi->lastAccess = lastAccess;
  strcpy(fi->name, name);
  return fi;
}

int cmpFileInfo(const void *va, const void *vb)
/* Compare two fileInfo. */
{
  const struct fileInfo *a = *((struct fileInfo **)va);
  const struct fileInfo *b = *((struct fileInfo **)vb);
  return strcmp(a->name, b->name);
}
