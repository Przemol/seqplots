/* Stuff that's specific for Win95 goes here. 
 *
 * This file is copyright 2002 Jim Kent, but license is hereby
 * granted for all use - public, private or commercial. */

#include <windows.h>
#include "common.h"
#include <io.h>
#include <direct.h>
#include "portable.h"
#include "_portimpl.h"          /* for cmpFileInfo */

static char const rcsid[] = "$Id: oswin9x.c,v 1.9 2008/06/27 18:46:53 markd Exp $";

/* Return how long the named file is in bytes. 
 * Return -1 if no such file. */
off_t fileSize(char *fileName)
{
int fd;
long size;
fd = _open(fileName, _O_RDONLY, 0);
if (fd < 0)
    return -1;
size = _lseek(fd, 0L, SEEK_END);
_close(fd);
return size;
}

time_t fileModTime(char *pathName)
/* Return file last modification time.  The units of
 * these may vary from OS to OS, but you can depend on
 * later files having a larger time. */
{
  struct _finddata_t fileInfo;
  if (_findfirst( pathName, &fileInfo) == -1L)
    errAbort("_findFirst failed in fileModTime: %s", pathName);
  return fileInfo.time_write;
}

void sleep1000(int milli)
/* Sleep for given number of 1000ths of second */
{
  if (milli > 0)
    {
      Sleep(milli);
    }
}

long clock1000()
/* A millisecond clock. */
{
return clock() /* 1000/CLOCKS_PER_SEC */;   /* CLOCKS_PER_SEC == 1000 for windows */
}

long clock1()
/* Second clock. */
{
return clock()/CLOCKS_PER_SEC;
}

void uglyfBreak()
/* Go into debugger. */
{
  /*
    __asm { int 3 }
  */
}

char *getCurrentDir()
/* Return current directory. */
{
static char dir[_MAX_PATH];

if( _getcwd( dir, _MAX_PATH ) == NULL )
    errnoAbort("can't get current directory");
return dir;
}

void setCurrentDir(char *newDir)
/* Set current directory.  Abort if it fails. */
{
if (_chdir(newDir) != 0)
    errnoAbort("can't to set current directory: %s", newDir);
}

boolean maybeSetCurrentDir(char *newDir)
/* Change directory, return FALSE (and set errno) if fail. */
{
  return _chdir(newDir) == 0;
}

struct slName *listDir(char *dir, char *pattern)
/* Return an alphabetized list of all files that match 
 * the wildcard pattern in directory. */
{
long hFile;
struct _finddata_t fileInfo;
struct slName *list = NULL, *name;
boolean otherDir = FALSE;
char *currentDir;

if (dir == NULL || sameString(".", dir) || sameString("", dir))
    dir = "";
else
    {
    currentDir = getCurrentDir();
    setCurrentDir(dir);
    otherDir = TRUE;
    }

if (pattern == NULL)
    pattern = "*";
if( (hFile = _findfirst( pattern, &fileInfo)) == -1L )
    return NULL;

do
    {
    if (!sameString(".", fileInfo.name) && !sameString("..", fileInfo.name))
        {
        name = newSlName(fileInfo.name);
        slAddHead(&list, name);
        }
    }
while( _findnext( hFile, &fileInfo) == 0 );
_findclose( hFile );
if (otherDir)
    setCurrentDir(currentDir);
slNameSort(&list);
return list;
}

boolean makeDir(char *dirName)
/* Make dir.  Returns TRUE on success.  Returns FALSE
 * if failed because directory exists.  Prints error
 * message and aborts on other error. */
{
  if (!CreateDirectory(dirName, NULL))
    {
      perror("");
      errAbort("Couldn't make directory %s", dirName);    
      return FALSE;
    }
  return TRUE;
}

struct fileInfo *listDirX(char *dir, char *pattern, boolean fullPath)
/* Return list of files matching wildcard pattern with
 * extra info. If full path is true then the path will be
 * included in the name of each file. */
{
  struct fileInfo *list = NULL, *el;
  long hFile;
  struct _finddata_t fileInfo;
  boolean otherDir = FALSE;
  char *currentDir;
  int dirNameSize = strlen(dir);
  int fileNameOffset = dirNameSize+1;
  char pathName[512];

  if (dir == NULL || sameString(".", dir) || sameString("", dir))
    dir = "";
  else
    {
      currentDir = getCurrentDir();
      setCurrentDir(dir);
      otherDir = TRUE;
    }

  if (pattern == NULL)
    pattern = "*";
  if( (hFile = _findfirst( pattern, &fileInfo)) == -1L )
    return NULL;

  memcpy(pathName, dir, dirNameSize);
  pathName[dirNameSize] = '/';

  do
    {
      if (!sameString(".", fileInfo.name) && !sameString("..", fileInfo.name))
        {
          char *fileName = fileInfo.name;
          strcpy(pathName+fileNameOffset, fileName);
          if (fullPath)
            fileName = pathName;
          el = newFileInfo(fileInfo.name, fileInfo.size,
                           fileInfo.attrib & _A_SUBDIR, 0,
                           fileInfo.time_access);
          slAddHead(&list, el);
        }
    }
  while( _findnext( hFile, &fileInfo) == 0 );
  _findclose( hFile );
  if (otherDir)
    setCurrentDir(currentDir);
  slSort(&list, cmpFileInfo);
  return list;
}
