/* rtracklayer took out these little utilities from cheapcgi.c */

#include "common.h"

void cgiDecode(char *in, char *out, int inLength)
/* Decode from cgi pluses-for-spaces format to normal.
 * Out will be a little shorter than in typically, and
 * can be the same buffer. */
{
  char c;
  int i;
  for (i=0; i<inLength;++i)
    {
      c = *in++;
      if (c == '+')
	*out++ = ' ';
      else if (c == '%')
	{
          int code;
          if (sscanf(in, "%2x", &code) != 1)
	    code = '?';
          in += 2;
          i += 2;
          *out++ = code;
	}
      else
	*out++ = c;
    }
  *out++ = 0;
}

char *cgiEncode(char *inString)
/* Return a cgi-encoded version of inString.
 * Alphanumerics kept as is, space translated to plus,
 * and all other characters translated to %hexVal. */
{
  char c;
  int outSize = 0;
  char *outString, *out, *in;

  if (inString == NULL)
    return(cloneString(""));

  /* Count up how long it will be */
  in = inString;
  while ((c = *in++) != 0)
    {
      if (isalnum(c) || c == ' ' || c == '.' || c == '_')
        outSize += 1;
      else
        outSize += 3;
    }
  outString = needMem(outSize+1);

  /* Encode string */
  in = inString;
  out = outString;
  while ((c = *in++) != 0)
    {
      if (isalnum(c) || c == '.' || c == '_')
        *out++ = c;
      else if (c == ' ')
        *out++ = '+';
      else
        {
          unsigned char uc = c;
          char buf[4];
          *out++ = '%';
          safef(buf, sizeof(buf), "%02X", uc);
          *out++ = buf[0];
          *out++ = buf[1];
        }
    }
  *out++ = 0;
  return outString;
}
