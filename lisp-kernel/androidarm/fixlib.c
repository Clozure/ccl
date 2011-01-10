/*   Copyright (C) 2011 Clozure Associates */
/*   This file is part of Clozure CL.  */

/*   Clozure CL is licensed under the terms of the Lisp Lesser GNU Public */
/*   License , known as the LLGPL and distributed with Clozure CL as the */
/*   file "LICENSE".  The LLGPL consists of a preamble and the LGPL, */
/*   which is distributed with Clozure CL as the file "LGPL".  Where these */
/*   conflict, the preamble takes precedence.   */

/*   Clozure CL is referenced in the preamble as the "LIBRARY." */

/*   The LLGPL is also available online at */
/*   http://opensource.franz.com/preamble.html */


#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>


struct android_preload_info {
  unsigned long addr;
  char sig[4];
};


main(int argc, char **argv)
{
  struct android_preload_info info = {0, "PRE "};
  
  if (argc == 2) {
    info.addr = strtoul(argv[1],NULL,0);
    if (write(1,&info,sizeof(info)) == sizeof(info)) {
      return 0;
    }
  }
  return 1;
}

