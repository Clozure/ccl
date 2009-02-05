/*
   Copyright (C) 2003 Clozure Associates
   This file is part of OpenMCL.  

   OpenMCL is licensed under the terms of the Lisp Lesser GNU Public
   License , known as the LLGPL and distributed with OpenMCL as the
   file "LICENSE".  The LLGPL consists of a preamble and the LGPL,
   which is distributed with OpenMCL as the file "LGPL".  Where these
   conflict, the preamble takes precedence.  

   OpenMCL is referenced in the preamble as the "LIBRARY."

   The LLGPL is also available online at
   http://opensource.franz.com/preamble.html

  $Log: main.m,v $
  Revision 1.3  2008/11/22 04:11:00  gb
  I -think- that we want to test the result of a call to not_detached(),
  not test the address of that function.

  Revision 1.2  2003/11/17 07:30:39  gb
  update copyright/license

*/

#import <Cocoa/Cocoa.h>
#include <sys/types.h>
#include <unistd.h>
#include <sys/errno.h>
#include <sys/fcntl.h>
#include <sys/ioctl.h>
#include <sys/stat.h>


BOOL
not_detached() {
  struct stat dev_null_stat, fd_0_stat;

  if ((fstat(0, &fd_0_stat) == 0) &&
      (stat("/dev/null", &dev_null_stat) == 0) &&
      (dev_null_stat.st_ino != fd_0_stat.st_ino)) {
    return YES;
  }
  return NO;
}

int
select_ignoring_eintr(int n, fd_set *in, fd_set *out, fd_set *err, struct timeval *tv) {
  int result;
  
  do {
    result = select(n, in, out, err, tv);
    if (result >= 0) {
      return result;
    }
    if (errno != EINTR) {
      return result;
    }
  } while (1);
  return result;
}

int main(int argc, const char *argv[]) {
  if (not_detached()) {
    int flags = fcntl(0, F_GETFL);
    fd_set in, err;
    
    /*
      It's apparently necessary to put the file descriptor into
      non-blocking mode, to keep the FIONREAD from indicating that
      data is available when we're actually at EOF.
    */
    fcntl(0, F_SETFL, flags | O_NONBLOCK);
    FD_ZERO(&in); 
    FD_ZERO(&err);
    FD_SET(0,&in);
    FD_SET(0,&err);
    
    /*
      GDB may cause the select() syscall to be interrupted if it
      attaches to us while we're blocked in select, so select until we
      win or get an error other than EINTR.
    */
    if (select_ignoring_eintr(1, &in, NULL, &err, NULL) == 1) {
      int nbytes;
      
      if ((ioctl(0, FIONREAD, &nbytes)  == 0) && (nbytes > 0)) {
	/* 
	   If we have incoming data, restore fd 0 to its previous
	   blocking state and start the Cocoa application.
	*/
        fcntl(0, F_SETFL, flags);
        return NSApplicationMain(argc, argv);
      }
    }
    fcntl(0, F_SETFL, flags);
  }
  return -1;
}
