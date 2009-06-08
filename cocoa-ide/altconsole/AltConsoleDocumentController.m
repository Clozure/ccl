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

  $Log: AltConsoleDocumentController.m,v $
  Revision 1.2  2003/11/17 07:30:39  gb
  update copyright/license

  Revision 1.1.1.1  2003/11/17 07:14:42  gb
  initial checkin

*/

#import "AltConsoleDocumentController.h"
#include <Carbon/Carbon.h>
#include <sys/types.h>
#include <unistd.h>

@implementation AltConsoleDocumentController

- (id) init {
  self = [super init];
  if (self) {
    ProcessSerialNumber psn;

    console_documents = 0;
    peer_pid = getppid();
    peer_name = @"Unknown";
    if (GetProcessForPID(peer_pid, &psn) == 0) {
      CFStringRef name;
      if (CopyProcessName(&psn, &name) == 0) {
        peer_name = [[NSString stringWithString: (NSString *)name] retain];
      }
    }
    peer_herald = [[[NSString stringWithFormat: @"~/%@-%d",peer_name, peer_pid]stringByExpandingTildeInPath] retain];
  }
  return self;
}
    
-(BOOL)validateMenuItem:(NSMenuItem *)item {
 if ([item action] == @selector(newDocument:)) {
    return (console_documents == 0);
  }
  return [super validateMenuItem:item];
}

- (NSString *)herald {
  return peer_herald;
}

- (void) add_console_document {
  console_documents++;
}

- (void) remove_console_document {
  if (console_documents) {
    --console_documents;
  }
}

@end
