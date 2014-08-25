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

  $Log: AltConsoleDocument.h,v $
  Revision 1.2  2003/11/17 07:30:39  gb
  update copyright/license

  Revision 1.1.1.1  2003/11/17 07:14:42  gb
  initial checkin

*/

#import <Cocoa/Cocoa.h>

@interface AltConsoleDocument : NSDocument <NSToolbarDelegate, NSTextViewDelegate>
{
  NSFileHandle *in, *out, *err;
  NSTextView *textView;
  unsigned outpos;
  NSDictionary *local_typing_attributes, *system_output_attributes;
  NSTextField *indicator;
  NSTimer *watchdog;
  Boolean peerDied;
}
@end
