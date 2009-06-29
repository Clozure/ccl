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

  $Log: AltConsoleDocument.m,v $
  Revision 1.2  2003/11/17 07:30:39  gb
  update copyright/license

  Revision 1.1.1.1  2003/11/17 07:14:42  gb
  initial checkin

*/

#import "AltConsoleDocument.h"
#import "AltConsoleDocumentController.h"
#include <sys/signal.h>

@implementation AltConsoleDocument
- (id)init {
    self = [super init];
    if (self) {
      NSMutableDictionary *dict;

      peerDied = NO;
      in = [[[NSFileHandle alloc] initWithFileDescriptor: 0] retain];
      out =[[[NSFileHandle alloc] initWithFileDescriptor: 1] retain];
      err =[[[NSFileHandle alloc] initWithFileDescriptor: 2] retain];
      dict = [[NSMutableDictionary alloc] initWithCapacity: 3];
      [dict setObject: [NSFont fontWithName: @"Courier" size:12.0] forKey: @"NSFont"];
      [dict setObject: [NSParagraphStyle defaultParagraphStyle] forKey: @"NSParagraphStyle"];
      system_output_attributes = dict;
      dict = [dict mutableCopy];
      [dict setObject: [NSFont fontWithName: @"Courier-Bold" size:12.0]
	    forKey: @"NSFont"];
      [dict setObject: [NSColor blackColor] forKey: @"NSColor"];
      local_typing_attributes = dict;
      outpos = 0;
    }
    return self;
}

- (NSString *)windowNibName {
    return @"AltConsole";
}

- (void) peerDied:(NSNotification *)notification {
  peerDied = YES;
  [indicator setStringValue:@"Disconnected"];
  [textView setEditable: NO];
  if (watchdog) {
    [watchdog invalidate];
    watchdog = nil;
  }
   [[NSApplication sharedApplication] terminate:nil];
}

- (void) gotData:(NSNotification *)notification {
  NSData *data = [[notification userInfo] objectForKey: NSFileHandleNotificationDataItem];
  if ([data length] != 0) {
    NSTextStorage *buffer_text = [textView textStorage];
    NSString *s = [[NSString alloc] initWithData: data encoding: NSASCIIStringEncoding];
    NSAttributedString *str = [[NSAttributedString alloc] initWithString: s attributes: system_output_attributes];
    int textlen;
    
    [buffer_text beginEditing];
    [buffer_text appendAttributedString: str];
    [buffer_text endEditing];
    textlen = [buffer_text length];
    [textView scrollRangeToVisible: NSMakeRange(textlen, 0)];
    [str release];
    [in readInBackgroundAndNotify];
    [self updateChangeCount: NSChangeDone];
    outpos = textlen;
  }
}


- (void) watchPeer: (id) theTimer {
  pid_t peer = getppid();

  if (kill(peer, 0) < 0) {
    [[NSNotificationCenter defaultCenter]
      postNotificationName: @"peerDied" object: nil];
  }
}


- (void)windowControllerDidLoadNib:(NSWindowController *) aController {
    [super windowControllerDidLoadNib:aController];

    NSWindow *w = [aController window];
    NSToolbar *toolbar = [[NSToolbar alloc] initWithIdentifier:@"altconsole"];

    [toolbar setDelegate:self];
    [w setToolbar:toolbar];
    [toolbar release];

    [[NSNotificationCenter defaultCenter]
     addObserver: self
     selector: @selector(gotData:)
     name: NSFileHandleReadCompletionNotification
     object: in];
    [[NSNotificationCenter defaultCenter]
     addObserver: self
     selector: @selector(peerDied:)
     name: @"peerDied"
     object: nil];
    [in readInBackgroundAndNotify];
    [textView setDelegate: self];
    [textView setContinuousSpellCheckingEnabled: NO];
    [self setFileURL: [NSURL fileURLWithPath: [[AltConsoleDocumentController sharedDocumentController] herald]]];
    [[AltConsoleDocumentController sharedDocumentController]
      add_console_document];
    watchdog = [NSTimer scheduledTimerWithTimeInterval: 1.0
			target: self
			selector: @selector(watchPeer:)
			userInfo: nil
			repeats: YES];
    [NSApp activateIgnoringOtherApps: YES];
}

- (NSData *)dataRepresentationOfType:(NSString *)aType {
  [[textView string] 
    dataUsingEncoding: NSASCIIStringEncoding
    allowLossyConversion: YES];
}

- (BOOL)loadDataRepresentation:(NSData *)data ofType:(NSString *)aType {
    return YES;
}

- (BOOL)textView: tv shouldChangeTextInRange: (NSRange)r replacementString: s {
  if (peerDied) {
    return NO;
  }
  if (r.location < outpos) {
    return NO;
  }
  [tv setTypingAttributes: local_typing_attributes];
  return YES;
}

- (BOOL)textView: tv doCommandBySelector: (SEL) aselector {
  if (! [self respondsToSelector: aselector]) {
    return NO;
  }
  [self performSelector: aselector withObject: tv];
  return YES;
}

- (void) sendString: (NSString *)s {
  [out writeData: [s dataUsingEncoding: NSASCIIStringEncoding allowLossyConversion: YES]];
}

- (void) insertNewline:(NSTextView *)tv {
  if (peerDied) {
    [tv insertNewline: self];
  } else {
    NSTextStorage *textbuf = [tv textStorage];
    int textlen = [textbuf length];
    NSString *textstring = [textbuf string];
    NSRange r = [tv selectedRange];
    int curpos = r.location, curlen = r.length;
    
    if (curpos >= outpos) {
      curpos += curlen;
      [tv setSelectedRange: NSMakeRange(curpos, 0)];
      [tv insertNewline: self];
      curpos++;
      textlen++;
      if (curpos == textlen) {
	[self sendString: [textstring substringWithRange: NSMakeRange(outpos, textlen-outpos)]];
	outpos = textlen;
      }
    } else if (curlen > 0) {
      [tv setSelectedRange: NSMakeRange(textlen,0)];
      [tv insertText: [textstring substringWithRange: r]];
      [tv scrollRangeToVisible: NSMakeRange([textbuf length],0)];
    }
  }
}

- (void) close {
  [[NSNotificationCenter defaultCenter] removeObserver: self];
  [[AltConsoleDocumentController sharedDocumentController]
    remove_console_document];
  if (watchdog) {
    [watchdog invalidate];
    watchdog = nil;
  }
  [super close];
}
  

- (NSArray *)toolbarAllowedItemIdentifiers:(NSToolbar *)toolbar
{
  [NSArray arrayWithObject:@"clear display"];
}

- (NSArray *)toolbarDefaultItemIdentifiers:(NSToolbar *)toolbar
{
  [NSArray arrayWithObject:@"clear display"];
}

- (NSToolbarItem *)toolbar:(NSToolbar *)toolbar itemForItemIdentifier:(NSString *)itemIdentifier willBeInsertedIntoToolbar:(BOOL)flag
{
  NSToolbarItem *item = [[[NSToolbarItem alloc]
			   initWithItemIdentifier:itemIdentifier] autorelease];

  if ([itemIdentifier isEqualToString:@"clear display"]) {
    [item setLabel:@"Clear Display"];
    [item setImage:[NSImage imageNamed:@"Clear"]];
    [item setTarget:self];
    [item setAction:@selector(clearDisplay:)];
  } else {
    item = nil;
  }
  return item;
}

- (void)clearDisplay:(id)sender
{
  NSTextStorage *storage = [textView textStorage];

  [storage deleteCharactersInRange:NSMakeRange(0, [storage length])];
  outpos = 0;
}

@end
