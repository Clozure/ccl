#
# $Log: Makefile,v $
# Revision 1.1.1.1  2003/11/17 07:14:42  gb
# initial checkin
#
#


APPBASE=.
OBJECTS=main.o AltConsoleDocument.o AltConsoleDocumentController.o
RESOURCES=resource/MainMenu.nib resource/AltConsole.nib resource/Credits.rtf resource/InfoPlist.strings resource/Clear.tiff
#CC=gcc-4.0

CFLAGS=-g -O -mmacosx-version-min=10.4 -m32 -Wno-protocol

AltConsole: $(OBJECTS)
	$(CC) $(CFLAGS) -o $@ $(OBJECTS) -framework Cocoa

$(APPBASE)/AltConsole.app: AltConsole $(RESOURCES) Info.plist
	mkdir -p $(APPBASE)
	rm -rf $(APPBASE)/AltConsole.app
	mkdir -p $(APPBASE)/AltConsole.app/Contents/Resources/English.lproj
	cp -r -p $(RESOURCES) $(APPBASE)/AltConsole.app/Contents/Resources/English.lproj
	mkdir -p $(APPBASE)/AltConsole.app/Contents/MacOS
	cp -p AltConsole $(APPBASE)/AltConsole.app/Contents/MacOS
	cp -p Info.plist $(APPBASE)/AltConsole.app/Contents/
	touch $(APPBASE)/AltConsole.app

install: $(APPBASE)/AltConsole.app

clean:
	rm -f AltConsole $(OBJECTS) *~ #*

remove: clean
	rm -rf $(APPBASE)/AltConsole.app
