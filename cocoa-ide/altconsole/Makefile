#
# $Log: Makefile,v $
# Revision 1.1.1.1  2003/11/17 07:14:42  gb
# initial checkin
#
#

INFO_PLIST = AltConsole-Info.plist

ifdef MAC_APP_STORE
INFO_PLIST = AltConsole-mas-Info.plist
endif

APPBASE=.
OBJECTS=main.o AltConsoleDocument.o AltConsoleDocumentController.o
RESOURCES=resource/MainMenu.nib resource/AltConsole.nib resource/Credits.rtf resource/InfoPlist.strings resource/Clear.tiff

CFLAGS=-g -O -mmacosx-version-min=10.9

AltConsole: $(OBJECTS)
	$(CC) $(CFLAGS) -o $@ $(OBJECTS) -framework Cocoa

$(APPBASE)/AltConsole.app: AltConsole $(RESOURCES) AltConsole-Info.plist
	mkdir -p $(APPBASE)
	rm -rf $(APPBASE)/AltConsole.app
	mkdir -p $(APPBASE)/AltConsole.app/Contents/Resources/English.lproj
	cp -r -p $(RESOURCES) $(APPBASE)/AltConsole.app/Contents/Resources/English.lproj
	mkdir -p $(APPBASE)/AltConsole.app/Contents/MacOS
	cp -p AltConsole $(APPBASE)/AltConsole.app/Contents/MacOS
	cp -p $(INFO_PLIST) $(APPBASE)/AltConsole.app/Contents/Info.plist
	touch $(APPBASE)/AltConsole.app

install: $(APPBASE)/AltConsole.app

clean:
	rm -f AltConsole $(OBJECTS) *~ #*

remove: clean
	rm -rf $(APPBASE)/AltConsole.app
