
CFLAGS=-g -D_WIN32_WINNT=0x500 -DPSAPI_VERSION=1 -mno-cygwin -mwindows
LIBS=-lpsapi

WaltConsole.exe: WaltConsole.c
	cc $(CFLAGS) -o $@ $< $(LIBS)

clean:
	rm -f WaltConsole.exe
