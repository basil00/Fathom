UNAME=$(shell uname -s)
ifeq ($(UNAME),Linux)
    TARGET=fathom.linux
endif
ifeq ($(UNAME),Darwin)
    TARGET=fathom.macosx
endif
CC=clang
STRIP=strip
CFLAGS=-std=gnu99 -O2 -Wall -D TB_NO_THREADS -D TB_NO_HW_POP_COUNT

main: $(TARGET)

fathom.linux:
	$(CC) $(CFLAGS) fathom.c tbprobe.c -o fathom.linux
	$(STRIP) fathom.linux

fathom.macosx:
	$(CC) $(CFLAGS) fathom.c tbprobe.c -o fathom.macosx
	$(STRIP) fathom.macosx

fathom.windows: CC=x86_64-w64-mingw32-gcc
fathom.windows: STRIP=x86_64-w64-mingw32-strip
fathom.windows:
	$(CC) $(CFLAGS) fathom.c tbprobe.c -o fathom.exe
	$(STRIP) fathom.exe

