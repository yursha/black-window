.POSIX:

PREFIX = /usr/local
MANPREFIX = $(PREFIX)/share/man

X11INC = /usr/include/X11
X11LIB = /usr/lib

INCS = -I$(X11INC) \
       `pkg-config --cflags fontconfig` \
       `pkg-config --cflags freetype2`
LIBS = -L$(X11LIB) -lm -lrt -lX11 -lutil -lXft \
       `pkg-config --libs fontconfig` \
       `pkg-config --libs freetype2`

CC = clang
CPPFLAGS = -D_XOPEN_SOURCE=600
STCFLAGS = -std=c11 $(INCS) $(CPPFLAGS) $(CFLAGS)
STLDFLAGS = $(LIBS) $(LDFLAGS)

SRC = bw.c x.c
OBJ = $(SRC:.c=.o)

all: bw

.c.o:
	$(CC) $(STCFLAGS) -c $<

bw.o: config.h bw.h win.h
x.o: bw.h win.h

bw: $(OBJ)
	$(CC) -o $@ $(OBJ) $(STLDFLAGS)

clean:
	rm -f bw $(OBJ)

install: bw
	mkdir -p $(DESTDIR)$(PREFIX)/bin
	cp -f bw $(DESTDIR)$(PREFIX)/bin
	chmod 755 $(DESTDIR)$(PREFIX)/bin/bw
	mkdir -p $(DESTDIR)$(MANPREFIX)/man1
	cp bw.1 $(DESTDIR)$(MANPREFIX)/man1/bw.1
	chmod 644 $(DESTDIR)$(MANPREFIX)/man1/bw.1
	tic -sx bw.info
	@echo Please see the README file regarding the terminfo entry of bw.

uninstall:
	rm -f $(DESTDIR)$(PREFIX)/bin/bw
	rm -f $(DESTDIR)$(MANPREFIX)/man1/bw.1

.PHONY: all clean dist install uninstall
