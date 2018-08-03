.POSIX:

# bw version
VERSION = 0.8.2

# Customize below to fit your system

# paths
PREFIX = /usr/local
MANPREFIX = $(PREFIX)/share/man

X11INC = /usr/include/X11
X11LIB = /usr/lib

# includes and libs
INCS = -I$(X11INC) \
       `pkg-config --cflags fontconfig` \
       `pkg-config --cflags freetype2`
LIBS = -L$(X11LIB) -lm -lrt -lX11 -lutil -lXft \
       `pkg-config --libs fontconfig` \
       `pkg-config --libs freetype2`

# flags
CPPFLAGS = -DVERSION=\"$(VERSION)\" -D_XOPEN_SOURCE=600
STCFLAGS = $(INCS) $(CPPFLAGS) $(CFLAGS)
STLDFLAGS = $(LIBS) $(LDFLAGS)

SRC = bw.c x.c
OBJ = $(SRC:.c=.o)

all: options bw

options:
	@echo bw build options:
	@echo "CFLAGS  = $(STCFLAGS)"
	@echo "LDFLAGS = $(STLDFLAGS)"
	@echo "CC      = $(CC)"

.c.o:
	$(CC) $(STCFLAGS) -c $<

bw.o: config.h bw.h win.h
x.o: bw.h win.h

$(OBJ): config.h

bw: $(OBJ)
	$(CC) -o $@ $(OBJ) $(STLDFLAGS)

clean:
	rm -f bw $(OBJ) bw-$(VERSION).tar.gz

dist: clean
	mkdir -p bw-$(VERSION)
	cp -R FAQ LICENSE Makefile README\
		config.h bw.info bw.1 bw.h win.h $(SRC)\
		bw-$(VERSION)
	tar -cf - bw-$(VERSION) | gzip > bw-$(VERSION).tar.gz
	rm -rf bw-$(VERSION)

install: bw
	mkdir -p $(DESTDIR)$(PREFIX)/bin
	cp -f bw $(DESTDIR)$(PREFIX)/bin
	chmod 755 $(DESTDIR)$(PREFIX)/bin/bw
	mkdir -p $(DESTDIR)$(MANPREFIX)/man1
	sed "s/VERSION/$(VERSION)/g" < bw.1 > $(DESTDIR)$(MANPREFIX)/man1/bw.1
	chmod 644 $(DESTDIR)$(MANPREFIX)/man1/bw.1
	tic -sx bw.info
	@echo Please see the README file regarding the terminfo entry of bw.

uninstall:
	rm -f $(DESTDIR)$(PREFIX)/bin/bw
	rm -f $(DESTDIR)$(MANPREFIX)/man1/bw.1

.PHONY: all options clean dist install uninstall
