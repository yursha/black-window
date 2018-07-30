# Review of terminal emulators

A terminal emulator allows a user to interact with a shell from within a GUI environment (Xorg).

## xterm

## uxterm

## rxvt

## rxvt-unicode (a.k.a. urxvt)


- website: http://software.schmorp.de/pkg/rxvt-unicode.html
- started: 2005
- motivation: Unlike the original rxvt, rxvt-unicode stores all text in Unicode internally. `rxvt-unicode` was born was solely because the author couldn't get mlterm to use one font for latin1 and another for japanese.
- author: <name>
- sources: http://cvs.schmorp.de/rxvt-unicode/
- version control: CVS
- LOC: 
- dependencies:
- implementation languages:
- features:
  + ISO/IEC 14755
  + separate font per script
  + support for X FreeType interface library (XFT)

## gnome-terminal

# Supporting libraries

## X FreeType interface library (xft)

https://freedesktop.org/wiki/Software/Xft/

It is designed to allow the FreeType rasterizer to be used with the X Rendering Extension; it is generally employed to use FreeType's anti-aliased fonts with the X Window System. Xft also depends on `fontconfig` for access to the system fonts.





