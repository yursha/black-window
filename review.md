# Review of terminal emulators

A terminal emulator allows a user to interact with a shell from within a GUI environment (Xorg).

## xterm

## uxterm

## xvt

X Window System terminal emulator.

## rxvt

A fork of xvt.

rxvt (acronym for *our extended virtual terminal*)

A color vt102 terminal emulator intended as an `xterm(1)` replacement for users who do not require features such as Tektronix 4014 emulation and [xt](https://en.wikipedia.org/wiki/X_Toolkit_Intrinsics)-style configurability. As a result, rxvt-unicode uses much less memory. `rxvt` options are mostly a subset of `xterm`.

## rxvt-unicode (a.k.a. urxvt)

A fork of rxvt.

Unlike the original `rxvt`, `rxvt-unicode` stores all text in Unicode internally. `rxvt-unicode` was born was solely because the author couldn't get `mlterm` to use one font for latin1 and another for japanese. 

- website: http://software.schmorp.de/pkg/rxvt-unicode.html
- started: 2005, first release: , last release:
- sources: http://cvs.schmorp.de/rxvt-unicode/
- version control: CVS
- LOC: 
- dependencies:
- implementation languages:
- features:
  + ISO/IEC 14755
  + separate font per script
  + support for X FreeType interface library (XFT)
  + comes with a client/daemon pair (urxvtd(1) and urxvtc(1)) that lets you open any number of terminal windows from within a single process, which makes startup time very fast and drastically reduces memory usage.

## gnome-terminal

# Supporting libraries

## X FreeType interface library (xft)

https://freedesktop.org/wiki/Software/Xft/

It is designed to allow the FreeType rasterizer to be used with the X Rendering Extension; it is generally employed to use FreeType's anti-aliased fonts with the X Window System. Xft also depends on `fontconfig` for access to the system fonts.





