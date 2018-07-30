# Review of terminal emulators

A terminal emulator allows a user to interact with a shell from within a GUI environment (Xorg or Wayland).

LOC metric was obtained with the following command (text files only):

```
sh> find . -type f -exec grep -Iq . {} \; -and -print0 | xargs -0 wc -l
```

Programming language breakdown was obtained with [`github-linguist`](https://github.com/github/linguist).

## xterm

A vt220 terminal emulator.

- website: 
- first release: , last release: 
- sources: 
- version control: 
- LOC: 174,973
- dependencies:
- implementation languages: C (68%), HTML (15%), Roff (11%), Perl (2%)
- features:

## uxterm

## xvt

X Window System terminal emulator.

Xvt is a VT100 terminal emulator for X.
It is intended as a replacement for `xterm(1)` for users who do not require the more esoteric features of xterm.
Specifically xvt does not implement the Tektronix 4014 emulation, session logging and [xt](https://en.wikipedia.org/wiki/X_Toolkit_Intrinsics)-style configurability.
As a result, xvt uses much less memory.
`xvt` options are mostly a subset of `xterm`.

## rxvt

A fork of xvt.

rxvt (acronym for *our extended virtual terminal*)

- website: http://rxvt.sourceforge.net/
- first release: 2000, last release: 2001
- LOC: 80,140
- dependencies:
- implementation languages: C (78%), Shell (17%), Makefile (3%)

## rxvt-unicode (a.k.a. urxvt)

A fork of rxvt.

Unlike the original `rxvt`, `rxvt-unicode` stores all text in Unicode internally. `rxvt-unicode` was born was solely because the author couldn't get `mlterm` to use one font for latin1 and another for japanese. 

- website: http://software.schmorp.de/pkg/rxvt-unicode.html
- first release: 2003, last release: 2016
- sources: http://cvs.schmorp.de/rxvt-unicode/
- version control: CVS
- LOC: 115,413
- dependencies:
- implementation languages: C (89%), Perl (4%), C++ (4%)
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





