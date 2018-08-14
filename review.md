# Review of terminal emulators

A terminal emulator allows a user to interact with a computer using text-only interface from within a GUI environment (Xorg or Wayland).

## Black Window

- LOC: 5,624

## [st](http://st.suckless.org/)

- LOC: 6,490

## [scurvy](https://git.sr.ht/~sircmpwn/scurvy/)

- LOC: 2,142. But it also depends on [libvterm](http://www.leonerd.org.uk/code/libvterm/), which is 10,941 LOC
- implementation languages:C (96%), Meson (4%)
- features: Wayland support

# Supporting libraries

## X FreeType interface library (xft)

https://freedesktop.org/wiki/Software/Xft/

It is designed to allow the FreeType rasterizer to be used with the X Rendering Extension; it is generally employed to use FreeType's anti-aliased fonts with the X Window System. Xft also depends on `fontconfig` for access to the system fonts.

# Terminals which don't depend on X11 or Wayland

- fbterm - 24,289 LOC, C++ (51%), Makefile (28%), Shell (11%), C (10%)
- kmscon - 106,990 LOC, C (94%), M4 (3%)

# Other terminals with Wayland support

???

# Why do we need a GUI terminal

- MouthSelect-copy-paste between a terminal and a web browser
- Freetype fonts
