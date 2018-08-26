# Black Window

Black Window is a minimalistic X11 *terminal emulator* for pseudo-terminal devices forked from [`st`](http://st.suckless.org/).

## Features

- UTF8 channel communication with slaves.
- Display of Unicode characters (granted that necessary fonts installed on the system).
- Alternate screen (for programs like `less (1)`).

## Dependencies

- `fontconfig` (`libfontconfig`)
- `freetype2` (`libfreetype`)
- `X11` (`libX11`).

## Readings

The original ECMA-48 standard (also approved as ISO/IEC 6429) describes the basic
teletype terminal concepts.

- https://www.ecma-international.org/publications/standards/Ecma-048.htm

`xterm` implements a set of control functions which are not officially part of the
standard, but are used ubiquitously, so they are de-facto standards.

- http://invisible-island.net/xterm/ctlseqs/ctlseqs.html
- https://vt100.net/docs/vt100-ug/
- https://www.gnu.org/software/screen/manual/html_node/Control-Sequences.html
