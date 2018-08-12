#include <X11/X.h> /* Window, Cursor, Drawable */
#include <X11/XKBlib.h>
#include <X11/Xatom.h>
#include <X11/Xft/Xft.h>
#include <X11/Xlib.h> /* XColor, XGCValues, XSetLocaleModifiers */
#include <X11/cursorfont.h>
#include <X11/keysym.h>
#include <errno.h>
#include <libgen.h>
#include <limits.h>
#include <locale.h> /* setlocale */
#include <math.h>
#include <signal.h>
#include <stdio.h>
#include <sys/select.h>
#include <time.h>
#include <unistd.h> /* getpid */

/*

Black Window is a terminal emulator for X11.


X Render Extension (Render)


FreeType2


Xft

*/

#include "bw.h"
#include "win.h"

/* types used in config.h */
typedef struct {
  uint mod;
  KeySym keysym;
  void (*func)(const Arg *);
  const Arg arg;
} Shortcut;

typedef struct {
  uint b;
  uint mask;
  char *s;
} MouseShortcut;

typedef struct {
  KeySym k;
  uint mask;
  char *s;
  /* three-valued logic variables: 0 indifferent, 1 on, -1 off */
  signed char appkey;    /* application keypad */
  signed char appcursor; /* application cursor */
} Key;

/* X modifiers */
#define XK_ANY_MOD UINT_MAX
#define XK_NO_MOD 0
#define XK_SWITCH_MOD (1 << 13)

/* function definitions used in config.h */
static void clipcopy(const Arg *);
static void clippaste(const Arg *);
static void numlock(const Arg *);
static void selpaste(const Arg *);
static void zoom(const Arg *);
static void zoomabs(const Arg *);
static void zoomreset(const Arg *);

/* config.h for applying patches and the configuration. */
#include "config.h"

/* XEMBED messages */
#define XEMBED_FOCUS_IN 4
#define XEMBED_FOCUS_OUT 5

/* macros */
#define IS_SET(flag) ((term_window.mode & (flag)) != 0)
#define TRUERED(x) (((x)&0xff0000) >> 8)
#define TRUEGREEN(x) (((x)&0xff00))
#define TRUEBLUE(x) (((x)&0xff) << 8)

/* Purely graphic info */
typedef struct {
  int tty_width;
  int tty_height;
  int window_width;
  int window_height;
  int char_width;
  int char_height;
  int mode; /* window state/mode flags */
} TermWindow;

typedef struct {
  Display *display;
  Colormap cmap;
  Window window;
  Drawable buf;
  XftGlyphFontSpec *specbuf; /* font spec buffer used for rendering */
  Atom xembed, wmdeletewin, netwmname, netwmpid;
  XIM xim;
  XIC xic;
  XftDraw *draw;
  Visual *visual;
  XSetWindowAttributes attrs;
  int screen;
  int left;
  int top;
} XWindow;

typedef struct {
  Atom xtarget;
  char *primary, *clipboard;
  struct timespec tclick1;
  struct timespec tclick2;
} XSelection;

typedef struct {
  int height;
  int width;
  int ascent;
  int descent;
  int badslant;
  int badweight;
  short lbearing;
  short rbearing;
  XftFont *match;
  FcFontSet *set;
  FcPattern *pattern;
} FontDescriptor;

typedef struct {
  XftColor *col;
  size_t collen;
  FontDescriptor font;
  FontDescriptor bfont;
  FontDescriptor ifont;
  FontDescriptor ibfont;
  GC gc;
} DrawingContext;

static inline ushort sixd_to_16bit(int);
static int x_make_glyph_font_specs(XftGlyphFontSpec *, const Character *, int,
                                   int, int);
static void x_draw_glyph_font_specs(const XftGlyphFontSpec *, Character, int,
                                    int, int);
static void x_draw_character(Character, int, int);
static void xclear(int, int, int, int);
static int xgeommasktogravity(int);
static void xinit(int, int);
static void cresize(int, int);
static void xresize(int, int);
static void xhints(void);
static int xloadcolor(int, const char *, XftColor *);
static int xloadfont(FontDescriptor *, FcPattern *);
static void xloadfonts(char *, double);
static void xunloadfont(FontDescriptor *);
static void xunloadfonts(void);
static void xsetenv(void);
static void xseturgency(int);
static int evcol(XEvent *);
static int evrow(XEvent *);

static void expose(XEvent *);
static void visibility(XEvent *);
static void unmap(XEvent *);
static void kpress(XEvent *);
static void cmessage(XEvent *);
static void resize(XEvent *);
static void focus(XEvent *);
static void brelease(XEvent *);
static void bpress(XEvent *);
static void bmotion(XEvent *);
static void propnotify(XEvent *);
static void selnotify(XEvent *);
static void selclear_(XEvent *);
static void selrequest(XEvent *);
static void setsel(char *, Time);
static void mousesel(XEvent *, int);
static void mousereport(XEvent *);
static char *kmap(KeySym, uint);
static int match(uint, uint);

static void run(void);
static void usage(void);

static void (*handler[LASTEvent])(XEvent *) = {
    [KeyPress] = kpress,
    [ClientMessage] = cmessage,
    [ConfigureNotify] = resize,
    [VisibilityNotify] = visibility,
    [UnmapNotify] = unmap,
    [Expose] = expose,
    [FocusIn] = focus,
    [FocusOut] = focus,
    [MotionNotify] = bmotion,
    [ButtonPress] = bpress,
    [ButtonRelease] = brelease,
    /*
     * Uncomment if you want the selection to disappear when you select
     * something different in another window.
     */
    /*	[SelectionClear] = selclear_, */
    [SelectionNotify] = selnotify,
    /*
     * PropertyNotify is only turned on when there is some INCR transfer
     * happening for the selection retrieval.
     */
    [PropertyNotify] = propnotify,
    [SelectionRequest] = selrequest,
};

/* Globals */
static DrawingContext drawing_context;
static XWindow x_window;
static XSelection xsel;
static TermWindow term_window;

/* Font Ring Cache */
enum { FRC_NORMAL, FRC_ITALIC, FRC_BOLD, FRC_ITALICBOLD };

typedef struct {
  XftFont *font;
  int flags;
  uint32_t unicodep;
} Fontcache;

/* Fontcache is an array now. A new font will be appended to the array. */
static Fontcache frc[16];
static int frclen = 0;
static double usedfontsize = 0;
static double defaultfontsize = 0;

static char **opt_slave = NULL;

static int oldbutton = 3; /* button event on startup: 3 = release */

void clipcopy(const Arg *dummy) {
  Atom clipboard;

  free(xsel.clipboard);
  xsel.clipboard = NULL;

  if (xsel.primary != NULL) {
    xsel.clipboard = xstrdup(xsel.primary);
    clipboard = XInternAtom(x_window.display, "CLIPBOARD", 0);
    XSetSelectionOwner(x_window.display, clipboard, x_window.window,
                       CurrentTime);
  }
}

void clippaste(const Arg *dummy) {
  Atom clipboard;

  clipboard = XInternAtom(x_window.display, "CLIPBOARD", 0);
  XConvertSelection(x_window.display, clipboard, xsel.xtarget, clipboard,
                    x_window.window, CurrentTime);
}

void selpaste(const Arg *dummy) {
  XConvertSelection(x_window.display, XA_PRIMARY, xsel.xtarget, XA_PRIMARY,
                    x_window.window, CurrentTime);
}

void numlock(const Arg *dummy) { term_window.mode ^= MODE_NUMLOCK; }

void zoom(const Arg *arg) {
  Arg larg;

  larg.f = usedfontsize + arg->f;
  zoomabs(&larg);
}

void zoomabs(const Arg *arg) {
  xunloadfonts();
  xloadfonts(font, arg->f);
  cresize(0, 0);
  redraw();
  xhints();
}

void zoomreset(const Arg *arg) {
  Arg larg;

  if (defaultfontsize > 0) {
    larg.f = defaultfontsize;
    zoomabs(&larg);
  }
}

int evcol(XEvent *e) {
  int x = e->xbutton.x - borderpx;
  LIMIT(x, 0, term_window.tty_width - 1);
  return x / term_window.char_width;
}

int evrow(XEvent *e) {
  int y = e->xbutton.y - borderpx;
  LIMIT(y, 0, term_window.tty_height - 1);
  return y / term_window.char_height;
}

void mousesel(XEvent *e, int done) {
  int type, seltype = SEL_REGULAR;
  uint state = e->xbutton.state & ~(Button1Mask | forceselmod);

  for (type = 1; type < LEN(selmasks); ++type) {
    if (match(selmasks[type], state)) {
      seltype = type;
      break;
    }
  }
  selextend(evcol(e), evrow(e), seltype, done);
  if (done)
    setsel(getsel(), e->xbutton.time);
}

void mousereport(XEvent *e) {
  int len, x = evcol(e), y = evrow(e), button = e->xbutton.button,
           state = e->xbutton.state;
  char buf[40];
  static int ox, oy;

  /* from urxvt */
  if (e->xbutton.type == MotionNotify) {
    if (x == ox && y == oy)
      return;
    if (!IS_SET(MODE_MOUSEMOTION) && !IS_SET(MODE_MOUSEMANY))
      return;
    /* MOUSE_MOTION: no reporting if no button is pressed */
    if (IS_SET(MODE_MOUSEMOTION) && oldbutton == 3)
      return;

    button = oldbutton + 32;
    ox = x;
    oy = y;
  } else {
    if (!IS_SET(MODE_MOUSESGR) && e->xbutton.type == ButtonRelease) {
      button = 3;
    } else {
      button -= Button1;
      if (button >= 3)
        button += 64 - 3;
    }
    if (e->xbutton.type == ButtonPress) {
      oldbutton = button;
      ox = x;
      oy = y;
    } else if (e->xbutton.type == ButtonRelease) {
      oldbutton = 3;
      /* MODE_MOUSEX10: no button release reporting */
      if (IS_SET(MODE_MOUSEX10))
        return;
      if (button == 64 || button == 65)
        return;
    }
  }

  if (!IS_SET(MODE_MOUSEX10)) {
    button += ((state & ShiftMask) ? 4 : 0) + ((state & Mod4Mask) ? 8 : 0) +
              ((state & ControlMask) ? 16 : 0);
  }

  if (IS_SET(MODE_MOUSESGR)) {
    len = snprintf(buf, sizeof(buf), "\033[<%d;%d;%d%c", button, x + 1, y + 1,
                   e->xbutton.type == ButtonRelease ? 'm' : 'M');
  } else if (x < 223 && y < 223) {
    len = snprintf(buf, sizeof(buf), "\033[M%c%c%c", 32 + button, 32 + x + 1,
                   32 + y + 1);
  } else {
    return;
  }

  ttywrite(buf, len, 0);
}

void bpress(XEvent *e) {
  struct timespec now;
  MouseShortcut *ms;
  int snap;

  if (IS_SET(MODE_MOUSE) && !(e->xbutton.state & forceselmod)) {
    mousereport(e);
    return;
  }

  for (ms = mshortcuts; ms < mshortcuts + LEN(mshortcuts); ms++) {
    if (e->xbutton.button == ms->b && match(ms->mask, e->xbutton.state)) {
      ttywrite(ms->s, strlen(ms->s), 1);
      return;
    }
  }

  if (e->xbutton.button == Button1) {
    /*
     * If the user clicks below predefined timeouts specific
     * snapping behaviour is exposed.
     */
    clock_gettime(CLOCK_MONOTONIC, &now);
    if (TIMEDIFF(now, xsel.tclick2) <= tripleclicktimeout) {
      snap = SNAP_LINE;
    } else if (TIMEDIFF(now, xsel.tclick1) <= doubleclicktimeout) {
      snap = SNAP_WORD;
    } else {
      snap = 0;
    }
    xsel.tclick2 = xsel.tclick1;
    xsel.tclick1 = now;

    selstart(evcol(e), evrow(e), snap);
  }
}

void propnotify(XEvent *e) {
  XPropertyEvent *xpev;
  Atom clipboard = XInternAtom(x_window.display, "CLIPBOARD", 0);

  xpev = &e->xproperty;
  if (xpev->state == PropertyNewValue &&
      (xpev->atom == XA_PRIMARY || xpev->atom == clipboard)) {
    selnotify(e);
  }
}

void selnotify(XEvent *e) {
  ulong nitems, ofs, rem;
  int format;
  uchar *data, *last, *repl;
  Atom type, incratom, property = None;

  incratom = XInternAtom(x_window.display, "INCR", 0);

  ofs = 0;
  if (e->type == SelectionNotify)
    property = e->xselection.property;
  else if (e->type == PropertyNotify)
    property = e->xproperty.atom;

  if (property == None)
    return;

  do {
    if (XGetWindowProperty(x_window.display, x_window.window, property, ofs,
                           BUFSIZ / 4, False, AnyPropertyType, &type, &format,
                           &nitems, &rem, &data)) {
      fprintf(stderr, "Clipboard allocation failed\n");
      return;
    }

    if (e->type == PropertyNotify && nitems == 0 && rem == 0) {
      /*
       * If there is some PropertyNotify with no data, then
       * this is the signal of the selection owner that all
       * data has been transferred. We won't need to receive
       * PropertyNotify events anymore.
       */
      MODBIT(x_window.attrs.event_mask, 0, PropertyChangeMask);
      XChangeWindowAttributes(x_window.display, x_window.window, CWEventMask,
                              &x_window.attrs);
    }

    if (type == incratom) {
      /*
       * Activate the PropertyNotify events so we receive
       * when the selection owner does send us the next
       * chunk of data.
       */
      MODBIT(x_window.attrs.event_mask, 1, PropertyChangeMask);
      XChangeWindowAttributes(x_window.display, x_window.window, CWEventMask,
                              &x_window.attrs);

      /*
       * Deleting the property is the transfer start signal.
       */
      XDeleteProperty(x_window.display, x_window.window, (int)property);
      continue;
    }

    /*
     * As seen in getsel:
     * Line endings are inconsistent in the terminal and GUI world
     * copy and pasting. When receiving some selection data,
     * replace all '\n' with '\r'.
     * FIXME: Fix the computer world.
     */
    repl = data;
    last = data + nitems * format / 8;
    while ((repl = memchr(repl, '\n', last - repl))) {
      *repl++ = '\r';
    }

    if (IS_SET(MODE_BRCKTPASTE) && ofs == 0)
      ttywrite("\033[200~", 6, 0);
    ttywrite((char *)data, nitems * format / 8, 1);
    if (IS_SET(MODE_BRCKTPASTE) && rem == 0)
      ttywrite("\033[201~", 6, 0);
    XFree(data);
    /* number of 32-bit chunks returned */
    ofs += nitems * format / 32;
  } while (rem > 0);

  /*
   * Deleting the property again tells the selection owner to send the
   * next data chunk in the property.
   */
  XDeleteProperty(x_window.display, x_window.window, (int)property);
}

void xclipcopy(void) { clipcopy(NULL); }

void selclear_(XEvent *e) { selclear(); }

void selrequest(XEvent *e) {
  XSelectionRequestEvent *xsre;
  XSelectionEvent xev;
  Atom xa_targets, string, clipboard;
  char *seltext;

  xsre = (XSelectionRequestEvent *)e;
  xev.type = SelectionNotify;
  xev.requestor = xsre->requestor;
  xev.selection = xsre->selection;
  xev.target = xsre->target;
  xev.time = xsre->time;
  if (xsre->property == None)
    xsre->property = xsre->target;

  /* reject */
  xev.property = None;

  xa_targets = XInternAtom(x_window.display, "TARGETS", 0);
  if (xsre->target == xa_targets) {
    /* respond with the supported type */
    string = xsel.xtarget;
    XChangeProperty(xsre->display, xsre->requestor, xsre->property, XA_ATOM, 32,
                    PropModeReplace, (uchar *)&string, 1);
    xev.property = xsre->property;
  } else if (xsre->target == xsel.xtarget || xsre->target == XA_STRING) {
    /*
     * xith XA_STRING non ascii characters may be incorrect in the
     * requestor. It is not our problem, use utf8.
     */
    clipboard = XInternAtom(x_window.display, "CLIPBOARD", 0);
    if (xsre->selection == XA_PRIMARY) {
      seltext = xsel.primary;
    } else if (xsre->selection == clipboard) {
      seltext = xsel.clipboard;
    } else {
      fprintf(stderr, "Unhandled clipboard selection 0x%lx\n", xsre->selection);
      return;
    }
    if (seltext != NULL) {
      XChangeProperty(xsre->display, xsre->requestor, xsre->property,
                      xsre->target, 8, PropModeReplace, (uchar *)seltext,
                      strlen(seltext));
      xev.property = xsre->property;
    }
  }

  /* all done, send a notification to the listener */
  if (!XSendEvent(xsre->display, xsre->requestor, 1, 0, (XEvent *)&xev))
    fprintf(stderr, "Error sending SelectionNotify event\n");
}

void setsel(char *str, Time t) {
  if (!str)
    return;

  free(xsel.primary);
  xsel.primary = str;

  XSetSelectionOwner(x_window.display, XA_PRIMARY, x_window.window, t);
  if (XGetSelectionOwner(x_window.display, XA_PRIMARY) != x_window.window)
    selclear();
}

void xsetsel(char *str) { setsel(str, CurrentTime); }

void brelease(XEvent *e) {
  if (IS_SET(MODE_MOUSE) && !(e->xbutton.state & forceselmod)) {
    mousereport(e);
    return;
  }

  if (e->xbutton.button == Button2)
    selpaste(NULL);
  else if (e->xbutton.button == Button1)
    mousesel(e, 1);
}

void bmotion(XEvent *e) {
  if (IS_SET(MODE_MOUSE) && !(e->xbutton.state & forceselmod)) {
    mousereport(e);
    return;
  }

  mousesel(e, 0);
}

void cresize(int width, int height) {
  int col, row;

  if (width != 0)
    term_window.window_width = width;
  if (height != 0)
    term_window.window_height = height;

  col = (term_window.window_width - 2 * borderpx) / term_window.char_width;
  row = (term_window.window_height - 2 * borderpx) / term_window.char_height;
  col = MAX(1, col);
  row = MAX(1, row);

  tresize(col, row);
  xresize(col, row);
  ttyresize(term_window.tty_width, term_window.tty_height);
}

void xresize(int col, int row) {
  term_window.tty_width = col * term_window.char_width;
  term_window.tty_height = row * term_window.char_height;

  XFreePixmap(x_window.display, x_window.buf);
  x_window.buf =
      XCreatePixmap(x_window.display, x_window.window, term_window.window_width,
                    term_window.window_height,
                    DefaultDepth(x_window.display, x_window.screen));
  XftDrawChange(x_window.draw, x_window.buf);
  xclear(0, 0, term_window.window_width, term_window.window_height);

  /* resize to new width */
  x_window.specbuf = xrealloc(x_window.specbuf, col * sizeof(XftGlyphFontSpec));
}

ushort sixd_to_16bit(int x) { return x == 0 ? 0 : 0x3737 + 0x2828 * x; }

int xloadcolor(int i, const char *name, XftColor *ncolor) {
  XRenderColor color = {.alpha = 0xffff};

  if (!name) {
    if (BETWEEN(i, 16, 255)) {  /* 256 color */
      if (i < 6 * 6 * 6 + 16) { /* same colors as xterm */
        color.red = sixd_to_16bit(((i - 16) / 36) % 6);
        color.green = sixd_to_16bit(((i - 16) / 6) % 6);
        color.blue = sixd_to_16bit(((i - 16) / 1) % 6);
      } else { /* greyscale */
        color.red = 0x0808 + 0x0a0a * (i - (6 * 6 * 6 + 16));
        color.green = color.blue = color.red;
      }
      return XftColorAllocValue(x_window.display, x_window.visual,
                                x_window.cmap, &color, ncolor);
    } else
      name = colorname[i];
  }

  return XftColorAllocName(x_window.display, x_window.visual, x_window.cmap,
                           name, ncolor);
}

void xloadcols(void) {
  int i;
  static int loaded;
  XftColor *cp;

  drawing_context.collen = MAX(LEN(colorname), 256);
  drawing_context.col = xmalloc(drawing_context.collen * sizeof(XftColor));

  if (loaded) {
    for (cp = drawing_context.col;
         cp < &drawing_context.col[drawing_context.collen]; ++cp)
      XftColorFree(x_window.display, x_window.visual, x_window.cmap, cp);
  }

  for (i = 0; i < drawing_context.collen; i++)
    if (!xloadcolor(i, NULL, &drawing_context.col[i])) {
      if (colorname[i])
        die("could not allocate color '%s'\n", colorname[i]);
      else
        die("could not allocate color %d\n", i);
    }
  loaded = 1;
}

int xsetcolorname(int x, const char *name) {
  XftColor ncolor;

  if (!BETWEEN(x, 0, drawing_context.collen))
    return 1;

  if (!xloadcolor(x, name, &ncolor))
    return 1;

  XftColorFree(x_window.display, x_window.visual, x_window.cmap,
               &drawing_context.col[x]);
  drawing_context.col[x] = ncolor;

  return 0;
}

/*
 * Absolute coordinates.
 */
void xclear(int x1, int y1, int x2, int y2) {
  XftDrawRect(
      x_window.draw,
      &drawing_context.col[IS_SET(MODE_REVERSE) ? defaultfg : defaultbg], x1,
      y1, x2 - x1, y2 - y1);
}

void xhints(void) {
  XClassHint class = {termname, termname};
  XWMHints wm = {.flags = InputHint, .input = 1};
  XSizeHints *sizeh;

  sizeh = XAllocSizeHints();

  sizeh->flags = PSize | PResizeInc | PBaseSize | PMinSize;
  sizeh->height = term_window.window_height;
  sizeh->width = term_window.window_width;
  sizeh->height_inc = term_window.char_height;
  sizeh->width_inc = term_window.char_width;
  sizeh->base_height = 2 * borderpx;
  sizeh->base_width = 2 * borderpx;
  sizeh->min_height = term_window.char_height + 2 * borderpx;
  sizeh->min_width = term_window.char_width + 2 * borderpx;
  XSetWMProperties(x_window.display, x_window.window, NULL, NULL, NULL, 0,
                   sizeh, &wm, &class);
  XFree(sizeh);
}

int xloadfont(FontDescriptor *f, FcPattern *pattern) {
  FcPattern *configured;
  FcPattern *match;
  FcResult result;
  XGlyphInfo extents;
  int wantattr, haveattr;

  /*
   * Manually configure instead of calling XftMatchFont
   * so that we can use the configured pattern for
   * "missing glyph" lookups.
   */
  configured = FcPatternDuplicate(pattern);
  if (!configured)
    return 1;

  FcConfigSubstitute(NULL, configured, FcMatchPattern);
  XftDefaultSubstitute(x_window.display, x_window.screen, configured);

  match = FcFontMatch(NULL, configured, &result);
  if (!match) {
    FcPatternDestroy(configured);
    return 1;
  }

  if (!(f->match = XftFontOpenPattern(x_window.display, match))) {
    FcPatternDestroy(configured);
    FcPatternDestroy(match);
    return 1;
  }

  if ((XftPatternGetInteger(pattern, "slant", 0, &wantattr) ==
       XftResultMatch)) {
    /*
     * Check if xft was unable to find a font with the appropriate
     * slant but gave us one anyway. Try to mitigate.
     */
    if ((XftPatternGetInteger(f->match->pattern, "slant", 0, &haveattr) !=
         XftResultMatch) ||
        haveattr < wantattr) {
      f->badslant = 1;
      fputs("font slant does not match\n", stderr);
    }
  }

  if ((XftPatternGetInteger(pattern, "weight", 0, &wantattr) ==
       XftResultMatch)) {
    if ((XftPatternGetInteger(f->match->pattern, "weight", 0, &haveattr) !=
         XftResultMatch) ||
        haveattr != wantattr) {
      f->badweight = 1;
      fputs("font weight does not match\n", stderr);
    }
  }

  XftTextExtentsUtf8(x_window.display, f->match,
                     (const FcChar8 *)ascii_printable, strlen(ascii_printable),
                     &extents);

  f->set = NULL;
  f->pattern = configured;

  f->ascent = f->match->ascent;
  f->descent = f->match->descent;
  f->lbearing = 0;
  f->rbearing = f->match->max_advance_width;

  f->height = f->ascent + f->descent;
  f->width = DIVCEIL(extents.xOff, strlen(ascii_printable));

  return 0;
}

void xloadfonts(char *fontstr, double fontsize) {
  FcPattern *pattern;
  double fontval;

  if (fontstr[0] == '-')
    pattern = XftXlfdParse(fontstr, False, False);
  else
    pattern = FcNameParse((FcChar8 *)fontstr);

  if (!pattern)
    die("can't open font %s\n", fontstr);

  if (fontsize > 1) {
    FcPatternDel(pattern, FC_PIXEL_SIZE);
    FcPatternDel(pattern, FC_SIZE);
    FcPatternAddDouble(pattern, FC_PIXEL_SIZE, (double)fontsize);
    usedfontsize = fontsize;
  } else {
    if (FcPatternGetDouble(pattern, FC_PIXEL_SIZE, 0, &fontval) ==
        FcResultMatch) {
      usedfontsize = fontval;
    } else if (FcPatternGetDouble(pattern, FC_SIZE, 0, &fontval) ==
               FcResultMatch) {
      usedfontsize = -1;
    } else {
      /*
       * Default font size is 12, if none given. This is to
       * have a known usedfontsize value.
       */
      FcPatternAddDouble(pattern, FC_PIXEL_SIZE, 12);
      usedfontsize = 12;
    }
    defaultfontsize = usedfontsize;
  }

  if (xloadfont(&drawing_context.font, pattern))
    die("can't open font %s\n", fontstr);

  if (usedfontsize < 0) {
    FcPatternGetDouble(drawing_context.font.match->pattern, FC_PIXEL_SIZE, 0,
                       &fontval);
    usedfontsize = fontval;
    if (fontsize == 0)
      defaultfontsize = fontval;
  }

  /* Setting character width and height. */
  term_window.char_width = ceilf(drawing_context.font.width * cwscale);
  term_window.char_height = ceilf(drawing_context.font.height * chscale);

  FcPatternDel(pattern, FC_SLANT);
  FcPatternAddInteger(pattern, FC_SLANT, FC_SLANT_ITALIC);
  if (xloadfont(&drawing_context.ifont, pattern))
    die("can't open font %s\n", fontstr);

  FcPatternDel(pattern, FC_WEIGHT);
  FcPatternAddInteger(pattern, FC_WEIGHT, FC_WEIGHT_BOLD);
  if (xloadfont(&drawing_context.ibfont, pattern))
    die("can't open font %s\n", fontstr);

  FcPatternDel(pattern, FC_SLANT);
  FcPatternAddInteger(pattern, FC_SLANT, FC_SLANT_ROMAN);
  if (xloadfont(&drawing_context.bfont, pattern))
    die("can't open font %s\n", fontstr);

  FcPatternDestroy(pattern);
}

void xunloadfont(FontDescriptor *f) {
  XftFontClose(x_window.display, f->match);
  FcPatternDestroy(f->pattern);
  if (f->set)
    FcFontSetDestroy(f->set);
}

void xunloadfonts(void) {
  /* Free the loaded fonts in the font cache.  */
  while (frclen > 0)
    XftFontClose(x_window.display, frc[--frclen].font);

  xunloadfont(&drawing_context.font);
  xunloadfont(&drawing_context.bfont);
  xunloadfont(&drawing_context.ifont);
  xunloadfont(&drawing_context.ibfont);
}

void xinit(int cols, int rows) {
  // X graphic context values
  XGCValues gcvalues;
  Cursor cursor_id;
  Window parent_window_id;
  pid_t thispid = getpid();
  XColor xmousefg, xmousebg;

  // Connect to X server. Use environment variable DISPLAY for address.
  if (!(x_window.display = XOpenDisplay(NULL)))
    die("can't open display\n");
  x_window.screen = XDefaultScreen(x_window.display);
  x_window.visual = XDefaultVisual(x_window.display, x_window.screen);

  /* font */
  if (!FcInit())
    die("could not init fontconfig.\n");

  xloadfonts(font, 0);

  /* colors */
  x_window.cmap = XDefaultColormap(x_window.display, x_window.screen);
  xloadcols();

  /* adjust fixed window geometry */
  term_window.window_width = 2 * borderpx + cols * term_window.char_width;
  term_window.window_height = 2 * borderpx + rows * term_window.char_height;

  /* Events */
  x_window.attrs.background_pixel = drawing_context.col[defaultbg].pixel;
  x_window.attrs.border_pixel = drawing_context.col[defaultbg].pixel;
  x_window.attrs.bit_gravity = NorthWestGravity;
  x_window.attrs.event_mask = FocusChangeMask | KeyPressMask | ExposureMask |
                              VisibilityChangeMask | StructureNotifyMask |
                              ButtonMotionMask | ButtonPressMask |
                              ButtonReleaseMask;
  x_window.attrs.colormap = x_window.cmap;

  parent_window_id = XRootWindow(x_window.display, x_window.screen);
  x_window.window = XCreateWindow(
      x_window.display, parent_window_id, x_window.left, x_window.top,
      term_window.window_width, term_window.window_height, 0,
      XDefaultDepth(x_window.display, x_window.screen), InputOutput,
      x_window.visual,
      CWBackPixel | CWBorderPixel | CWBitGravity | CWEventMask | CWColormap,
      &x_window.attrs);

  memset(&gcvalues, 0, sizeof(gcvalues));
  gcvalues.graphics_exposures = False;
  drawing_context.gc = XCreateGC(x_window.display, parent_window_id,
                                 GCGraphicsExposures, &gcvalues);
  x_window.buf =
      XCreatePixmap(x_window.display, x_window.window, term_window.window_width,
                    term_window.window_height,
                    DefaultDepth(x_window.display, x_window.screen));
  XSetForeground(x_window.display, drawing_context.gc,
                 drawing_context.col[defaultbg].pixel);
  XFillRectangle(x_window.display, x_window.buf, drawing_context.gc, 0, 0,
                 term_window.window_width, term_window.window_height);

  /* font spec buffer */
  x_window.specbuf = xmalloc(cols * sizeof(XftGlyphFontSpec));

  /* Xft rendering context */
  x_window.draw = XftDrawCreate(x_window.display, x_window.buf, x_window.visual,
                                x_window.cmap);

  /* input methods */
  if ((x_window.xim = XOpenIM(x_window.display, NULL, NULL, NULL)) == NULL) {
    XSetLocaleModifiers("@im=local");
    if ((x_window.xim = XOpenIM(x_window.display, NULL, NULL, NULL)) == NULL) {
      XSetLocaleModifiers("@im=");
      if ((x_window.xim = XOpenIM(x_window.display, NULL, NULL, NULL)) ==
          NULL) {
        die("XOpenIM failed. Could not open input"
            " device.\n");
      }
    }
  }
  x_window.xic = XCreateIC(
      x_window.xim, XNInputStyle, XIMPreeditNothing | XIMStatusNothing,
      XNClientWindow, x_window.window, XNFocusWindow, x_window.window, NULL);
  if (x_window.xic == NULL)
    die("XCreateIC failed. Could not obtain input method.\n");

  /* white cursor, black outline */
  cursor_id = XCreateFontCursor(x_window.display, mouseshape);
  XDefineCursor(x_window.display, x_window.window, cursor_id);

  if (XParseColor(x_window.display, x_window.cmap, colorname[mousefg],
                  &xmousefg) == 0) {
    xmousefg.red = 0xffff;
    xmousefg.green = 0xffff;
    xmousefg.blue = 0xffff;
  }

  if (XParseColor(x_window.display, x_window.cmap, colorname[mousebg],
                  &xmousebg) == 0) {
    xmousebg.red = 0x0000;
    xmousebg.green = 0x0000;
    xmousebg.blue = 0x0000;
  }

  XRecolorCursor(x_window.display, cursor_id, &xmousefg, &xmousebg);

  x_window.xembed = XInternAtom(x_window.display, "_XEMBED", False);
  x_window.wmdeletewin =
      XInternAtom(x_window.display, "WM_DELETE_WINDOW", False);
  x_window.netwmname = XInternAtom(x_window.display, "_NET_WM_NAME", False);
  XSetWMProtocols(x_window.display, x_window.window, &x_window.wmdeletewin, 1);

  x_window.netwmpid = XInternAtom(x_window.display, "_NET_WM_PID", False);
  XChangeProperty(x_window.display, x_window.window, x_window.netwmpid,
                  XA_CARDINAL, 32, PropModeReplace, (uchar *)&thispid, 1);

  term_window.mode = MODE_NUMLOCK;
  resettitle();
  XMapWindow(x_window.display, x_window.window);
  xhints();
  XSync(x_window.display, False);

  clock_gettime(CLOCK_MONOTONIC, &xsel.tclick1);
  clock_gettime(CLOCK_MONOTONIC, &xsel.tclick2);
  xsel.primary = NULL;
  xsel.clipboard = NULL;
  xsel.xtarget = XInternAtom(x_window.display, "UTF8_STRING", 0);
  if (xsel.xtarget == None)
    xsel.xtarget = XA_STRING;
}

int x_make_glyph_font_specs(XftGlyphFontSpec *specs,
                            const Character *characters, int len, int x,
                            int y) {
  FontDescriptor *font = &drawing_context.font;
  int frcflags = FRC_NORMAL;
  float runewidth = term_window.char_width;
  FcResult fcres;
  FcPattern *fcpattern;
  FcPattern *fontpattern;
  FcFontSet *fcsets[] = {NULL};
  FcCharSet *fccharset;
  int numspecs = 0;

  float winx = borderpx + x * term_window.char_width;
  float winy = borderpx + y * term_window.char_height;
  float xp;
  float yp;
  int i;
  ushort prevmode = USHRT_MAX;
  for (i = 0, xp = winx, yp = winy + font->ascent; i < len; ++i) {
    Character current_character = characters[i];

    /* Skip dummy wide-character spacing. */
    if (current_character.mode == ATTR_WDUMMY)
      continue;

    /* Determine font for character if different from previous character. */
    if (prevmode != current_character.mode) {
      prevmode = current_character.mode;
      font = &drawing_context.font;
      frcflags = FRC_NORMAL;
      runewidth = term_window.char_width *
                  ((current_character.mode & ATTR_WIDE) ? 2.0f : 1.0f);
      if ((current_character.mode & ATTR_ITALIC) &&
          (current_character.mode & ATTR_BOLD)) {
        font = &drawing_context.ibfont;
        frcflags = FRC_ITALICBOLD;
      } else if (current_character.mode & ATTR_ITALIC) {
        font = &drawing_context.ifont;
        frcflags = FRC_ITALIC;
      } else if (current_character.mode & ATTR_BOLD) {
        font = &drawing_context.bfont;
        frcflags = FRC_BOLD;
      }
      yp = winy + font->ascent;
    }

    /* Lookup character index with default font. */
    FT_UInt glyphidx = XftCharIndex(x_window.display, font->match,
                                    current_character.utf32_code_point);
    if (glyphidx) {
      specs[numspecs].font = font->match;
      specs[numspecs].glyph = glyphidx;
      specs[numspecs].x = (short)xp;
      specs[numspecs].y = (short)yp;
      xp += runewidth;
      numspecs++;
      continue;
    }

    /* Fallback on font cache, search the font cache for match. */
    int f;
    for (f = 0; f < frclen; f++) {
      glyphidx = XftCharIndex(x_window.display, frc[f].font,
                              current_character.utf32_code_point);
      /* Everything correct. */
      if (glyphidx && frc[f].flags == frcflags)
        break;
      /* We got a default font for a not found glyph. */
      if (!glyphidx && frc[f].flags == frcflags &&
          frc[f].unicodep == current_character.utf32_code_point) {
        break;
      }
    }

    /* Nothing was found. Use fontconfig to find matching font. */
    if (f >= frclen) {
      if (!font->set)
        font->set = FcFontSort(0, font->pattern, 1, 0, &fcres);
      fcsets[0] = font->set;

      /*
       * Nothing was found in the cache. Now use
       * some dozen of Fontconfig calls to get the
       * font for one single character.
       *
       * Xft and fontconfig are design failures.
       */
      fcpattern = FcPatternDuplicate(font->pattern);
      fccharset = FcCharSetCreate();

      FcCharSetAddChar(fccharset, current_character.utf32_code_point);
      FcPatternAddCharSet(fcpattern, FC_CHARSET, fccharset);
      FcPatternAddBool(fcpattern, FC_SCALABLE, 1);

      FcConfigSubstitute(0, fcpattern, FcMatchPattern);
      FcDefaultSubstitute(fcpattern);

      fontpattern = FcFontSetMatch(0, fcsets, 1, fcpattern, &fcres);

      /*
       * Overwrite or create the new cache entry.
       */
      if (frclen >= LEN(frc)) {
        frclen = LEN(frc) - 1;
        XftFontClose(x_window.display, frc[frclen].font);
        frc[frclen].unicodep = 0;
      }

      frc[frclen].font = XftFontOpenPattern(x_window.display, fontpattern);
      if (!frc[frclen].font)
        die("XftFontOpenPattern failed seeking fallback font: %s\n",
            strerror(errno));
      frc[frclen].flags = frcflags;
      frc[frclen].unicodep = current_character.utf32_code_point;

      glyphidx = XftCharIndex(x_window.display, frc[frclen].font,
                              current_character.utf32_code_point);

      f = frclen;
      frclen++;

      FcPatternDestroy(fcpattern);
      FcCharSetDestroy(fccharset);
    }

    specs[numspecs].font = frc[f].font;
    specs[numspecs].glyph = glyphidx;
    specs[numspecs].x = (short)xp;
    specs[numspecs].y = (short)yp;
    xp += runewidth;
    numspecs++;
  }

  return numspecs;
}

void x_draw_glyph_font_specs(const XftGlyphFontSpec *specs, Character base,
                             int len, int x, int y) {
  int charlen = len * ((base.mode & ATTR_WIDE) ? 2 : 1);
  int winx = borderpx + x * term_window.char_width,
      winy = borderpx + y * term_window.char_height,
      width = charlen * term_window.char_width;
  XftColor *fg, *bg, *temp, revfg, revbg, truefg, truebg;
  XRenderColor colfg, colbg;
  XRectangle r;

  /* Fallback on color display for attributes not supported by the font */
  if (base.mode & ATTR_ITALIC && base.mode & ATTR_BOLD) {
    if (drawing_context.ibfont.badslant || drawing_context.ibfont.badweight)
      base.fg = defaultattr;
  } else if ((base.mode & ATTR_ITALIC && drawing_context.ifont.badslant) ||
             (base.mode & ATTR_BOLD && drawing_context.bfont.badweight)) {
    base.fg = defaultattr;
  }

  if (IS_TRUECOL(base.fg)) {
    colfg.alpha = 0xffff;
    colfg.red = TRUERED(base.fg);
    colfg.green = TRUEGREEN(base.fg);
    colfg.blue = TRUEBLUE(base.fg);
    XftColorAllocValue(x_window.display, x_window.visual, x_window.cmap, &colfg,
                       &truefg);
    fg = &truefg;
  } else {
    fg = &drawing_context.col[base.fg];
  }

  if (IS_TRUECOL(base.bg)) {
    colbg.alpha = 0xffff;
    colbg.green = TRUEGREEN(base.bg);
    colbg.red = TRUERED(base.bg);
    colbg.blue = TRUEBLUE(base.bg);
    XftColorAllocValue(x_window.display, x_window.visual, x_window.cmap, &colbg,
                       &truebg);
    bg = &truebg;
  } else {
    bg = &drawing_context.col[base.bg];
  }

  /* Change basic system colors [0-7] to bright system colors [8-15] */
  if ((base.mode & ATTR_BOLD_FAINT) == ATTR_BOLD && BETWEEN(base.fg, 0, 7))
    fg = &drawing_context.col[base.fg + 8];

  if (IS_SET(MODE_REVERSE)) {
    if (fg == &drawing_context.col[defaultfg]) {
      fg = &drawing_context.col[defaultbg];
    } else {
      colfg.red = ~fg->color.red;
      colfg.green = ~fg->color.green;
      colfg.blue = ~fg->color.blue;
      colfg.alpha = fg->color.alpha;
      XftColorAllocValue(x_window.display, x_window.visual, x_window.cmap,
                         &colfg, &revfg);
      fg = &revfg;
    }

    if (bg == &drawing_context.col[defaultbg]) {
      bg = &drawing_context.col[defaultfg];
    } else {
      colbg.red = ~bg->color.red;
      colbg.green = ~bg->color.green;
      colbg.blue = ~bg->color.blue;
      colbg.alpha = bg->color.alpha;
      XftColorAllocValue(x_window.display, x_window.visual, x_window.cmap,
                         &colbg, &revbg);
      bg = &revbg;
    }
  }

  if ((base.mode & ATTR_BOLD_FAINT) == ATTR_FAINT) {
    colfg.red = fg->color.red / 2;
    colfg.green = fg->color.green / 2;
    colfg.blue = fg->color.blue / 2;
    colfg.alpha = fg->color.alpha;
    XftColorAllocValue(x_window.display, x_window.visual, x_window.cmap, &colfg,
                       &revfg);
    fg = &revfg;
  }

  if (base.mode & ATTR_REVERSE) {
    temp = fg;
    fg = bg;
    bg = temp;
  }

  if (base.mode & ATTR_BLINK && term_window.mode & MODE_BLINK)
    fg = bg;

  if (base.mode & ATTR_INVISIBLE)
    fg = bg;

  /* Intelligent cleaning up of the borders. */
  if (x == 0) {
    xclear(0, (y == 0) ? 0 : winy, borderpx,
           winy + term_window.char_height +
               ((winy + term_window.char_height >=
                 borderpx + term_window.tty_height)
                    ? term_window.window_height
                    : 0));
  }
  if (winx + width >= borderpx + term_window.tty_width) {
    xclear(
        winx + width, (y == 0) ? 0 : winy, term_window.window_width,
        ((winy + term_window.char_height >= borderpx + term_window.tty_height)
             ? term_window.window_height
             : (winy + term_window.char_height)));
  }
  if (y == 0)
    xclear(winx, 0, winx + width, borderpx);
  if (winy + term_window.char_height >= borderpx + term_window.tty_height)
    xclear(winx, winy + term_window.char_height, winx + width,
           term_window.window_height);

  /* Clean up the region we want to draw to. */
  XftDrawRect(x_window.draw, bg, winx, winy, width, term_window.char_height);

  /* Set the clip region because Xft is sometimes dirty. */
  r.x = 0;
  r.y = 0;
  r.height = term_window.char_height;
  r.width = width;
  XftDrawSetClipRectangles(x_window.draw, winx, winy, &r, 1);

  /* Render the glyphs. */
  XftDrawGlyphFontSpec(x_window.draw, fg, specs, len);

  /* Render underline and strikethrough. */
  if (base.mode & ATTR_UNDERLINE) {
    XftDrawRect(x_window.draw, fg, winx, winy + drawing_context.font.ascent + 1,
                width, 1);
  }

  if (base.mode & ATTR_STRUCK) {
    XftDrawRect(x_window.draw, fg, winx,
                winy + 2 * drawing_context.font.ascent / 3, width, 1);
  }

  /* Reset clip to none. */
  XftDrawSetClip(x_window.draw, 0);
}

void x_draw_character(Character character, int x, int y) {
  XftGlyphFontSpec spec;
  int numspecs = x_make_glyph_font_specs(/*specs=*/&spec, /*glyphs=*/&character,
                                         /*length=*/1, x, y);
  x_draw_glyph_font_specs(&spec, character, numspecs, x, y);
}

void x_draw_cursor(int cursor_x, int cursor_y, Character cursor_glyph,
                   int old_cursor_x, int old_cursor_y,
                   Character old_cursor_glyph) {
  XftColor color;

  /* remove the old cursor */
  if (selected(old_cursor_x, old_cursor_y))
    old_cursor_glyph.mode ^= ATTR_REVERSE;
  x_draw_character(old_cursor_glyph, old_cursor_x, old_cursor_y);

  if (IS_SET(MODE_HIDE))
    return;

  /*
   * Select the right color for the right mode.
   */
  cursor_glyph.mode &=
      ATTR_BOLD | ATTR_ITALIC | ATTR_UNDERLINE | ATTR_STRUCK | ATTR_WIDE;

  if (IS_SET(MODE_REVERSE)) {
    cursor_glyph.mode |= ATTR_REVERSE;
    cursor_glyph.bg = defaultfg;
    if (selected(cursor_x, cursor_y)) {
      color = drawing_context.col[defaultcs];
      cursor_glyph.fg = defaultrcs;
    } else {
      color = drawing_context.col[defaultrcs];
      cursor_glyph.fg = defaultcs;
    }
  } else {
    if (selected(cursor_x, cursor_y)) {
      cursor_glyph.fg = defaultfg;
      cursor_glyph.bg = defaultrcs;
    } else {
      cursor_glyph.fg = defaultbg;
      cursor_glyph.bg = defaultcs;
    }
    color = drawing_context.col[cursor_glyph.bg];
  }

  /* draw the new one if the window is focused */
  if (term_window.mode & MODE_FOCUSED) {
    x_draw_character(cursor_glyph, cursor_x, cursor_y);
  }
}

void xsetenv(void) {
  char buf[sizeof(long) * 8 + 1];

  snprintf(buf, sizeof(buf), "%lu", x_window.window);
  setenv("WINDOWID", buf, 1);
}

void xsettitle(char *p) {
  XTextProperty prop;
  DEFAULT(p, "Black Window");

  Xutf8TextListToTextProperty(x_window.display, &p, 1, XUTF8StringStyle, &prop);
  XSetWMName(x_window.display, x_window.window, &prop);
  XSetTextProperty(x_window.display, x_window.window, &prop,
                   x_window.netwmname);
  XFree(prop.value);
}

int xstartdraw(void) { return IS_SET(MODE_VISIBLE); }

void xdrawline(Line line, int x1, int y1, int x2) {
  int i, x, ox, numspecs;
  Character base, new;
  XftGlyphFontSpec *specs = x_window.specbuf;

  numspecs = x_make_glyph_font_specs(specs, &line[x1], x2 - x1, x1, y1);
  i = ox = 0;
  for (x = x1; x < x2 && i < numspecs; x++) {
    new = line[x];
    if (new.mode == ATTR_WDUMMY)
      continue;
    if (selected(x, y1))
      new.mode ^= ATTR_REVERSE;
    if (i > 0 && ATTRCMP(base, new)) {
      x_draw_glyph_font_specs(specs, base, i, ox, y1);
      specs += i;
      numspecs -= i;
      i = 0;
    }
    if (i == 0) {
      ox = x;
      base = new;
    }
    i++;
  }
  if (i > 0)
    x_draw_glyph_font_specs(specs, base, i, ox, y1);
}

void xfinishdraw(void) {
  XCopyArea(x_window.display, x_window.buf, x_window.window, drawing_context.gc,
            0, 0, term_window.window_width, term_window.window_height, 0, 0);
  XSetForeground(
      x_window.display, drawing_context.gc,
      drawing_context.col[IS_SET(MODE_REVERSE) ? defaultfg : defaultbg].pixel);
}

void expose(XEvent *ev) { redraw(); }

void visibility(XEvent *ev) {
  XVisibilityEvent *e = &ev->xvisibility;

  MODBIT(term_window.mode, e->state != VisibilityFullyObscured, MODE_VISIBLE);
}

void unmap(XEvent *ev) { term_window.mode &= ~MODE_VISIBLE; }

void xsetpointermotion(int set) {
  MODBIT(x_window.attrs.event_mask, set, PointerMotionMask);
  XChangeWindowAttributes(x_window.display, x_window.window, CWEventMask,
                          &x_window.attrs);
}

void xsetmode(int set, unsigned int flags) {
  int mode = term_window.mode;
  MODBIT(term_window.mode, set, flags);
  if ((term_window.mode & MODE_REVERSE) != (mode & MODE_REVERSE))
    redraw();
}

void xseturgency(int add) {
  XWMHints *h = XGetWMHints(x_window.display, x_window.window);

  MODBIT(h->flags, add, XUrgencyHint);
  XSetWMHints(x_window.display, x_window.window, h);
  XFree(h);
}

void xbell(void) {
  if (!(IS_SET(MODE_FOCUSED)))
    xseturgency(1);
  if (bellvolume)
    XkbBell(x_window.display, x_window.window, bellvolume, (Atom)NULL);
}

void focus(XEvent *ev) {
  XFocusChangeEvent *e = &ev->xfocus;

  if (e->mode == NotifyGrab)
    return;

  if (ev->type == FocusIn) {
    XSetICFocus(x_window.xic);
    term_window.mode |= MODE_FOCUSED;
    xseturgency(0);
    if (IS_SET(MODE_FOCUS))
      ttywrite("\033[I", 3, 0);
  } else {
    XUnsetICFocus(x_window.xic);
    term_window.mode &= ~MODE_FOCUSED;
    if (IS_SET(MODE_FOCUS))
      ttywrite("\033[O", 3, 0);
  }
}

int match(uint mask, uint state) {
  return mask == XK_ANY_MOD || mask == (state & ~ignoremod);
}

char *kmap(KeySym k, uint state) {
  Key *kp;
  int i;

  /* Check for mapped keys out of X11 function keys. */
  for (i = 0; i < LEN(mappedkeys); i++) {
    if (mappedkeys[i] == k)
      break;
  }
  if (i == LEN(mappedkeys)) {
    if ((k & 0xFFFF) < 0xFD00)
      return NULL;
  }

  for (kp = key; kp < key + LEN(key); kp++) {
    if (kp->k != k)
      continue;

    if (!match(kp->mask, state))
      continue;

    if (IS_SET(MODE_APPKEYPAD) ? kp->appkey < 0 : kp->appkey > 0)
      continue;
    if (IS_SET(MODE_NUMLOCK) && kp->appkey == 2)
      continue;

    if (IS_SET(MODE_APPCURSOR) ? kp->appcursor < 0 : kp->appcursor > 0)
      continue;

    return kp->s;
  }

  return NULL;
}

void kpress(XEvent *ev) {
  XKeyEvent *e = &ev->xkey;
  KeySym ksym;
  char buf[32], *customkey;
  int len;
  uint32_t c;
  Status status;
  Shortcut *bp;

  if (IS_SET(MODE_KBDLOCK))
    return;

  len = XmbLookupString(x_window.xic, e, buf, sizeof buf, &ksym, &status);
  /* 1. shortcuts */
  for (bp = shortcuts; bp < shortcuts + LEN(shortcuts); bp++) {
    if (ksym == bp->keysym && match(bp->mod, e->state)) {
      bp->func(&(bp->arg));
      return;
    }
  }

  /* 2. custom keys from config.h */
  if ((customkey = kmap(ksym, e->state))) {
    ttywrite(customkey, strlen(customkey), 1);
    return;
  }

  /* 3. composed string from input method */
  if (len == 0)
    return;
  if (len == 1 && e->state & Mod1Mask) {
    if (IS_SET(MODE_8BIT)) {
      if (*buf < 0177) {
        c = *buf | 0x80;
        len = utf8_encode(c, buf);
      }
    } else {
      buf[1] = buf[0];
      buf[0] = '\033';
      len = 2;
    }
  }
  ttywrite(buf, len, 1);
}

void cmessage(XEvent *e) {
  /*
   * See xembed specs
   *  http://standards.freedesktop.org/xembed-spec/xembed-spec-latest.html
   */
  if (e->xclient.message_type == x_window.xembed && e->xclient.format == 32) {
    if (e->xclient.data.l[1] == XEMBED_FOCUS_IN) {
      term_window.mode |= MODE_FOCUSED;
      xseturgency(0);
    } else if (e->xclient.data.l[1] == XEMBED_FOCUS_OUT) {
      term_window.mode &= ~MODE_FOCUSED;
    }
  } else if (e->xclient.data.l[0] == x_window.wmdeletewin) {
    ttyhangup();
    exit(0);
  }
}

void resize(XEvent *e) {
  if (e->xconfigure.width == term_window.window_width &&
      e->xconfigure.height == term_window.window_height)
    return;

  cresize(e->xconfigure.width, e->xconfigure.height);
}

void run(void) {
  int blinkset = 0;
  int dodraw = 0;
  struct timespec drawtimeout;
  struct timespec now;
  long deltatime;

  /* Waiting for window mapping */
  XEvent event;
  int w = term_window.window_width;
  int h = term_window.window_height;
  do {
    XNextEvent(x_window.display, &event);
    /*
     * This XFilterEvent call is required because of XOpenIM. It
     * does filter out the key event and some client message for
     * the input method too.
     */
    if (XFilterEvent(&event, None))
      continue;
    if (event.type == ConfigureNotify) {
      w = event.xconfigure.width;
      h = event.xconfigure.height;
    }
  } while (event.type != MapNotify);

  int tty_master_fd = tty_new(opt_slave);
  cresize(w, h);

  struct timespec last;
  clock_gettime(CLOCK_MONOTONIC, &last);
  struct timespec lastblink = last;

  int x_fd =
      XConnectionNumber(x_window.display); // X connection file descriptor
  fd_set read_fds;
  int xev;
  struct timespec *pselect_timeout = NULL;
  for (xev = action_fps;;) {
    FD_ZERO(&read_fds);
    FD_SET(tty_master_fd, &read_fds);
    FD_SET(x_fd, &read_fds);

    if (pselect(MAX(x_fd, tty_master_fd) + 1, &read_fds, /*write_fds=*/NULL,
                /*error_fds=*/NULL, /*timeout=*/pselect_timeout,
                /*sigmask=*/NULL) < 0) {
      if (errno == EINTR)
        continue;
      die("select failed: %s\n", strerror(errno));
    }
    if (FD_ISSET(tty_master_fd, &read_fds)) {
      tty_read();
      if (blinktimeout) {
        blinkset = tattrset(ATTR_BLINK);
        if (!blinkset)
          MODBIT(term_window.mode, 0, MODE_BLINK);
      }
    }

    if (FD_ISSET(x_fd, &read_fds))
      xev = action_fps;

    clock_gettime(CLOCK_MONOTONIC, &now);
    drawtimeout.tv_sec = 0;
    drawtimeout.tv_nsec = (1000 * 1E6) / x_fps;
    pselect_timeout = &drawtimeout;

    dodraw = 0;
    if (blinktimeout && TIMEDIFF(now, lastblink) > blinktimeout) {
      tsetdirtattr(ATTR_BLINK);
      term_window.mode ^= MODE_BLINK;
      lastblink = now;
      dodraw = 1;
    }
    deltatime = TIMEDIFF(now, last);
    if (deltatime > 1000 / (xev ? x_fps : action_fps)) {
      dodraw = 1;
      last = now;
    }

    if (dodraw) {
      while (XPending(x_window.display)) {
        XNextEvent(x_window.display, &event);
        if (XFilterEvent(&event, None))
          continue;
        if (handler[event.type])
          (handler[event.type])(&event);
      }

      draw();
      XFlush(x_window.display);

      if (xev && !FD_ISSET(x_fd, &read_fds))
        xev--;
      if (!FD_ISSET(tty_master_fd, &read_fds) && !FD_ISSET(x_fd, &read_fds)) {
        if (blinkset) {
          if (TIMEDIFF(now, lastblink) > blinktimeout) {
            drawtimeout.tv_nsec = 1000;
          } else {
            drawtimeout.tv_nsec =
                (1E6 * (blinktimeout - TIMEDIFF(now, lastblink)));
          }
          drawtimeout.tv_sec = drawtimeout.tv_nsec / 1E9;
          drawtimeout.tv_nsec %= (long)1E9;
        } else {
          pselect_timeout = NULL;
        }
      }
    }
  }
}

int main(int argc, char **argv, char **envp) {
  argv++, argc--; // Skip this program's name.
  opt_slave = argv;

  // At program startup locale is set to "C".
  // We're overriding character classification locale here according to
  // environment variables. In glibc the following environment variables
  // are checked in order:
  //  - LC_ALL
  //  - LC_CTYPE
  //  - LANG
  //
  // On ArchLinux LANG is usually set to en_US.UTF-8.
  setlocale(LC_CTYPE, "");

  // The only supported modifies is `im` (input method).
  // Here we explicitly specify implementation-dependent default.
  XSetLocaleModifiers("");

  terminal_init(cols, rows);
  xinit(cols, rows);
  xsetenv();
  selinit();
  run();

  return 0;
}
