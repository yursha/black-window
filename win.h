enum win_mode {
  MODE_VISIBLE = 1 << 0,
  MODE_FOCUSED = 1 << 1,
  MODE_MOUSEBTN = 1 << 3,
  MODE_MOUSEMOTION = 1 << 4,
  MODE_REVERSE = 1 << 5,
  MODE_KBDLOCK = 1 << 6,
  MODE_MOUSESGR = 1 << 9,
  MODE_8BIT = 1 << 10,
  MODE_BLINK = 1 << 11,
  MODE_FBLINK = 1 << 12,
  MODE_FOCUS = 1 << 13,
  MODE_MOUSEX10 = 1 << 14,
  MODE_MOUSEMANY = 1 << 15,
  MODE_BRCKTPASTE = 1 << 16,
  MODE_MOUSE =
      MODE_MOUSEBTN | MODE_MOUSEMOTION | MODE_MOUSEX10 | MODE_MOUSEMANY,
};

void xbell(void);
void xclipcopy(void);
void x_draw_cursor(int, int, Character, int, int, Character);
void xdrawline(Line, int, int, int);
void xfinishdraw(void);
void xloadcols(void);
int xsetcolorname(int, const char *);
void xsettitle(char *);
void xsetmode(int, unsigned int);
void xsetpointermotion(int);
void xsetsel(char *);
int xstartdraw(void);
