#include <ctype.h>
#include <errno.h>
#include <fcntl.h>
#include <limits.h>
#include <pty.h>
#include <pwd.h>
#include <signal.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/ioctl.h>
#include <sys/select.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <termios.h>
#include <unistd.h>
#include <wchar.h>

#include "bw.h"
#include "win.h"

/* Arbitrary sizes */
#define UTF_INVALID 0xFFFD
#define UTF8_MAX_CHAR_SIZE 4
#define ESC_BUF_SIZ (128 * UTF8_MAX_CHAR_SIZE)
#define ESC_ARG_SIZ 16
#define STR_BUF_SIZ ESC_BUF_SIZ
#define STR_ARG_SIZ ESC_ARG_SIZ

/* macros */
#define IS_SET(flag) ((terminal.mode & (flag)) != 0)
#define NUMMAXLEN(x) ((int)(sizeof(x) * 2.56 + 0.5) + 1)

/* https://en.wikipedia.org/wiki/C0_and_C1_control_codes */
#define ISCONTROLC0(c) (BETWEEN(c, 0, 0x1f) || (c) == '\177') // ISO646 (ASCII)
#define ISCONTROLC1(c) (BETWEEN(c, 0x80, 0x9f))               // ECMA-48
#define ISCONTROL(c) (ISCONTROLC0(c) || ISCONTROLC1(c))

#define ISDELIM(utf32_code_point)                                              \
  (utf8strchr(worddelimiters, utf32_code_point) != NULL)

/* constants */
#define ISO14755CMD "dmenu -w \"$WINDOWID\" -p codepoint: </dev/null"

enum term_mode {
  MODE_WRAP = 1 << 0,
  MODE_INSERT = 1 << 1,
  MODE_ALTSCREEN = 1 << 2,
  MODE_CRLF = 1 << 3,
  MODE_ECHO = 1 << 4,
  MODE_UTF8 = 1 << 5,
};

enum cursor_movement { CURSOR_SAVE, CURSOR_LOAD };

enum cursor_state {
  CURSOR_DEFAULT = 0,
  CURSOR_WRAPNEXT = 1,
  CURSOR_ORIGIN = 2
};

enum charset {
  CS_GRAPHIC0,
  CS_GRAPHIC1,
  CS_UK,
  CS_USA,
  CS_MULTI,
  CS_GER,
  CS_FIN
};

enum escape_state {
  ESC_START = 1, // Escpase Sequence Start      'ESC'
  ESC_CSI = 2,   // Control Sequence Introducer '['
  ESC_STR = 4,   /* OSC, PM, APC */
  ESC_ALTCHARSET = 8,
  ESC_STR_END = 16, /* a final string was encountered */
  ESC_TEST = 32,    /* Enter in test mode */
  ESC_UTF8 = 64,
  ESC_DCS = 128, // Device Control String       'P'
};

typedef struct {
  Character attr; /* current char attributes */
  int x;
  int y;
  char state;
} TCursor;

typedef struct {
  int mode;
  int type;
  int snap;
  /*
   * Selection variables:
   * nb – normalized coordinates of the beginning of the selection
   * ne – normalized coordinates of the end of the selection
   * ob – original coordinates of the beginning of the selection
   * oe – original coordinates of the end of the selection
   */
  struct {
    int x, y;
  } nb, ne, ob, oe;

  int alt;
} Selection;

/* Internal representation of the screen */
typedef struct {
  int row;        /* nb row */
  int col;        /* nb col */
  Line *line;     /* screen */
  Line *alt;      /* alternate screen */
  int *dirty;     /* dirtyness of lines */
  TCursor cursor; /* cursor */
  int old_cursor_x;
  int old_cursor_y;
  int top;         /* top    scroll limit */
  int bot;         /* bottom scroll limit */
  int mode;        /* terminal mode flags */
  int esc;         /* escape state flags */
  char trantbl[4]; /* charset table translation */
  int charset;     /* current charset */
  int icharset;    /* selected charset for sequence */
  int *tabs;
} Terminal;

/* CSI Escape sequence structs */
/* ESC '[' [[ [<priv>] <arg> [;]] <mode> [<mode>]] */
typedef struct {
  char buf[ESC_BUF_SIZ]; /* raw string */
  int len;               /* raw string length */
  char priv;
  int arg[ESC_ARG_SIZ];
  int narg; /* nb of args */
  char mode[2];
} CSIEscape;

/* STR Escape sequence structs */
/* ESC type [[ [<priv>] <arg> [;]] <mode>] ESC '\' */
typedef struct {
  char type;             /* ESC type ... */
  char buf[STR_BUF_SIZ]; /* raw string */
  int len;               /* raw string length */
  char *args[STR_ARG_SIZ];
  int narg; /* nb of args */
} STREscape;

static void stty(char **);
static void sigchld(int);
static void ttywriteraw(const char *, size_t);

static void csidump(void);
static void csihandle(void);
static void csiparse(void);
static void csireset(void);
static int eschandle(uchar);
static void strdump(void);
static void strhandle(void);
static void strparse(void);
static void strreset(void);

static void tdumpsel(void);
static void tdumpline(int);
static void tdump(void);
static void tclearregion(int, int, int, int);
static void tcursor(int);
static void tdeletechar(int);
static void tdeleteline(int);
static void tinsertblank(int);
static void tinsertblankline(int);
static int tlinelen(int);
static void tmoveto(int, int);
static void tmoveato(int, int);
static void tnewline(int);
static void tputtab(int);
static void tputc(uint32_t);
static void treset(void);
static void tscrollup(int, int);
static void tscrolldown(int, int);
static void tsetattr(int *, int);
static void tsetchar(uint32_t, Character *, int, int);
static void tsetdirt(int, int);
static void tsetscroll(int, int);
static void tswapscreen(void);
static void tsetmode(int, int, int *, int);
static int twrite(const char *, int, int);
static void tfulldirt(void);
static void tcontrolcode(uchar);
static void tdectest(char);
static void tdefutf8(char);
static int32_t tdefcolor(int *, int *, int);
static void tdeftran(char);
static void tstrsequence(uchar);

static void drawregion(int, int, int, int);

static void selnormalize(void);
static void selscroll(int, int);
static void selsnap(int *, int *, int);

static size_t utf8_decode(const char *, uint32_t *, size_t);
static uint32_t utf8_decode_byte(char, size_t *);
static char utf8_encode_byte(uint32_t, size_t);
static char *utf8strchr(char *, uint32_t);
static size_t utf8_validate(uint32_t *, size_t);

static char *base64dec(const char *);
static char base64dec_getc(const char **);

static ssize_t xwrite(int, const char *, size_t);

/* Globals */
static Terminal terminal;
static Selection sel;
static CSIEscape csiescseq;
static STREscape strescseq;
static int tty_master_fd;
static pid_t pid;

static uchar utfbyte[UTF8_MAX_CHAR_SIZE + 1] = {0x80, 0, 0xC0, 0xE0, 0xF0};
static uchar utfmask[UTF8_MAX_CHAR_SIZE + 1] = {0xC0, 0x80, 0xE0, 0xF0, 0xF8};
static uint32_t utfmin[UTF8_MAX_CHAR_SIZE + 1] = {0, 0, 0x80, 0x800, 0x10000};
static uint32_t utfmax[UTF8_MAX_CHAR_SIZE + 1] = {0x10FFFF, 0x7F, 0x7FF, 0xFFFF,
                                                  0x10FFFF};

void *xmalloc(size_t len) {
  void *p;

  if (!(p = malloc(len)))
    die("malloc: %s\n", strerror(errno));

  return p;
}

void *xrealloc(void *p, size_t len) {
  if ((p = realloc(p, len)) == NULL)
    die("realloc: %s\n", strerror(errno));

  return p;
}

char *xstrdup(char *s) {
  if ((s = strdup(s)) == NULL)
    die("strdup: %s\n", strerror(errno));

  return s;
}

size_t utf8_decode(const char *buffer, uint32_t *rune, size_t length) {
  size_t i, j, type;
  *rune = UTF_INVALID;
  if (!length)
    return 0;
  size_t len;
  uint32_t udecoded = utf8_decode_byte(buffer[0], &len);
  if (!BETWEEN(len, 1, UTF8_MAX_CHAR_SIZE))
    return 1;
  for (i = 1, j = 1; i < length && j < len; ++i, ++j) {
    udecoded = (udecoded << 6) | utf8_decode_byte(buffer[i], &type);
    if (type != 0)
      return j;
  }
  if (j < len)
    return 0;
  *rune = udecoded;
  utf8_validate(rune, len);

  return len;
}

uint32_t utf8_decode_byte(char c, size_t *i) {
  for (*i = 0; *i < LEN(utfmask); ++(*i))
    if (((uchar)c & utfmask[*i]) == utfbyte[*i])
      return (uchar)c & ~utfmask[*i];

  return 0;
}

size_t utf8_encode(uint32_t utf32_code_point, char *c) {
  size_t len, i;

  len = utf8_validate(&utf32_code_point, 0);
  if (len > UTF8_MAX_CHAR_SIZE)
    return 0;

  for (i = len - 1; i != 0; --i) {
    c[i] = utf8_encode_byte(utf32_code_point, 0);
    utf32_code_point >>= 6;
  }
  c[0] = utf8_encode_byte(utf32_code_point, len);

  return len;
}

char utf8_encode_byte(uint32_t utf32_code_point, size_t i) {
  return utfbyte[i] | (utf32_code_point & ~utfmask[i]);
}

char *utf8strchr(char *s, uint32_t utf32_code_point) {
  uint32_t r;
  size_t i, j, len;

  len = strlen(s);
  for (i = 0, j = 0; i < len; i += j) {
    if (!(j = utf8_decode(&s[i], &r, len - i)))
      break;
    if (r == utf32_code_point)
      return &(s[i]);
  }

  return NULL;
}

size_t utf8_validate(uint32_t *utf32_code_point, size_t i) {
  if (!BETWEEN(*utf32_code_point, utfmin[i], utfmax[i]) ||
      BETWEEN(*utf32_code_point, 0xD800, 0xDFFF))
    *utf32_code_point = UTF_INVALID;
  for (i = 1; *utf32_code_point > utfmax[i]; ++i)
    ;

  return i;
}

static const char base64_digits[] = {
    0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
    0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
    0,  0,  0,  0,  0,  62, 0,  0,  0,  63, 52, 53, 54, 55, 56, 57, 58, 59, 60,
    61, 0,  0,  0,  -1, 0,  0,  0,  0,  1,  2,  3,  4,  5,  6,  7,  8,  9,  10,
    11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 0,  0,  0,  0,
    0,  0,  26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42,
    43, 44, 45, 46, 47, 48, 49, 50, 51, 0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
    0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
    0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
    0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
    0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
    0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
    0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
    0,  0,  0,  0,  0,  0,  0,  0};

char base64dec_getc(const char **src) {
  while (**src && !isprint(**src))
    (*src)++;
  return *((*src)++);
}

char *base64dec(const char *src) {
  size_t in_len = strlen(src);
  char *result, *dst;

  if (in_len % 4)
    in_len += 4 - (in_len % 4);
  result = dst = xmalloc(in_len / 4 * 3 + 1);
  while (*src) {
    int a = base64_digits[(unsigned char)base64dec_getc(&src)];
    int b = base64_digits[(unsigned char)base64dec_getc(&src)];
    int c = base64_digits[(unsigned char)base64dec_getc(&src)];
    int d = base64_digits[(unsigned char)base64dec_getc(&src)];

    *dst++ = (a << 2) | ((b & 0x30) >> 4);
    if (c == -1)
      break;
    *dst++ = ((b & 0x0f) << 4) | ((c & 0x3c) >> 2);
    if (d == -1)
      break;
    *dst++ = ((c & 0x03) << 6) | d;
  }
  *dst = '\0';
  return result;
}

void selinit(void) {
  sel.mode = SEL_IDLE;
  sel.snap = 0;
  sel.ob.x = -1;
}

int tlinelen(int y) {
  int i = terminal.col;

  if (terminal.line[y][i - 1].mode & ATTR_WRAP)
    return i;

  while (i > 0 && terminal.line[y][i - 1].utf32_code_point == ' ')
    --i;

  return i;
}

void selstart(int col, int row, int snap) {
  selclear();
  sel.mode = SEL_EMPTY;
  sel.type = SEL_REGULAR;
  sel.alt = IS_SET(MODE_ALTSCREEN);
  sel.snap = snap;
  sel.oe.x = sel.ob.x = col;
  sel.oe.y = sel.ob.y = row;
  selnormalize();

  if (sel.snap != 0)
    sel.mode = SEL_READY;
  tsetdirt(sel.nb.y, sel.ne.y);
}

void selextend(int col, int row, int type, int done) {
  int oldey, oldex, oldsby, oldsey, oldtype;

  if (sel.mode == SEL_IDLE)
    return;
  if (done && sel.mode == SEL_EMPTY) {
    selclear();
    return;
  }

  oldey = sel.oe.y;
  oldex = sel.oe.x;
  oldsby = sel.nb.y;
  oldsey = sel.ne.y;
  oldtype = sel.type;

  sel.oe.x = col;
  sel.oe.y = row;
  selnormalize();
  sel.type = type;

  if (oldey != sel.oe.y || oldex != sel.oe.x || oldtype != sel.type)
    tsetdirt(MIN(sel.nb.y, oldsby), MAX(sel.ne.y, oldsey));

  sel.mode = done ? SEL_IDLE : SEL_READY;
}

void selnormalize(void) {
  int i;

  if (sel.type == SEL_REGULAR && sel.ob.y != sel.oe.y) {
    sel.nb.x = sel.ob.y < sel.oe.y ? sel.ob.x : sel.oe.x;
    sel.ne.x = sel.ob.y < sel.oe.y ? sel.oe.x : sel.ob.x;
  } else {
    sel.nb.x = MIN(sel.ob.x, sel.oe.x);
    sel.ne.x = MAX(sel.ob.x, sel.oe.x);
  }
  sel.nb.y = MIN(sel.ob.y, sel.oe.y);
  sel.ne.y = MAX(sel.ob.y, sel.oe.y);

  selsnap(&sel.nb.x, &sel.nb.y, -1);
  selsnap(&sel.ne.x, &sel.ne.y, +1);

  /* expand selection over line breaks */
  if (sel.type == SEL_RECTANGULAR)
    return;
  i = tlinelen(sel.nb.y);
  if (i < sel.nb.x)
    sel.nb.x = i;
  if (tlinelen(sel.ne.y) <= sel.ne.x)
    sel.ne.x = terminal.col - 1;
}

int selected(int x, int y) {
  if (sel.mode == SEL_EMPTY || sel.ob.x == -1 ||
      sel.alt != IS_SET(MODE_ALTSCREEN))
    return 0;

  if (sel.type == SEL_RECTANGULAR)
    return BETWEEN(y, sel.nb.y, sel.ne.y) && BETWEEN(x, sel.nb.x, sel.ne.x);

  return BETWEEN(y, sel.nb.y, sel.ne.y) && (y != sel.nb.y || x >= sel.nb.x) &&
         (y != sel.ne.y || x <= sel.ne.x);
}

void selsnap(int *x, int *y, int direction) {
  int newx, newy, xt, yt;
  int delim, prevdelim;
  Character *gp, *prevgp;

  switch (sel.snap) {
  case SNAP_WORD:
    /*
     * Snap around if the word wraps around at the end or
     * beginning of a line.
     */
    prevgp = &terminal.line[*y][*x];
    prevdelim = ISDELIM(prevgp->utf32_code_point);
    for (;;) {
      newx = *x + direction;
      newy = *y;
      if (!BETWEEN(newx, 0, terminal.col - 1)) {
        newy += direction;
        newx = (newx + terminal.col) % terminal.col;
        if (!BETWEEN(newy, 0, terminal.row - 1))
          break;

        if (direction > 0)
          yt = *y, xt = *x;
        else
          yt = newy, xt = newx;
        if (!(terminal.line[yt][xt].mode & ATTR_WRAP))
          break;
      }

      if (newx >= tlinelen(newy))
        break;

      gp = &terminal.line[newy][newx];
      delim = ISDELIM(gp->utf32_code_point);
      if (!(gp->mode & ATTR_WDUMMY) &&
          (delim != prevdelim ||
           (delim && gp->utf32_code_point != prevgp->utf32_code_point)))
        break;

      *x = newx;
      *y = newy;
      prevgp = gp;
      prevdelim = delim;
    }
    break;
  case SNAP_LINE:
    /*
     * Snap around if the the previous line or the current one
     * has set ATTR_WRAP at its end. Then the whole next or
     * previous line will be selected.
     */
    *x = (direction < 0) ? 0 : terminal.col - 1;
    if (direction < 0) {
      for (; *y > 0; *y += direction) {
        if (!(terminal.line[*y - 1][terminal.col - 1].mode & ATTR_WRAP)) {
          break;
        }
      }
    } else if (direction > 0) {
      for (; *y < terminal.row - 1; *y += direction) {
        if (!(terminal.line[*y][terminal.col - 1].mode & ATTR_WRAP)) {
          break;
        }
      }
    }
    break;
  }
}

char *getsel(void) {
  char *str, *ptr;
  int y, bufsize, lastx, linelen;
  Character *gp, *last;

  if (sel.ob.x == -1)
    return NULL;

  bufsize = (terminal.col + 1) * (sel.ne.y - sel.nb.y + 1) * UTF8_MAX_CHAR_SIZE;
  ptr = str = xmalloc(bufsize);

  /* append every set & selected glyph to the selection */
  for (y = sel.nb.y; y <= sel.ne.y; y++) {
    if ((linelen = tlinelen(y)) == 0) {
      *ptr++ = '\n';
      continue;
    }

    if (sel.type == SEL_RECTANGULAR) {
      gp = &terminal.line[y][sel.nb.x];
      lastx = sel.ne.x;
    } else {
      gp = &terminal.line[y][sel.nb.y == y ? sel.nb.x : 0];
      lastx = (sel.ne.y == y) ? sel.ne.x : terminal.col - 1;
    }
    last = &terminal.line[y][MIN(lastx, linelen - 1)];
    while (last >= gp && last->utf32_code_point == ' ')
      --last;

    for (; gp <= last; ++gp) {
      if (gp->mode & ATTR_WDUMMY)
        continue;

      ptr += utf8_encode(gp->utf32_code_point, ptr);
    }

    /*
     * Copy and pasting of line endings is inconsistent
     * in the inconsistent terminal and GUI world.
     * The best solution seems like to produce '\n' when
     * something is copied from st and convert '\n' to
     * '\r', when something to be pasted is received by
     * st.
     * FIXME: Fix the computer world.
     */
    if ((y < sel.ne.y || lastx >= linelen) && !(last->mode & ATTR_WRAP))
      *ptr++ = '\n';
  }
  *ptr = 0;
  return str;
}

void selclear(void) {
  if (sel.ob.x == -1)
    return;
  sel.mode = SEL_IDLE;
  sel.ob.x = -1;
  tsetdirt(sel.nb.y, sel.ne.y);
}

void die(const char *errstr, ...) {
  va_list ap;
  va_start(ap, errstr);
  vfprintf(stderr, errstr, ap);
  va_end(ap);
  exit(1);
}

void sigchld(int a) {
  int stat;
  pid_t p;

  if ((p = waitpid(pid, &stat, WNOHANG)) < 0)
    die("waiting for pid %hd failed: %s\n", pid, strerror(errno));

  if (pid != p)
    return;

  if (!WIFEXITED(stat) || WEXITSTATUS(stat))
    die("child finished with error '%d'\n", stat);
  exit(0);
}

void stty(char **args) {
  char cmd[_POSIX_ARG_MAX], **p, *q, *s;
  size_t n, siz;

  if ((n = strlen(stty_args)) > sizeof(cmd) - 1)
    die("incorrect stty parameters\n");
  memcpy(cmd, stty_args, n);
  q = cmd + n;
  siz = sizeof(cmd) - n;
  for (p = args; p && (s = *p); ++p) {
    if ((n = strlen(s)) > siz - 1)
      die("stty parameter length too long\n");
    *q++ = ' ';
    memcpy(q, s, n);
    q += n;
    siz -= n + 1;
  }
  *q = '\0';
  if (system(cmd) != 0)
    perror("Couldn't call stty");
}

// Return the file descriptor
int tty_new(char **slave_args) {
  int master;
  int slave;

  if (openpty(&master, &slave, NULL, NULL, NULL) < 0)
    die("openpty failed: %s\n", strerror(errno));

  switch (pid = fork()) {
  case -1:
    die("fork failed: %s\n", strerror(errno));
    break;
  case 0:
    setsid(); /* create a new process group */
    dup2(slave, 0);
    dup2(slave, 1);
    dup2(slave, 2);
    if (ioctl(slave, TIOCSCTTY, NULL) < 0)
      die("ioctl TIOCSCTTY failed: %s\n", strerror(errno));
    close(slave);
    close(master);
    execvp(slave_args[0], slave_args);
    break;
  default:
    close(slave);
    tty_master_fd = master;
    signal(SIGCHLD, sigchld);
    break;
  }
  return tty_master_fd;
}

size_t tty_read(void) {
  static char buffer[BUFSIZ]; // 8192 bytes
  static int length = 0;

  /* append read bytes to unprocessed bytes */
  int bytes_read = read(tty_master_fd, buffer + length, LEN(buffer) - length);
  if (bytes_read < 0)
    die("couldn't read from slave: %s\n", strerror(errno));
  length += bytes_read;

  int bytes_written = twrite(buffer, length, 0);
  length -= bytes_written;
  /* keep any uncomplete utf8 char for the next call */
  if (length > 0)
    memmove(buffer, buffer + bytes_written, length);

  return bytes_read;
}

void ttywrite(const char *s, size_t n, int may_echo) {
  const char *next;

  if (may_echo && IS_SET(MODE_ECHO))
    twrite(s, n, 1);

  if (!IS_SET(MODE_CRLF)) {
    ttywriteraw(s, n);
    return;
  }

  /* This is similar to how the kernel handles ONLCR for ttys */
  while (n > 0) {
    if (*s == '\r') {
      next = s + 1;
      ttywriteraw("\r\n", 2);
    } else {
      next = memchr(s, '\r', n);
      DEFAULT(next, s + n);
      ttywriteraw(s, next - s);
    }
    n -= next - s;
    s = next;
  }
}

void ttywriteraw(const char *s, size_t n) {
  fd_set wfd, rfd;
  ssize_t r;
  size_t lim = 256;

  /*
   * Remember that we are using a pty, which might be a modem line.
   * Writing too much will clog the line. That's why we are doing this
   * dance.
   * FIXME: Migrate the world to Plan 9.
   */
  while (n > 0) {
    FD_ZERO(&wfd);
    FD_ZERO(&rfd);
    FD_SET(tty_master_fd, &wfd);
    FD_SET(tty_master_fd, &rfd);

    /* Check if we can write. */
    if (pselect(tty_master_fd + 1, &rfd, &wfd, NULL, NULL, NULL) < 0) {
      if (errno == EINTR)
        continue;
      die("select failed: %s\n", strerror(errno));
    }
    if (FD_ISSET(tty_master_fd, &wfd)) {
      /*
       * Only write the bytes written by ttywrite() or the
       * default of 256. This seems to be a reasonable value
       * for a serial line. Bigger values might clog the I/O.
       */
      if ((r = write(tty_master_fd, s, (n < lim) ? n : lim)) < 0)
        goto write_error;
      if (r < n) {
        /*
         * We weren't able to write out everything.
         * This means the buffer is getting full
         * again. Empty it.
         */
        if (n < lim)
          lim = tty_read();
        n -= r;
        s += r;
      } else {
        /* All bytes have been written. */
        break;
      }
    }
    if (FD_ISSET(tty_master_fd, &rfd))
      lim = tty_read();
  }
  return;

write_error:
  die("write error on tty: %s\n", strerror(errno));
}

void ttyresize(int tw, int th) {
  struct winsize w;

  w.ws_row = terminal.row;
  w.ws_col = terminal.col;
  w.ws_xpixel = tw;
  w.ws_ypixel = th;
  if (ioctl(tty_master_fd, TIOCSWINSZ, &w) < 0)
    fprintf(stderr, "Couldn't set window size: %s\n", strerror(errno));
}

void ttyhangup() {
  /* Send SIGHUP to shell */
  kill(pid, SIGHUP);
}

int tattrset(int attr) {
  int i, j;

  for (i = 0; i < terminal.row - 1; i++) {
    for (j = 0; j < terminal.col - 1; j++) {
      if (terminal.line[i][j].mode & attr)
        return 1;
    }
  }

  return 0;
}

void tsetdirt(int top, int bot) {
  int i;

  LIMIT(top, 0, terminal.row - 1);
  LIMIT(bot, 0, terminal.row - 1);

  for (i = top; i <= bot; i++)
    terminal.dirty[i] = 1;
}

void tsetdirtattr(int attr) {
  int i, j;

  for (i = 0; i < terminal.row - 1; i++) {
    for (j = 0; j < terminal.col - 1; j++) {
      if (terminal.line[i][j].mode & attr) {
        tsetdirt(i, i);
        break;
      }
    }
  }
}

void tfulldirt(void) { tsetdirt(0, terminal.row - 1); }

void tcursor(int mode) {
  static TCursor cursor[2];
  int alt = IS_SET(MODE_ALTSCREEN);

  if (mode == CURSOR_SAVE) {
    cursor[alt] = terminal.cursor;
  } else if (mode == CURSOR_LOAD) {
    terminal.cursor = cursor[alt];
    tmoveto(cursor[alt].x, cursor[alt].y);
  }
}

void treset(void) {
  uint i;

  terminal.cursor =
      (TCursor){{.mode = ATTR_NULL, .fg = defaultfg, .bg = defaultbg},
                .x = 0,
                .y = 0,
                .state = CURSOR_DEFAULT};

  memset(terminal.tabs, 0, terminal.col * sizeof(*terminal.tabs));
  for (i = tabspaces; i < terminal.col; i += tabspaces)
    terminal.tabs[i] = 1;
  terminal.top = 0;
  terminal.bot = terminal.row - 1;
  terminal.mode = MODE_WRAP | MODE_UTF8;
  memset(terminal.trantbl, CS_USA, sizeof(terminal.trantbl));
  terminal.charset = 0;

  for (i = 0; i < 2; i++) {
    tmoveto(0, 0);
    tcursor(CURSOR_SAVE);
    tclearregion(0, 0, terminal.col - 1, terminal.row - 1);
    tswapscreen();
  }
}

void tswapscreen(void) {
  Line *tmp = terminal.line;

  terminal.line = terminal.alt;
  terminal.alt = tmp;
  terminal.mode ^= MODE_ALTSCREEN;
  tfulldirt();
}

void tscrolldown(int orig, int n) {
  int i;
  Line temp;

  LIMIT(n, 0, terminal.bot - orig + 1);

  tsetdirt(orig, terminal.bot - n);
  tclearregion(0, terminal.bot - n + 1, terminal.col - 1, terminal.bot);

  for (i = terminal.bot; i >= orig + n; i--) {
    temp = terminal.line[i];
    terminal.line[i] = terminal.line[i - n];
    terminal.line[i - n] = temp;
  }

  selscroll(orig, n);
}

void tscrollup(int orig, int n) {
  int i;
  Line temp;

  LIMIT(n, 0, terminal.bot - orig + 1);

  tclearregion(0, orig, terminal.col - 1, orig + n - 1);
  tsetdirt(orig + n, terminal.bot);

  for (i = orig; i <= terminal.bot - n; i++) {
    temp = terminal.line[i];
    terminal.line[i] = terminal.line[i + n];
    terminal.line[i + n] = temp;
  }

  selscroll(orig, -n);
}

void selscroll(int orig, int n) {
  if (sel.ob.x == -1)
    return;

  if (BETWEEN(sel.ob.y, orig, terminal.bot) ||
      BETWEEN(sel.oe.y, orig, terminal.bot)) {
    if ((sel.ob.y += n) > terminal.bot || (sel.oe.y += n) < terminal.top) {
      selclear();
      return;
    }
    if (sel.type == SEL_RECTANGULAR) {
      if (sel.ob.y < terminal.top)
        sel.ob.y = terminal.top;
      if (sel.oe.y > terminal.bot)
        sel.oe.y = terminal.bot;
    } else {
      if (sel.ob.y < terminal.top) {
        sel.ob.y = terminal.top;
        sel.ob.x = 0;
      }
      if (sel.oe.y > terminal.bot) {
        sel.oe.y = terminal.bot;
        sel.oe.x = terminal.col;
      }
    }
    selnormalize();
  }
}

void tnewline(int first_col) {
  int y = terminal.cursor.y;

  if (y == terminal.bot) {
    tscrollup(terminal.top, 1);
  } else {
    y++;
  }
  tmoveto(first_col ? 0 : terminal.cursor.x, y);
}

void csiparse(void) {
  char *p = csiescseq.buf, *np;
  long int v;

  csiescseq.narg = 0;
  if (*p == '?') {
    csiescseq.priv = 1;
    p++;
  }

  csiescseq.buf[csiescseq.len] = '\0';
  while (p < csiescseq.buf + csiescseq.len) {
    np = NULL;
    v = strtol(p, &np, 10);
    if (np == p)
      v = 0;
    if (v == LONG_MAX || v == LONG_MIN)
      v = -1;
    csiescseq.arg[csiescseq.narg++] = v;
    p = np;
    if (*p != ';' || csiescseq.narg == ESC_ARG_SIZ)
      break;
    p++;
  }
  csiescseq.mode[0] = *p++;
  csiescseq.mode[1] = (p < csiescseq.buf + csiescseq.len) ? *p : '\0';
}

/* for absolute user moves, when decom is set */
void tmoveato(int x, int y) {
  tmoveto(x, y + ((terminal.cursor.state & CURSOR_ORIGIN) ? terminal.top : 0));
}

void tmoveto(int x, int y) {
  int miny, maxy;

  if (terminal.cursor.state & CURSOR_ORIGIN) {
    miny = terminal.top;
    maxy = terminal.bot;
  } else {
    miny = 0;
    maxy = terminal.row - 1;
  }
  terminal.cursor.state &= ~CURSOR_WRAPNEXT;
  terminal.cursor.x = LIMIT(x, 0, terminal.col - 1);
  terminal.cursor.y = LIMIT(y, miny, maxy);
}

void tsetchar(uint32_t utf32_code_point, Character *attr, int x, int y) {
  static char *vt100_0[62] = {
      /* 0x41 - 0x7e */
      "↑", "↓", "→", "←", "█", "▚", "☃",      /* A - G */
      0,   0,   0,   0,   0,   0,   0,   0,   /* H - O */
      0,   0,   0,   0,   0,   0,   0,   0,   /* P - W */
      0,   0,   0,   0,   0,   0,   0,   " ", /* X - _ */
      "◆", "▒", "␉", "␌", "␍", "␊", "°", "±", /* ` - g */
      "␤", "␋", "┘", "┐", "┌", "└", "┼", "⎺", /* h - o */
      "⎻", "─", "⎼", "⎽", "├", "┤", "┴", "┬", /* p - w */
      "│", "≤", "≥", "π", "≠", "£", "·",      /* x - ~ */
  };

  /*
   * The table is proudly stolen from rxvt.
   */
  if (terminal.trantbl[terminal.charset] == CS_GRAPHIC0 &&
      BETWEEN(utf32_code_point, 0x41, 0x7e) && vt100_0[utf32_code_point - 0x41])
    utf8_decode(vt100_0[utf32_code_point - 0x41], &utf32_code_point,
                UTF8_MAX_CHAR_SIZE);

  if (terminal.line[y][x].mode & ATTR_WIDE) {
    if (x + 1 < terminal.col) {
      terminal.line[y][x + 1].utf32_code_point = ' ';
      terminal.line[y][x + 1].mode &= ~ATTR_WDUMMY;
    }
  } else if (terminal.line[y][x].mode & ATTR_WDUMMY) {
    terminal.line[y][x - 1].utf32_code_point = ' ';
    terminal.line[y][x - 1].mode &= ~ATTR_WIDE;
  }

  terminal.dirty[y] = 1;
  terminal.line[y][x] = *attr;
  terminal.line[y][x].utf32_code_point = utf32_code_point;
}

void tclearregion(int x1, int y1, int x2, int y2) {
  int x, y, temp;
  Character *gp;

  if (x1 > x2)
    temp = x1, x1 = x2, x2 = temp;
  if (y1 > y2)
    temp = y1, y1 = y2, y2 = temp;

  LIMIT(x1, 0, terminal.col - 1);
  LIMIT(x2, 0, terminal.col - 1);
  LIMIT(y1, 0, terminal.row - 1);
  LIMIT(y2, 0, terminal.row - 1);

  for (y = y1; y <= y2; y++) {
    terminal.dirty[y] = 1;
    for (x = x1; x <= x2; x++) {
      gp = &terminal.line[y][x];
      if (selected(x, y))
        selclear();
      gp->fg = terminal.cursor.attr.fg;
      gp->bg = terminal.cursor.attr.bg;
      gp->mode = 0;
      gp->utf32_code_point = ' ';
    }
  }
}

void tdeletechar(int n) {
  int dst, src, size;
  Character *line;

  LIMIT(n, 0, terminal.col - terminal.cursor.x);

  dst = terminal.cursor.x;
  src = terminal.cursor.x + n;
  size = terminal.col - src;
  line = terminal.line[terminal.cursor.y];

  memmove(&line[dst], &line[src], size * sizeof(Character));
  tclearregion(terminal.col - n, terminal.cursor.y, terminal.col - 1,
               terminal.cursor.y);
}

void tinsertblank(int n) {
  int dst, src, size;
  Character *line;

  LIMIT(n, 0, terminal.col - terminal.cursor.x);

  dst = terminal.cursor.x + n;
  src = terminal.cursor.x;
  size = terminal.col - dst;
  line = terminal.line[terminal.cursor.y];

  memmove(&line[dst], &line[src], size * sizeof(Character));
  tclearregion(src, terminal.cursor.y, dst - 1, terminal.cursor.y);
}

void tinsertblankline(int n) {
  if (BETWEEN(terminal.cursor.y, terminal.top, terminal.bot))
    tscrolldown(terminal.cursor.y, n);
}

void tdeleteline(int n) {
  if (BETWEEN(terminal.cursor.y, terminal.top, terminal.bot))
    tscrollup(terminal.cursor.y, n);
}

int32_t tdefcolor(int *attr, int *npar, int l) {
  int32_t idx = -1;
  uint r, g, b;

  switch (attr[*npar + 1]) {
  case 2: /* direct color in RGB space */
    if (*npar + 4 >= l) {
      fprintf(stderr, "erresc(38): Incorrect number of parameters (%d)\n",
              *npar);
      break;
    }
    r = attr[*npar + 2];
    g = attr[*npar + 3];
    b = attr[*npar + 4];
    *npar += 4;
    if (!BETWEEN(r, 0, 255) || !BETWEEN(g, 0, 255) || !BETWEEN(b, 0, 255))
      fprintf(stderr, "erresc: bad rgb color (%u,%u,%u)\n", r, g, b);
    else
      idx = TRUECOLOR(r, g, b);
    break;
  case 5: /* indexed color */
    if (*npar + 2 >= l) {
      fprintf(stderr, "erresc(38): Incorrect number of parameters (%d)\n",
              *npar);
      break;
    }
    *npar += 2;
    if (!BETWEEN(attr[*npar], 0, 255))
      fprintf(stderr, "erresc: bad fgcolor %d\n", attr[*npar]);
    else
      idx = attr[*npar];
    break;
  case 0: /* implemented defined (only foreground) */
  case 1: /* transparent */
  case 3: /* direct color in CMY space */
  case 4: /* direct color in CMYK space */
  default:
    fprintf(stderr, "erresc(38): gfx attr %d unknown\n", attr[*npar]);
    break;
  }

  return idx;
}

void tsetattr(int *attr, int l) {
  int i;
  int32_t idx;

  for (i = 0; i < l; i++) {
    switch (attr[i]) {
    case 0:
      terminal.cursor.attr.mode &=
          ~(ATTR_BOLD | ATTR_FAINT | ATTR_ITALIC | ATTR_UNDERLINE | ATTR_BLINK |
            ATTR_REVERSE | ATTR_INVISIBLE | ATTR_STRUCK);
      terminal.cursor.attr.fg = defaultfg;
      terminal.cursor.attr.bg = defaultbg;
      break;
    case 1:
      terminal.cursor.attr.mode |= ATTR_BOLD;
      break;
    case 2:
      terminal.cursor.attr.mode |= ATTR_FAINT;
      break;
    case 3:
      terminal.cursor.attr.mode |= ATTR_ITALIC;
      break;
    case 4:
      terminal.cursor.attr.mode |= ATTR_UNDERLINE;
      break;
    case 5: /* slow blink */
            /* FALLTHROUGH */
    case 6: /* rapid blink */
      terminal.cursor.attr.mode |= ATTR_BLINK;
      break;
    case 7:
      terminal.cursor.attr.mode |= ATTR_REVERSE;
      break;
    case 8:
      terminal.cursor.attr.mode |= ATTR_INVISIBLE;
      break;
    case 9:
      terminal.cursor.attr.mode |= ATTR_STRUCK;
      break;
    case 22:
      terminal.cursor.attr.mode &= ~(ATTR_BOLD | ATTR_FAINT);
      break;
    case 23:
      terminal.cursor.attr.mode &= ~ATTR_ITALIC;
      break;
    case 24:
      terminal.cursor.attr.mode &= ~ATTR_UNDERLINE;
      break;
    case 25:
      terminal.cursor.attr.mode &= ~ATTR_BLINK;
      break;
    case 27:
      terminal.cursor.attr.mode &= ~ATTR_REVERSE;
      break;
    case 28:
      terminal.cursor.attr.mode &= ~ATTR_INVISIBLE;
      break;
    case 29:
      terminal.cursor.attr.mode &= ~ATTR_STRUCK;
      break;
    case 38:
      if ((idx = tdefcolor(attr, &i, l)) >= 0)
        terminal.cursor.attr.fg = idx;
      break;
    case 39:
      terminal.cursor.attr.fg = defaultfg;
      break;
    case 48:
      if ((idx = tdefcolor(attr, &i, l)) >= 0)
        terminal.cursor.attr.bg = idx;
      break;
    case 49:
      terminal.cursor.attr.bg = defaultbg;
      break;
    default:
      if (BETWEEN(attr[i], 30, 37)) {
        terminal.cursor.attr.fg = attr[i] - 30;
      } else if (BETWEEN(attr[i], 40, 47)) {
        terminal.cursor.attr.bg = attr[i] - 40;
      } else if (BETWEEN(attr[i], 90, 97)) {
        terminal.cursor.attr.fg = attr[i] - 90 + 8;
      } else if (BETWEEN(attr[i], 100, 107)) {
        terminal.cursor.attr.bg = attr[i] - 100 + 8;
      } else {
        fprintf(stderr, "erresc(default): gfx attr %d unknown\n", attr[i]),
            csidump();
      }
      break;
    }
  }
}

void tsetscroll(int t, int b) {
  int temp;

  LIMIT(t, 0, terminal.row - 1);
  LIMIT(b, 0, terminal.row - 1);
  if (t > b) {
    temp = t;
    t = b;
    b = temp;
  }
  terminal.top = t;
  terminal.bot = b;
}

void tsetmode(int priv, int set, int *args, int narg) {
  int alt, *lim;

  for (lim = args + narg; args < lim; ++args) {
    if (priv) {
      switch (*args) {
      case 1: /* DECCKM -- Cursor key */
        xsetmode(set, MODE_APPCURSOR);
        break;
      case 5: /* DECSCNM -- Reverse video */
        xsetmode(set, MODE_REVERSE);
        break;
      case 6: /* DECOM -- Origin */
        MODBIT(terminal.cursor.state, set, CURSOR_ORIGIN);
        tmoveato(0, 0);
        break;
      case 7: /* DECAWM -- Auto wrap */
        MODBIT(terminal.mode, set, MODE_WRAP);
        break;
      case 0:  /* Error (IGNORED) */
      case 2:  /* DECANM -- ANSI/VT52 (IGNORED) */
      case 3:  /* DECCOLM -- Column  (IGNORED) */
      case 4:  /* DECSCLM -- Scroll (IGNORED) */
      case 8:  /* DECARM -- Auto repeat (IGNORED) */
      case 18: /* DECPFF -- Printer feed (IGNORED) */
      case 19: /* DECPEX -- Printer extent (IGNORED) */
      case 42: /* DECNRCM -- National characters (IGNORED) */
      case 12: /* att610 -- Start blinking cursor (IGNORED) */
        break;
      case 25: /* DECTCEM -- Text Cursor Enable Mode */
        xsetmode(!set, MODE_HIDE);
        break;
      case 9: /* X10 mouse compatibility mode */
        xsetpointermotion(0);
        xsetmode(0, MODE_MOUSE);
        xsetmode(set, MODE_MOUSEX10);
        break;
      case 1000: /* 1000: report button press */
        xsetpointermotion(0);
        xsetmode(0, MODE_MOUSE);
        xsetmode(set, MODE_MOUSEBTN);
        break;
      case 1002: /* 1002: report motion on button press */
        xsetpointermotion(0);
        xsetmode(0, MODE_MOUSE);
        xsetmode(set, MODE_MOUSEMOTION);
        break;
      case 1003: /* 1003: enable all mouse motions */
        xsetpointermotion(set);
        xsetmode(0, MODE_MOUSE);
        xsetmode(set, MODE_MOUSEMANY);
        break;
      case 1004: /* 1004: send focus events to tty */
        xsetmode(set, MODE_FOCUS);
        break;
      case 1006: /* 1006: extended reporting mode */
        xsetmode(set, MODE_MOUSESGR);
        break;
      case 1034:
        xsetmode(set, MODE_8BIT);
        break;
      case 1049: /* swap screen & set/restore cursor as xterm */
        if (!allowaltscreen)
          break;
        tcursor((set) ? CURSOR_SAVE : CURSOR_LOAD);
        /* FALLTHROUGH */
      case 47: /* swap screen */
      case 1047:
        if (!allowaltscreen)
          break;
        alt = IS_SET(MODE_ALTSCREEN);
        if (alt) {
          tclearregion(0, 0, terminal.col - 1, terminal.row - 1);
        }
        if (set ^ alt) /* set is always 1 or 0 */
          tswapscreen();
        if (*args != 1049)
          break;
        /* FALLTHROUGH */
      case 1048:
        tcursor((set) ? CURSOR_SAVE : CURSOR_LOAD);
        break;
      case 2004: /* 2004: bracketed paste mode */
        xsetmode(set, MODE_BRCKTPASTE);
        break;
      /* Not implemented mouse modes. See comments there. */
      case 1001: /* mouse highlight mode; can hang the
                    terminal by design when implemented. */
      case 1005: /* UTF-8 mouse mode; will confuse
                    applications not supporting UTF-8
                    and luit. */
      case 1015: /* urxvt mangled mouse mode; incompatible
                    and can be mistaken for other control
                    codes. */
      default:
        fprintf(stderr, "erresc: unknown private set/reset mode %d\n", *args);
        break;
      }
    } else {
      switch (*args) {
      case 0: /* Error (IGNORED) */
        break;
      case 2:
        xsetmode(set, MODE_KBDLOCK);
        break;
      case 4: /* IRM -- Insertion-replacement */
        MODBIT(terminal.mode, set, MODE_INSERT);
        break;
      case 12: /* SRM -- Send/Receive */
        MODBIT(terminal.mode, !set, MODE_ECHO);
        break;
      case 20: /* LNM -- Linefeed/new line */
        MODBIT(terminal.mode, set, MODE_CRLF);
        break;
      default:
        fprintf(stderr, "erresc: unknown set/reset mode %d\n", *args);
        break;
      }
    }
  }
}

void csihandle(void) {
  char buf[40];
  int len;

  switch (csiescseq.mode[0]) {
  default:
  unknown:
    fprintf(stderr, "erresc: unknown csi ");
    csidump();
    /* die(""); */
    break;
  case '@': /* ICH -- Insert <n> blank char */
    DEFAULT(csiescseq.arg[0], 1);
    tinsertblank(csiescseq.arg[0]);
    break;
  case 'A': /* CUU -- Cursor <n> Up */
    DEFAULT(csiescseq.arg[0], 1);
    tmoveto(terminal.cursor.x, terminal.cursor.y - csiescseq.arg[0]);
    break;
  case 'B': /* CUD -- Cursor <n> Down */
  case 'e': /* VPR --Cursor <n> Down */
    DEFAULT(csiescseq.arg[0], 1);
    tmoveto(terminal.cursor.x, terminal.cursor.y + csiescseq.arg[0]);
    break;
  case 'c': /* DA -- Device Attributes */
    if (csiescseq.arg[0] == 0)
      ttywrite(vtiden, strlen(vtiden), 0);
    break;
  case 'C': /* CUF -- Cursor <n> Forward */
  case 'a': /* HPR -- Cursor <n> Forward */
    DEFAULT(csiescseq.arg[0], 1);
    tmoveto(terminal.cursor.x + csiescseq.arg[0], terminal.cursor.y);
    break;
  case 'D': /* CUB -- Cursor <n> Backward */
    DEFAULT(csiescseq.arg[0], 1);
    tmoveto(terminal.cursor.x - csiescseq.arg[0], terminal.cursor.y);
    break;
  case 'E': /* CNL -- Cursor <n> Down and first col */
    DEFAULT(csiescseq.arg[0], 1);
    tmoveto(0, terminal.cursor.y + csiescseq.arg[0]);
    break;
  case 'F': /* CPL -- Cursor <n> Up and first col */
    DEFAULT(csiescseq.arg[0], 1);
    tmoveto(0, terminal.cursor.y - csiescseq.arg[0]);
    break;
  case 'g': /* TBC -- Tabulation clear */
    switch (csiescseq.arg[0]) {
    case 0: /* clear current tab stop */
      terminal.tabs[terminal.cursor.x] = 0;
      break;
    case 3: /* clear all the tabs */
      memset(terminal.tabs, 0, terminal.col * sizeof(*terminal.tabs));
      break;
    default:
      goto unknown;
    }
    break;
  case 'G': /* CHA -- Move to <col> */
  case '`': /* HPA */
    DEFAULT(csiescseq.arg[0], 1);
    tmoveto(csiescseq.arg[0] - 1, terminal.cursor.y);
    break;
  case 'H': /* CUP -- Move to <row> <col> */
  case 'f': /* HVP */
    DEFAULT(csiescseq.arg[0], 1);
    DEFAULT(csiescseq.arg[1], 1);
    tmoveato(csiescseq.arg[1] - 1, csiescseq.arg[0] - 1);
    break;
  case 'I': /* CHT -- Cursor Forward Tabulation <n> tab stops */
    DEFAULT(csiescseq.arg[0], 1);
    tputtab(csiescseq.arg[0]);
    break;
  case 'J': /* ED -- Clear screen */
    switch (csiescseq.arg[0]) {
    case 0: /* below */
      tclearregion(terminal.cursor.x, terminal.cursor.y, terminal.col - 1,
                   terminal.cursor.y);
      if (terminal.cursor.y < terminal.row - 1) {
        tclearregion(0, terminal.cursor.y + 1, terminal.col - 1,
                     terminal.row - 1);
      }
      break;
    case 1: /* above */
      if (terminal.cursor.y > 1)
        tclearregion(0, 0, terminal.col - 1, terminal.cursor.y - 1);
      tclearregion(0, terminal.cursor.y, terminal.cursor.x, terminal.cursor.y);
      break;
    case 2: /* all */
      tclearregion(0, 0, terminal.col - 1, terminal.row - 1);
      break;
    default:
      goto unknown;
    }
    break;
  case 'K': /* EL -- Clear line */
    switch (csiescseq.arg[0]) {
    case 0: /* right */
      tclearregion(terminal.cursor.x, terminal.cursor.y, terminal.col - 1,
                   terminal.cursor.y);
      break;
    case 1: /* left */
      tclearregion(0, terminal.cursor.y, terminal.cursor.x, terminal.cursor.y);
      break;
    case 2: /* all */
      tclearregion(0, terminal.cursor.y, terminal.col - 1, terminal.cursor.y);
      break;
    }
    break;
  case 'S': /* SU -- Scroll <n> line up */
    DEFAULT(csiescseq.arg[0], 1);
    tscrollup(terminal.top, csiescseq.arg[0]);
    break;
  case 'T': /* SD -- Scroll <n> line down */
    DEFAULT(csiescseq.arg[0], 1);
    tscrolldown(terminal.top, csiescseq.arg[0]);
    break;
  case 'L': /* IL -- Insert <n> blank lines */
    DEFAULT(csiescseq.arg[0], 1);
    tinsertblankline(csiescseq.arg[0]);
    break;
  case 'l': /* RM -- Reset Mode */
    tsetmode(csiescseq.priv, 0, csiescseq.arg, csiescseq.narg);
    break;
  case 'M': /* DL -- Delete <n> lines */
    DEFAULT(csiescseq.arg[0], 1);
    tdeleteline(csiescseq.arg[0]);
    break;
  case 'X': /* ECH -- Erase <n> char */
    DEFAULT(csiescseq.arg[0], 1);
    tclearregion(terminal.cursor.x, terminal.cursor.y,
                 terminal.cursor.x + csiescseq.arg[0] - 1, terminal.cursor.y);
    break;
  case 'P': /* DCH -- Delete <n> char */
    DEFAULT(csiescseq.arg[0], 1);
    tdeletechar(csiescseq.arg[0]);
    break;
  case 'Z': /* CBT -- Cursor Backward Tabulation <n> tab stops */
    DEFAULT(csiescseq.arg[0], 1);
    tputtab(-csiescseq.arg[0]);
    break;
  case 'd': /* VPA -- Move to <row> */
    DEFAULT(csiescseq.arg[0], 1);
    tmoveato(terminal.cursor.x, csiescseq.arg[0] - 1);
    break;
  case 'h': /* SM -- Set terminal mode */
    tsetmode(csiescseq.priv, 1, csiescseq.arg, csiescseq.narg);
    break;
  case 'm': /* SGR -- Terminal attribute (color) */
    tsetattr(csiescseq.arg, csiescseq.narg);
    break;
  case 'n': /* DSR – Device Status Report (cursor position) */
    if (csiescseq.arg[0] == 6) {
      len = snprintf(buf, sizeof(buf), "\033[%i;%iR", terminal.cursor.y + 1,
                     terminal.cursor.x + 1);
      ttywrite(buf, len, 0);
    }
    break;
  case 'r': /* DECSTBM -- Set Scrolling Region */
    if (csiescseq.priv) {
      goto unknown;
    } else {
      DEFAULT(csiescseq.arg[0], 1);
      DEFAULT(csiescseq.arg[1], terminal.row);
      tsetscroll(csiescseq.arg[0] - 1, csiescseq.arg[1] - 1);
      tmoveato(0, 0);
    }
    break;
  case 's': /* DECSC -- Save cursor position (ANSI.SYS) */
    tcursor(CURSOR_SAVE);
    break;
  case 'u': /* DECRC -- Restore cursor position (ANSI.SYS) */
    tcursor(CURSOR_LOAD);
    break;
  case ' ':
    switch (csiescseq.mode[1]) {
    case 'q': /* DECSCUSR -- Set Cursor Style */
      if (xsetcursor(csiescseq.arg[0]))
        goto unknown;
      break;
    default:
      goto unknown;
    }
    break;
  }
}

void csidump(void) {
  int i;
  uint c;

  fprintf(stderr, "ESC[");
  for (i = 0; i < csiescseq.len; i++) {
    c = csiescseq.buf[i] & 0xff;
    if (isprint(c)) {
      putc(c, stderr);
    } else if (c == '\n') {
      fprintf(stderr, "(\\n)");
    } else if (c == '\r') {
      fprintf(stderr, "(\\r)");
    } else if (c == 0x1b) {
      fprintf(stderr, "(\\e)");
    } else {
      fprintf(stderr, "(%02x)", c);
    }
  }
  putc('\n', stderr);
}

void csireset(void) { memset(&csiescseq, 0, sizeof(csiescseq)); }

void strhandle(void) {
  char *p = NULL;
  int j, narg, par;

  terminal.esc &= ~(ESC_STR_END | ESC_STR);
  strparse();
  par = (narg = strescseq.narg) ? atoi(strescseq.args[0]) : 0;

  switch (strescseq.type) {
  case ']': /* OSC -- Operating System Command */
    switch (par) {
    case 0:
    case 1:
    case 2:
      if (narg > 1)
        xsettitle(strescseq.args[1]);
      return;
    case 52:
      if (narg > 2) {
        char *dec;

        dec = base64dec(strescseq.args[2]);
        if (dec) {
          xsetsel(dec);
          xclipcopy();
        } else {
          fprintf(stderr, "erresc: invalid base64\n");
        }
      }
      return;
    case 4: /* color set */
      if (narg < 3)
        break;
      p = strescseq.args[2];
      /* FALLTHROUGH */
    case 104: /* color reset, here p = NULL */
      j = (narg > 1) ? atoi(strescseq.args[1]) : -1;
      if (xsetcolorname(j, p)) {
        fprintf(stderr, "erresc: invalid color %s\n", p);
      } else {
        /*
         * TODO if defaultbg color is changed, borders
         * are dirty
         */
        redraw();
      }
      return;
    }
    break;
  case 'k': /* old title set compatibility */
    xsettitle(strescseq.args[0]);
    return;
  case 'P': /* DCS -- Device Control String */
    terminal.mode |= ESC_DCS;
  case '_': /* APC -- Application Program Command */
  case '^': /* PM -- Privacy Message */
    return;
  }

  fprintf(stderr, "erresc: unknown str ");
  strdump();
}

void strparse(void) {
  int c;
  char *p = strescseq.buf;

  strescseq.narg = 0;
  strescseq.buf[strescseq.len] = '\0';

  if (*p == '\0')
    return;

  while (strescseq.narg < STR_ARG_SIZ) {
    strescseq.args[strescseq.narg++] = p;
    while ((c = *p) != ';' && c != '\0')
      ++p;
    if (c == '\0')
      return;
    *p++ = '\0';
  }
}

void strdump(void) {
  int i;
  uint c;

  fprintf(stderr, "ESC%c", strescseq.type);
  for (i = 0; i < strescseq.len; i++) {
    c = strescseq.buf[i] & 0xff;
    if (c == '\0') {
      putc('\n', stderr);
      return;
    } else if (isprint(c)) {
      putc(c, stderr);
    } else if (c == '\n') {
      fprintf(stderr, "(\\n)");
    } else if (c == '\r') {
      fprintf(stderr, "(\\r)");
    } else if (c == 0x1b) {
      fprintf(stderr, "(\\e)");
    } else {
      fprintf(stderr, "(%02x)", c);
    }
  }
  fprintf(stderr, "ESC\\\n");
}

void strreset(void) { memset(&strescseq, 0, sizeof(strescseq)); }

void sendbreak(const Arg *arg) {
  if (tcsendbreak(tty_master_fd, 0))
    perror("Error sending break");
}

void iso14755(const Arg *arg) {
  FILE *p;
  char *us, *e, codepoint[9], uc[UTF8_MAX_CHAR_SIZE];
  unsigned long utf32;

  if (!(p = popen(ISO14755CMD, "r")))
    return;

  us = fgets(codepoint, sizeof(codepoint), p);
  pclose(p);

  if (!us || *us == '\0' || *us == '-' || strlen(us) > 7)
    return;
  if ((utf32 = strtoul(us, &e, 16)) == ULONG_MAX || (*e != '\n' && *e != '\0'))
    return;

  ttywrite(uc, utf8_encode(utf32, uc), 1);
}

void tputtab(int n) {
  uint x = terminal.cursor.x;

  if (n > 0) {
    while (x < terminal.col && n--)
      for (++x; x < terminal.col && !terminal.tabs[x]; ++x)
        /* nothing */;
  } else if (n < 0) {
    while (x > 0 && n++)
      for (--x; x > 0 && !terminal.tabs[x]; --x)
        /* nothing */;
  }
  terminal.cursor.x = LIMIT(x, 0, terminal.col - 1);
}

void tdefutf8(char ascii) {
  if (ascii == 'G')
    terminal.mode |= MODE_UTF8;
  else if (ascii == '@')
    terminal.mode &= ~MODE_UTF8;
}

void tdeftran(char ascii) {
  static char cs[] = "0B";
  static int vcs[] = {CS_GRAPHIC0, CS_USA};
  char *p;

  if ((p = strchr(cs, ascii)) == NULL) {
    fprintf(stderr, "esc unhandled charset: ESC ( %c\n", ascii);
  } else {
    terminal.trantbl[terminal.icharset] = vcs[p - cs];
  }
}

void tdectest(char c) {
  int x, y;

  if (c == '8') { /* DEC screen alignment test. */
    for (x = 0; x < terminal.col; ++x) {
      for (y = 0; y < terminal.row; ++y)
        tsetchar('E', &terminal.cursor.attr, x, y);
    }
  }
}

void tstrsequence(uchar c) {
  strreset();

  switch (c) {
  case 0x90: /* DCS -- Device Control String */
    c = 'P';
    terminal.esc |= ESC_DCS;
    break;
  case 0x9f: /* APC -- Application Program Command */
    c = '_';
    break;
  case 0x9e: /* PM -- Privacy Message */
    c = '^';
    break;
  case 0x9d: /* OSC -- Operating System Command */
    c = ']';
    break;
  }
  strescseq.type = c;
  terminal.esc |= ESC_STR;
}

void tcontrolcode(uchar ascii) {
  switch (ascii) {
  case '\t': /* HT */
    tputtab(1);
    return;
  case '\b': /* BS */
    tmoveto(terminal.cursor.x - 1, terminal.cursor.y);
    return;
  case '\r': /* CR */
    tmoveto(0, terminal.cursor.y);
    return;
  case '\f': /* LF */
  case '\v': /* VT */
  case '\n': /* LF */
    /* go to first col if the mode is set */
    tnewline(IS_SET(MODE_CRLF));
    return;
  case '\a': /* BEL */
    if (terminal.esc & ESC_STR_END) {
      /* backwards compatibility to xterm */
      strhandle();
    } else {
      xbell();
    }
    break;
  case '\033': /* ESC */
    csireset();
    terminal.esc &= ~(ESC_CSI | ESC_ALTCHARSET | ESC_TEST);
    terminal.esc |= ESC_START;
    return;
  case '\016': /* SO (LS1 -- Locking shift 1) */
  case '\017': /* SI (LS0 -- Locking shift 0) */
    terminal.charset = 1 - (ascii - '\016');
    return;
  case '\032': /* SUB */
    tsetchar('?', &terminal.cursor.attr, terminal.cursor.x, terminal.cursor.y);
  case '\030': /* CAN */
    csireset();
    break;
  case '\005': /* ENQ (IGNORED) */
  case '\000': /* NUL (IGNORED) */
  case '\021': /* XON (IGNORED) */
  case '\023': /* XOFF (IGNORED) */
  case 0177:   /* DEL (IGNORED) */
    return;
  case 0x80: /* TODO: PAD */
  case 0x81: /* TODO: HOP */
  case 0x82: /* TODO: BPH */
  case 0x83: /* TODO: NBH */
  case 0x84: /* TODO: IND */
    break;
  case 0x85:     /* NEL -- Next line */
    tnewline(1); /* always go to first col */
    break;
  case 0x86: /* TODO: SSA */
  case 0x87: /* TODO: ESA */
    break;
  case 0x88: /* HTS -- Horizontal tab stop */
    terminal.tabs[terminal.cursor.x] = 1;
    break;
  case 0x89: /* TODO: HTJ */
  case 0x8a: /* TODO: VTS */
  case 0x8b: /* TODO: PLD */
  case 0x8c: /* TODO: PLU */
  case 0x8d: /* TODO: RI */
  case 0x8e: /* TODO: SS2 */
  case 0x8f: /* TODO: SS3 */
  case 0x91: /* TODO: PU1 */
  case 0x92: /* TODO: PU2 */
  case 0x93: /* TODO: STS */
  case 0x94: /* TODO: CCH */
  case 0x95: /* TODO: MW */
  case 0x96: /* TODO: SPA */
  case 0x97: /* TODO: EPA */
  case 0x98: /* TODO: SOS */
  case 0x99: /* TODO: SGCI */
    break;
  case 0x9a: /* DECID -- Identify Terminal */
    ttywrite(vtiden, strlen(vtiden), 0);
    break;
  case 0x9b: /* TODO: CSI */
  case 0x9c: /* TODO: ST */
    break;
  case 0x90: /* DCS -- Device Control String */
  case 0x9d: /* OSC -- Operating System Command */
  case 0x9e: /* PM -- Privacy Message */
  case 0x9f: /* APC -- Application Program Command */
    tstrsequence(ascii);
    return;
  }
  /* only CAN, SUB, \a and C1 chars interrupt a sequence */
  terminal.esc &= ~(ESC_STR_END | ESC_STR);
}

/*
 * returns 1 when the sequence is finished and it hasn't to read
 * more characters for this sequence, otherwise 0
 */
int eschandle(uchar ascii) {
  switch (ascii) {
  case '[':
    terminal.esc |= ESC_CSI;
    return 0;
  case '#':
    terminal.esc |= ESC_TEST;
    return 0;
  case '%':
    terminal.esc |= ESC_UTF8;
    return 0;
  case 'P': /* DCS -- Device Control String */
  case '_': /* APC -- Application Program Command */
  case '^': /* PM -- Privacy Message */
  case ']': /* OSC -- Operating System Command */
  case 'k': /* old title set compatibility */
    tstrsequence(ascii);
    return 0;
  case 'n': /* LS2 -- Locking shift 2 */
  case 'o': /* LS3 -- Locking shift 3 */
    terminal.charset = 2 + (ascii - 'n');
    break;
  case '(': /* GZD4 -- set primary charset G0 */
  case ')': /* G1D4 -- set secondary charset G1 */
  case '*': /* G2D4 -- set tertiary charset G2 */
  case '+': /* G3D4 -- set quaternary charset G3 */
    terminal.icharset = ascii - '(';
    terminal.esc |= ESC_ALTCHARSET;
    return 0;
  case 'D': /* IND -- Linefeed */
    if (terminal.cursor.y == terminal.bot) {
      tscrollup(terminal.top, 1);
    } else {
      tmoveto(terminal.cursor.x, terminal.cursor.y + 1);
    }
    break;
  case 'E':      /* NEL -- Next line */
    tnewline(1); /* always go to first col */
    break;
  case 'H': /* HTS -- Horizontal tab stop */
    terminal.tabs[terminal.cursor.x] = 1;
    break;
  case 'M': /* RI -- Reverse index */
    if (terminal.cursor.y == terminal.top) {
      tscrolldown(terminal.top, 1);
    } else {
      tmoveto(terminal.cursor.x, terminal.cursor.y - 1);
    }
    break;
  case 'Z': /* DECID -- Identify Terminal */
    ttywrite(vtiden, strlen(vtiden), 0);
    break;
  case 'c': /* RIS -- Reset to inital state */
    treset();
    resettitle();
    xloadcols();
    break;
  case '=': /* DECPAM -- Application keypad */
    xsetmode(1, MODE_APPKEYPAD);
    break;
  case '>': /* DECPNM -- Normal keypad */
    xsetmode(0, MODE_APPKEYPAD);
    break;
  case '7': /* DECSC -- Save Cursor */
    tcursor(CURSOR_SAVE);
    break;
  case '8': /* DECRC -- Restore Cursor */
    tcursor(CURSOR_LOAD);
    break;
  case '\\': /* ST -- String Terminator */
    if (terminal.esc & ESC_STR_END)
      strhandle();
    break;
  default:
    fprintf(stderr, "erresc: unknown sequence ESC 0x%02X '%c'\n", (uchar)ascii,
            isprint(ascii) ? ascii : '.');
    break;
  }
  return 1;
}

void tputc(uint32_t utf32_code_point) {
  char utf8_char[UTF8_MAX_CHAR_SIZE];
  int width;
  int utf8_char_size;
  Character *gp;

  int is_control_character = ISCONTROL(utf32_code_point);
  if (terminal.mode & MODE_UTF8) {
    utf8_char_size = utf8_encode(utf32_code_point, utf8_char);
    if (!is_control_character && (width = wcwidth(utf32_code_point)) == -1) {
      memcpy(utf8_char, "\357\277\275", 4); /* UTF_INVALID */
      width = 1;
    }
  } else {
    utf8_char[0] = utf32_code_point;
    width = utf8_char_size = 1;
  }

  /*
   * STR sequence must be checked before anything else
   * because it uses all following characters until it
   * receives a ESC, a SUB, a ST or any other C1 control
   * character.
   */
  if (terminal.esc & ESC_STR) {
    if (utf32_code_point == '\a' || utf32_code_point == 030 ||
        utf32_code_point == 032 || utf32_code_point == 033 ||
        ISCONTROLC1(utf32_code_point)) {
      terminal.esc &= ~(ESC_START | ESC_STR | ESC_DCS);
      terminal.esc |= ESC_STR_END;
      goto check_control_code;
    }

    if (strescseq.len + utf8_char_size >= sizeof(strescseq.buf) - 1) {
      /*
       * Here is a bug in terminals. If the user never sends
       * some code to stop the str or esc command, then st
       * will stop responding. But this is better than
       * silently failing with unknown characters. At least
       * then users will report back.
       *
       * In the case users ever get fixed, here is the code:
       */
      /*
       * terminal.esc = 0;
       * strhandle();
       */
      return;
    }

    memmove(&strescseq.buf[strescseq.len], utf8_char, utf8_char_size);
    strescseq.len += utf8_char_size;
    return;
  }

check_control_code:
  /*
   * Actions of control codes must be performed as soon they arrive
   * because they can be embedded inside a control sequence, and
   * they must not cause conflicts with sequences.
   */
  if (is_control_character) {
    tcontrolcode(utf32_code_point);
    /*
     * control codes are not shown ever
     */
    return;
  } else if (terminal.esc & ESC_START) {
    if (terminal.esc & ESC_CSI) {
      csiescseq.buf[csiescseq.len++] = utf32_code_point;
      if (BETWEEN(utf32_code_point, 0x40, 0x7E) ||
          csiescseq.len >= sizeof(csiescseq.buf) - 1) {
        terminal.esc = 0;
        csiparse();
        csihandle();
      }
      return;
    } else if (terminal.esc & ESC_UTF8) {
      tdefutf8(utf32_code_point);
    } else if (terminal.esc & ESC_ALTCHARSET) {
      tdeftran(utf32_code_point);
    } else if (terminal.esc & ESC_TEST) {
      tdectest(utf32_code_point);
    } else {
      if (!eschandle(utf32_code_point))
        return;
      /* sequence already finished */
    }
    terminal.esc = 0;
    /*
     * All characters which form part of a sequence are not
     * printed
     */
    return;
  }
  if (sel.ob.x != -1 && BETWEEN(terminal.cursor.y, sel.ob.y, sel.oe.y))
    selclear();

  gp = &terminal.line[terminal.cursor.y][terminal.cursor.x];
  if (IS_SET(MODE_WRAP) && (terminal.cursor.state & CURSOR_WRAPNEXT)) {
    gp->mode |= ATTR_WRAP;
    tnewline(1);
    gp = &terminal.line[terminal.cursor.y][terminal.cursor.x];
  }

  if (IS_SET(MODE_INSERT) && terminal.cursor.x + width < terminal.col)
    memmove(gp + width, gp,
            (terminal.col - terminal.cursor.x - width) * sizeof(Character));

  if (terminal.cursor.x + width > terminal.col) {
    tnewline(1);
    gp = &terminal.line[terminal.cursor.y][terminal.cursor.x];
  }

  tsetchar(utf32_code_point, &terminal.cursor.attr, terminal.cursor.x,
           terminal.cursor.y);

  if (width == 2) {
    gp->mode |= ATTR_WIDE;
    if (terminal.cursor.x + 1 < terminal.col) {
      gp[1].utf32_code_point = '\0';
      gp[1].mode = ATTR_WDUMMY;
    }
  }
  if (terminal.cursor.x + width < terminal.col) {
    tmoveto(terminal.cursor.x + width, terminal.cursor.y);
  } else {
    terminal.cursor.state |= CURSOR_WRAPNEXT;
  }
}

int twrite(const char *buffer, int length, int show_ctrl) {
  int bytes_written = 0;
  int charsize;
  for (; bytes_written < length; bytes_written += charsize) {
    uint32_t utf32_code_point;
    if (IS_SET(MODE_UTF8)) {
      /* process a complete utf8 char */
      charsize = utf8_decode(buffer + bytes_written, &utf32_code_point,
                             length - bytes_written);
      if (charsize == 0)
        break;
    } else {
      utf32_code_point = buffer[bytes_written] & 0xFF;
      charsize = 1;
    }
    if (show_ctrl && ISCONTROL(utf32_code_point)) {
      if (utf32_code_point & 0x80) {
        utf32_code_point &= 0x7f;
        tputc('^');
        tputc('[');
      } else if (utf32_code_point != '\n' && utf32_code_point != '\r' &&
                 utf32_code_point != '\t') {
        utf32_code_point ^= 0x40;
        tputc('^');
      }
    }
    tputc(utf32_code_point);
  }
  return bytes_written;
}

void terminal_init(int cols, int rows) {
  terminal = (Terminal){.cursor = {.attr = {.fg = defaultfg, .bg = defaultbg}}};
  tresize(cols, rows);
  treset();
}

void tresize(int col, int row) {
  int i;
  int minrow = MIN(row, terminal.row);
  int mincol = MIN(col, terminal.col);
  int *bp;
  TCursor cursor;

  if (col < 1 || row < 1) {
    fprintf(stderr, "tresize: error resizing to %dx%d\n", col, row);
    return;
  }

  /*
   * slide screen to keep cursor where we expect it -
   * tscrollup would work here, but we can optimize to
   * memmove because we're freeing the earlier lines
   */
  for (i = 0; i <= terminal.cursor.y - row; i++) {
    free(terminal.line[i]);
    free(terminal.alt[i]);
  }
  /* ensure that both src and dst are not NULL */
  if (i > 0) {
    memmove(terminal.line, terminal.line + i, row * sizeof(Line));
    memmove(terminal.alt, terminal.alt + i, row * sizeof(Line));
  }
  for (i += row; i < terminal.row; i++) {
    free(terminal.line[i]);
    free(terminal.alt[i]);
  }

  /* resize to new height */
  terminal.line = xrealloc(terminal.line, row * sizeof(Line));
  terminal.alt = xrealloc(terminal.alt, row * sizeof(Line));
  terminal.dirty = xrealloc(terminal.dirty, row * sizeof(*terminal.dirty));
  terminal.tabs = xrealloc(terminal.tabs, col * sizeof(*terminal.tabs));

  /* resize each row to new width, zero-pad if needed */
  for (i = 0; i < minrow; i++) {
    terminal.line[i] = xrealloc(terminal.line[i], col * sizeof(Character));
    terminal.alt[i] = xrealloc(terminal.alt[i], col * sizeof(Character));
  }

  /* allocate any new rows */
  for (/* i = minrow */; i < row; i++) {
    terminal.line[i] = xmalloc(col * sizeof(Character));
    terminal.alt[i] = xmalloc(col * sizeof(Character));
  }
  if (col > terminal.col) {
    bp = terminal.tabs + terminal.col;

    memset(bp, 0, sizeof(*terminal.tabs) * (col - terminal.col));
    while (--bp > terminal.tabs && !*bp)
      /* nothing */;
    for (bp += tabspaces; bp < terminal.tabs + col; bp += tabspaces)
      *bp = 1;
  }
  /* update terminal size */
  terminal.col = col;
  terminal.row = row;
  /* reset scrolling region */
  tsetscroll(0, row - 1);
  /* make use of the LIMIT in tmoveto */
  tmoveto(terminal.cursor.x, terminal.cursor.y);
  /* Clearing both screens (it makes dirty all lines) */
  cursor = terminal.cursor;
  for (i = 0; i < 2; i++) {
    if (mincol < col && 0 < minrow) {
      tclearregion(mincol, 0, col - 1, minrow - 1);
    }
    if (0 < col && minrow < row) {
      tclearregion(0, minrow, col - 1, row - 1);
    }
    tswapscreen();
    tcursor(CURSOR_LOAD);
  }
  terminal.cursor = cursor;
}

void resettitle(void) { xsettitle(NULL); }

void drawregion(int x1, int y1, int x2, int y2) {
  int y;
  for (y = y1; y < y2; y++) {
    if (!terminal.dirty[y])
      continue;

    terminal.dirty[y] = 0;
    xdrawline(terminal.line[y], x1, y, x2);
  }
}

void draw(void) {
  int cx = terminal.cursor.x;

  if (!xstartdraw())
    return;

  /* adjust cursor position */
  LIMIT(terminal.old_cursor_x, 0, terminal.col - 1);
  LIMIT(terminal.old_cursor_y, 0, terminal.row - 1);
  if (terminal.line[terminal.old_cursor_y][terminal.old_cursor_x].mode &
      ATTR_WDUMMY)
    terminal.old_cursor_x--;
  if (terminal.line[terminal.cursor.y][cx].mode & ATTR_WDUMMY)
    cx--;

  drawregion(0, 0, terminal.col, terminal.row);
  x_draw_cursor(cx, terminal.cursor.y, terminal.line[terminal.cursor.y][cx],
                terminal.old_cursor_x, terminal.old_cursor_y,
                terminal.line[terminal.old_cursor_y][terminal.old_cursor_x]);
  terminal.old_cursor_x = cx, terminal.old_cursor_y = terminal.cursor.y;
  xfinishdraw();
}

void redraw(void) {
  tfulldirt();
  draw();
}
