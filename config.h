/*
 * appearance
 *
 * font: see http://freedesktop.org/software/fontconfig/fontconfig-user.html
 */
static char *font = "inconsolata:pixelsize=20:antialias=true:autohint=true";

/* identification sequence returned in DA and DECID */
char *vtiden = "\033[?6c";

/* Kerning / character bounding-box multipliers */
static float cwscale = 1.0;
static float chscale = 1.0;

/*
 * word delimiter string
 *
 * More advanced example: " `'\"()[]{}"
 */
char *worddelimiters = " ";

/* selection timeouts (in milliseconds) */
static unsigned int doubleclicktimeout = 300;
static unsigned int tripleclicktimeout = 600;

/*
 * alt screens
 * https://stackoverflow.com/questions/11023929/using-the-alternate-screen-in-a-bash-script
 */
int allowaltscreen = 0;

/* frames per second bw should at maximum draw to the screen */
static unsigned int x_fps = 120;
static unsigned int action_fps = 30;

/*
 * blinking timeout (set to 0 to disable blinking) for the terminal blinking
 * attribute.
 */
static unsigned int blinktimeout = 800;

/*
 * bell volume. It must be a value between -100 and 100. Use 0 for disabling
 * it
 */
static int bellvolume = 0;

/* default TERM value */
char *termname = "bw-256color";

/*
 * spaces per tab
 *
 * When you are changing this value, don't forget to adapt the »it« value in
 * the bw.info and appropriately install the bw.info in the environment where
 * you use this bw version.
 *
 *	it#$tabspaces,
 *
 * Secondly make sure your kernel is not expanding tabs. When running `stty
 * -a` »tab0« should appear. You can tell the terminal to not expand tabs by
 *  running following command:
 *
 *	stty tabs
 */
unsigned int tabspaces = 8;

/* Terminal colors (16 first used in escape sequence) */
static const char *colorname[] = {
    /* 8 normal colors */
    "black", "red3", "green3", "yellow3", "blue2", "magenta3", "cyan3",
    "gray90",

    /* 8 bright colors */
    "gray50", "red", "green", "yellow", "#5c5cff", "magenta", "cyan", "white",

    [255] = 0,

    /* more colors can be added after 255 to use with DefaultXX */
    "#cccccc", "#555555",
    "#fffacd", // Lemon Chiffon
};

/*
 * Default colors (colorname index)
 * foreground, background, cursor, reverse cursor
 */
unsigned int defaultfg = 0;
unsigned int defaultbg = 258;
static unsigned int defaultcs = 256;
static unsigned int defaultrcs = 257;

/*
 * Default columns and rows numbers
 * Must be positive.
 */
static unsigned int cols = 80;
static unsigned int rows = 24;

/*
 * Color used to display font attributes when fontconfig selected a font which
 * doesn't match the ones requested.
 */
static unsigned int defaultattr = 11;

/* Internal keyboard shortcuts. */
#define MODKEY Mod1Mask
#define TERMMOD (ControlMask | ShiftMask)

static Shortcut shortcuts[] = {
    /* mask                 keysym          function        argument */
    {XK_ANY_MOD, XK_Break, sendbreak, {.i = 0}},
    {TERMMOD, XK_C, clipcopy, {.i = 0}},
    {TERMMOD, XK_V, clippaste, {.i = 0}},
    {TERMMOD, XK_Y, selpaste, {.i = 0}},
    {TERMMOD, XK_I, iso14755, {.i = 0}},
};

/*
 * State bits to ignore when matching key or button events.  By default,
 * numlock (Mod2Mask) and keyboard layout (XK_SWITCH_MOD) are ignored.
 */
static uint ignoremod = Mod2Mask | XK_SWITCH_MOD;

/*
 * Override mouse-select while mask is active (when MODE_MOUSE is set).
 * Note that if you want to use ShiftMask with selmasks, set this to an other
 * modifier, set to 0 to not use it.
 */
static uint forceselmod = ShiftMask;

static Key key[] = {
    // keysym, string
    {XK_Up, "\033[A"},
    {XK_Down, "\033[B"},
    {XK_Left, "\033[D"},
    {XK_Right, "\033[C"},
    {XK_Return, "\r"},
    {XK_BackSpace, "\177"},
    {XK_F1, "\033OP"},
    {XK_F2, "\033OQ"},
    {XK_F3, "\033OR"},
    {XK_F4, "\033OS"},
    {XK_F5, "\033[15~"},
    {XK_F6, "\033[17~"},
    {XK_F7, "\033[18~"},
    {XK_F8, "\033[19~"},
    {XK_F9, "\033[20~"},
    {XK_F10, "\033[21~"},
};

/*
 * Selection types' masks.
 * Use the same masks as usual.
 * Button1Mask is always unset, to make masks match between ButtonPress.
 * ButtonRelease and MotionNotify.
 * If no match is found, regular selection is used.
 */
static uint selmasks[] = {
    [SEL_RECTANGULAR] = Mod1Mask,
};

/*
 * Printable characters in ASCII, used to estimate the advance width
 * of single wide characters.
 */
static char ascii_printable[] = " !\"#$%&'()*+,-./0123456789:;<=>?"
                                "@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_"
                                "`abcdefghijklmnopqrstuvwxyz{|}~";
