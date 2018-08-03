// http://math.msu.su/~vvb/2course/Borisenko/CppProjects/GWindow/xintro.html
// http://math.msu.su/~vvb/2course/Borisenko/CppProjects/GWindow/hi.c

#include <X11/Xlib.h>
#include <X11/Xos.h>
#include <X11/Xutil.h>
#include <stdio.h>
#include <stdlib.h> /* exit */

Display *display;
int screen;
Window window;
GC graphics_context;

void init_x();
void close_x();

int main() {
  XEvent event;
  KeySym key;     // a dealie-bob to handle KeyPress Events
  char text[255]; // a char buffer for KeyPress Events

  init_x();

  int enable_drawing_pointer_motion = 0;
  while (1) {
    // Get the next event and stuff it into our event variable.
    // Note: only events we set the mask for are detected!
    XNextEvent(display, &event);

    if (event.type == Expose && event.xexpose.count == 0) {
      // The window was exposed redraw it!
      XClearWindow(display, window);
    }
    if (event.type == KeyPress &&
        XLookupString(&event.xkey, text, 255, &key, 0) == 1) {
      // Use the XLookupString routine to convert the invent
      // KeyPress data into regular text.  Weird but necessary...
      if (text[0] == 'q') {
        close_x();
      }
      if (text[0] == 'e') {
        enable_drawing_pointer_motion = enable_drawing_pointer_motion ? 0 : 1;
      }
      printf("You pressed the %c key!\n", text[0]);
    }
    if (event.type == ButtonPress) {
      // Where the mouse clicked.
      int x = event.xbutton.x;
      int y = event.xbutton.y;

      strcpy(text, "X is FUN!");
      XSetForeground(display, graphics_context, rand() % event.xbutton.x % 255);
      XDrawString(display, window, graphics_context, x, y, text, strlen(text));
    }
    if (event.type == MotionNotify && enable_drawing_pointer_motion) {
      int x = event.xmotion.x;
      int y = event.xmotion.y;
      // Avoid division by 0 if `event.xbutton.x == 0`.
      int seed = event.xbutton.x == 0 ? 1 : event.xbutton.x;
      XSetForeground(display, graphics_context, rand() % seed % 255);
      XDrawPoint(display, window, graphics_context, x, y);
    }
  }
}

void init_x() {
  // Connect to X server. Use environment variable DISPLAY for address.
  display = XOpenDisplay(NULL);
  if (!display)
    exit(1);

  screen = DefaultScreen(display);
  unsigned long black = BlackPixel(display, screen);
  unsigned long white = WhitePixel(display, screen);

  // This window will be have be 200 pixels across and 300 down.
  // It will have the foreground white and background black
  window = XCreateSimpleWindow(display, DefaultRootWindow(display),
                               /*x-origin=*/0, /*y-origin=*/0, /*width=*/20,
                               /*height=*/30, /*border-width=*/0,
                               /*border-color=*/white, /*background=*/black);

  // Here is where some properties of the window can be set.
  // The third and fourth items indicate the name which appears
  // at the top of the window and the name of the minimized window
  // respectively.
  XSetStandardProperties(display, window, "My Window", "HI!", None, NULL, 0,
                         NULL);

  // Determines which types of input are allowed in the input.
  XSelectInput(display, window,
               ExposureMask | ButtonPressMask | KeyPressMask |
                   PointerMotionMask);
  graphics_context = XCreateGC(display, window, 0, 0);
  XSetBackground(display, graphics_context, white);
  XSetForeground(display, graphics_context, black);

  // Redraw the window.
  XClearWindow(display, window);

  // Bring window on top of other windows.
  XMapRaised(display, window);
};

void close_x() {
  XFreeGC(display, graphics_context);
  XDestroyWindow(display, window);
  XCloseDisplay(display);
  exit(1);
}
