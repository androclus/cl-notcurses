
# cl-notcurses

A first attempt at a Common Lisp wrapper library for the [notcurses](https://github.com/dankamongmen/notcurses) terminal programming C library by Nick Black.

# Files

- **src/** - cl-notcurses package definition and wrappers
- **examples/** - Example CL programs using the wrappers

# Requirements

- **notcurses shared C libraries** (.so or .dylib or .dll) on your system.  For instance, on my Linux OpenSUSE Tumbleweed system, these are:
  ```
  /usr/lib64/libnotcurses.so =>
    /usr/lib64/libnotcurses.so.3 =>
      /usr/lib64/libnotcurses.so.3.0.16

  /usr/lib64/libnotcurses-core.so =>
    /usr/lib64/libnotcurses-core.so.3 =>
      /usr/lib64/libnotcurses-core.so.3.0.16

  /usr/lib64/libnotcurses-ffi.so =>
    /usr/lib64/libnotcurses-ffi.so.3 =>
      /usr/lib64/libnotcurses-ffi.so.3.0.16

  ```
  and came in the rpm packages
  ```
  libnotcurses3
  libnotcurses-core3
  libnotcurses-ffi3
  ```
  respectively.

## Running from inside emacs
1. **git clone** this project into your `~/common-lisp` or `~/quicklisp/local-projects` dir
2. Open up another terminal window (xterm, kitty, wezterm, rxvt, konsole, etc.)
3. In that terminal window, start up your Lisp processor (sbcl, clisp, ccl, or ecl):
   ```bash
   $ sbcl
   This is SBCL 2.5.11-1.1-suse, an implementation of ANSI Common Lisp.
   More information about SBCL is available at <http://www.sbcl.org/>.

   SBCL is free software, provided as is, with absolutely no warranty.
   It is mostly in the public domain; some portions are provided under
   BSD-style licenses.  See the CREDITS and COPYING files in the
   distribution for more information.
   * _
   ```
4. At that prompt, start up an external swank or slynk server:
   - If you use slime:
      ```lisp
      * (ql:quickload :swank)
      * (swank:create-server :dont-close t)
      * (loop (sleep 1))
      ```
   - If you use sly:
      ```lisp
      * (ql:quickload :slynk)
      * (slynk:create-server :dont-close t)
      * (loop (sleep 1))
      ```
5. Now return to emacs and
   - Make a connection to that external swank server (if you use slime as your REPL):
      ```lisp
      M-x slime-connect
      ```
    - or to that external slynk server (if you use sly as your REPL):
       ```lisp
       M-x sly-connect
       ```
6. Load the cl-ncurses project/library:
   ```lisp
   CL-USER> (asdf:load-system "cl-ncurses")
   T
   CL-USER> _
   ```
7. Load an example:
   ```lisp
   CL-USER> (load "~/common-lisp/cl-notcurses/examples/00-hello-world.lisp")
   T
   CL-USER> _
   ```
8. Run the startxx function from that example, being careful to run it from within the cl-ncurses package by either staying within the CL-USER package but referencing the cl-notcurses package (within which the example lives) explicitly:
   ```lisp
   CL-USER> (cl-notcurses::start00)
   NIL
   CL-USER> _
   ```
   or by first switching into the package from within your REPL:
   ```lisp
   CL-USER> (in-package :cl-notcurses)
   #<PACKAGE "CL-NOTCURSES">
   CL-NOTCURSES> _
   ```
   and now running the function
   ```lisp
   CL-NOTCURSES> (start00)
   NIL
   CL-NOTCURSES> _
   ```
9. Either way, when you call that startxx function, you should be able to see the results in the external terminal window with the swank/slynk server you set up.

10. When you are done, you can close down the terminal-window's slime/slynk server from within emacs:
      ```
      CL-USER> (quit)
      ```
