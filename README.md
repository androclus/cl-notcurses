
# cl-notcurses

A first attempt at a Common Lisp wrapper library for the [notcurses](https://github.com/dankamongmen/notcurses) terminal programming C library by Nick Black.

# Files

- **cffi-notcurses.lisp** - the package with Lisp wrapper functions
- **examples/** - A directory of example lisp programs using the wrappers

# Running the Lisp demos

## Running from the command line
- Make sure you have the notcurses shared libraries (.so or .dylib or .dll) on your system.  For instance, on my Linux OpenSUSE Tumbleweed system, these are:
  ```
  /usr/lib64/libnotcurses-core.so =>
    /usr/lib64/libnotcurses-core.so.3 =>
      /usr/lib64/libnotcurses-core.so.3.0.16
  /usr/lib64/libnotcurses-ffi.so =>
    /usr/lib64/libnotcurses-ffi.so.3 =>
      /usr/lib64/libnotcurses-ffi.so.3.0.16
  /usr/lib64/libnotcurses.so =>
    /usr/lib64/libnotcurses.so.3 =>
      /usr/lib64/libnotcurses.so.3.0.16
  ```
  and came in the rpm packages
  ```
  libnotcurses3
  libnotcurses-core3
  libnotcurses-ffi3
  ```
  respectively.
- *(Note that if you want to add to the wrappers, you'll need access to the .h files as a reference, which would require the additional -devel versions of these packages.)*
- cd to the examples/ directory at the command line.
- Edit the run.sh script to point CLEXE to your preferred Common Lisp compiler (default=sbcl).
- Run the run.sh command with any .lisp file in the directory. E.g.,
  ```
  $ ./run.sh 03-asterisks.lisp
  ```
## Running from inside emacs
- If you want to tinker with the Lisp code and see what it does to an I/O terminal in real time, you'll need to have a different terminal window for live output. (You wouldn't want to be attempting to send notcurses output to your emacs window). To do so, what most emacs CL developers apparently do is set up a client-server relationship like the following:

1. Open up another terminal window (xterm, kitty, wezterm, rxvt, konsole, etc.)
2. Start up your Lisp processor inside that terminal window:
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
3. Then run these commands from that prompt:
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
4. Now return to emacs and
  - Make a connection to that external slime server
    ```lisp
    M-x slime-connect
    ```
  - or slynk server
    ```lisp
    M-x sly-connect
    ```
5. Then either 
  - load the file at the REPL:
    ```lisp
    CL-USER> (load "<path-to>/03-asterisks.lisp")
    ```
    or
  - bring the file into the editor with C-x C-f and then evaluate it, sending the output to the external swank/slynk server window via
    ```emacs
    M-x slime-eval-buffer
    ```
    or
    ```emacs
    M-x sly-eval-buffer
    ```
6. When you are done, you can close down the slime/slynk server from within emacs by
  - for slime:
    ```
    M-x slime-disconnect
    ```
  - for sly:
    ```
    M-x sly-disconnect
    ```
  - and then quit inside emacs at the prompt:
    ```
    CL-USER> (quit)
    ```
