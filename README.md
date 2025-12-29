
# cl-notcurses

A first attempt at a Common Lisp wrapper library for the [notcurses](https://github.com/dankamongmen/notcurses) terminal programming C library by Nick Black.

# Files

- **cffi-notcurses.lisp** - the package with Lisp wrapper functions
- **examples/** - A directory of example lisp programs using the wrappers

# Running the Lisp demos

## Running from the command line
- cd to examples/
- Make sure you have the notcurses shared libraries (.so or .dylib or .dll) on your system, as well as the development files (e.g., /usr/include/notcurses/notcurses.h).
- Edit the run.sh script to point CLEXE to your preferred Common Lisp compiler
- Run the run.sh command with any .lisp file in the directory
  ```
  $ ./run.sh 03-asterisks.lisp
  ```
## Running from inside emacs
- The following directions will be familiar already to long-time emacs users who program in CL, but for the sake of those (like myself) starting out, I have found [a paragraph from plisp](https://github.com/Plisp/uncursed/blob/master/README.md) to be highly useful, and have reproduced them here in slightly extended form. To wit: 
- If you want to tinker with the Lisp code, you can run the demos interactively from inside emacs but output to another terminal window (such as an xterm or kitty or wezterm etc.).
- This way, you can use your separate terminal window for output. This is especially helpful for working with notcurses (and ncurses) and any other library which changes the modes the terminal is in.

1. Open up another terminal window (xterm, kitty, wezterm, rxvt, konsole, etc.) and start up your Lisp processor just at the terminal
  ```bash
  $ sbcl
  * _
```
2. Then run these commands from your sbcl or other CL prompt:
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
3. Now return to emacs and
  - Make a connection to that external slime server
    ```lisp
    M-x slime-connect
    ```
  - or slynk server
    ```lisp
    M-x sly-connect
    ```
4. Then either 
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
