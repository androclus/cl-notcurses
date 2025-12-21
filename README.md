
# cl-notcurses

A first attempt at a Common Lisp wrapper library for the [notcurses](https://github.com/dankamongmen/notcurses) terminal programming C library by Nick Black.

# Files

- **cffi-notcurses.lisp** - the package with Lisp wrapper functions
- **example-wyatts.c** - This is an updated (year 2025) version of Wyatt Sheffield's simple notcurses demo program in C from his [2020 Blog post](https://wyatts.xyz/blog/writingasimpleno_2020-02-09)
- **example-wyatts.lisp** - The same demo program rewritten in Common Lisp and using wrapper functions in **cffi-notcurses.lisp**.

# Prerequisites for running the demos
- If you want to run the Lisp program, you'll need to have libncurses and libncurses-ffi shared libraries on your system.

# Compiling and running the C demo
- Make sure you have the notcurses shared libraries (.so or .dylib or .dll) on your system, as well as the development files (e.g., /usr/include/notcurses/notcurses.h).
- Download from your Linux repo or download the latest source from Nick's [notcurses](https://github.com/dankamongmen/notcurses) project on GitHub.
- To compile the C program:

```bash
$ gcc -o example-wyatts example-wyatts.c $(pkg-config --static --libs notcurses) -D_XOPEN_SOURCE=800
```
- Then to run:
```bash
$ ./example-wyatts
```

# Running the Lisp demo
## From the command line
- Edit example-wyatts.lisp first line so that it is pointing to your current directory where you downloaded this .git project, so it can find the cffi-notcurses.lisp file.
- From command line:
```bash
$ sblc --script example-wyatts.lisp
```
## From inside emacs
- If you want to mess around with the Lisp code, you can run the demo interactively from inside emacs but output to another terminal window (such as an xterm or kitty or wezterm etc.).
- Stealing a paragraph from [plisp's excellent instructions for his project](https://github.com/Plisp/uncursed/blob/master/README.md), here are the steps:
- For interactive development, an output terminal device is necessary but the SLIME repl within emacs does not emulate a terminal. To work around this, start a swank server in a terminal session and connect using M-x slime-connect or sly-connect in emacs.
- Start up sblc or other CL from the shell:
```bash
$ sblc
```
- Then run these commands from the prompt:
```lisp
* (ql:quickload :swank) ; for sly users, this is slynk
* (swank:create-server :dont-close t)
* (loop (sleep 1))
```
- Now return to emacs and make a connection to that external slime/slynk server:
```lisp
M-x slime-connect
```
- Then either load the file at the REPL:
```lisp
CL-USER> (load "example-wyatts.lisp")
```
or put the file in th editor with C-x C-s and then send it to the external swank/slynk server via M-x slime-eval-buffer (or M-x sly-eval-buffer)
- You should see the asterisks printed out in the 

# Video: What it should look like

When run, both the C and Lisp versions should produce output that looks like [this](https://github.com/user-attachments/assets/c3d3b023-520b-41cb-b8d7-752378853e30).

