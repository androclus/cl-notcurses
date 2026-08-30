
# cl-notcurses

A first attempt at a Common Lisp wrapper library for the [notcurses](https://github.com/dankamongmen/notcurses) terminal programming C library by Nick Black.

# Files

- **src/** - cl-notcurses package definition and wrappers
- **examples/** - Example CL programs using the wrappers

# Requirements

- Right now, I am developing on a Linux box, and that's all I have time for right now
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

## Running the examples from inside emacs
1. **git clone** this project into your `~/common-lisp` or `~/quicklisp/local-projects` dir
2. cd into cl-notcurses
3. Start up a Swank or Slynk server (depending on whether you use SLIME or SLY in your Emacs). Convenience scripts have been included in the project's main directory:
   - If you use SLIME (Swank server):
   ```bash
   $ sbcl --load start-slime.lisp
   ```
   - If you use SLY (Slynk server): 
   ```bash
   $ sbcl --load start-slynk.lisp
   ```
4. Now return to emacs and
   - Make a connection to that external swank server (if you use slime as your REPL):
      ```lisp
      M-x slime-connect
      ```
    - or to that external slynk server (if you use sly as your REPL):
       ```lisp
       M-x sly-connect
       ```
    - usually just choose defaults (localhost, port 4005)
5. Load the cl-notcurses project/library:
   ```lisp
   CL-USER> (asdf:load-system "cl-notcurses")
   T
   CL-USER> _
   ```
6. Load an example:
   ```lisp
   CL-USER> (load "~/common-lisp/cl-notcurses/examples/00-hello-world.lisp")
   T
   CL-USER> _
   ```
7. The program should immediately run in the terminal window where you have your SWANK or SLYNK server running.
8. When you are done, you can close down the terminal-window's slime/slynk server from within emacs:
      ```lisp
      CL-USER> (sb-ext:exit)
      ; Evaluation aborted on T
      CL-USER> 
      Process sly-pty-32-1 killed

      ; Server side close
      ; --------------------------------------------------------
      ; Lisp connection closed unexpectedly: connection broken by remote peer
      ; --------------------------------------------------------
        
      ```
## Running the examples directly from a terminal
1. Go to the top directory of the cl-notcurses project (wherever you git cloned it into).

```bash
$ cd ~/common-lisp/cl-notcurses
```
2. There are scripts for each of the 4 examples. For script 00:

``` bash
$ bash ./run00.sh
```
3. Alternatively, make the scripts executable:

``` bash
$ chmod +x run*
```
4. and now you can run them directly:

``` bash
$ ./run00.sh
```
## The Examples

Note: These examples follow the same 3 that Nick demonstrates in C. I have tried (with huge help from Gemini!) to make these as bareboned and direct to the C layer as possible, without relying on FFI pass-by-value translations which slow everything down. Thank goodness Nick wrote an --ffi layer library which makes it easier for all of us in other languages (Rust, CL, Python) to call these functions with the minimum of overhead since his functions accept our data in the format that our languages are used to and we don't have to write any translations.

1. **00-hello-world.lisp**
    - This example just gets notcurses to take over the whole screen, put a "hello world" up in the upper corner, hold it for 2 or 3 seconds, and then quit, returning the terminal to its normal mode.
2. **01-hello-world.lisp**
    - This is the only example I have so far which does /not/ take over the screen mode, but demonstrates notcurses' ability to just work with the CLI. It will put a "hello world" starting wherever your cursor already happens to be.
3. **02-capabilities.lisp**
    - This example shows the ability to get information out of the notcurses system. A few attributes are explored but there are many others.
4. **03-asterisks.lisp**
    - This example shows the ability to run a bunch of asterisks down the screen in rows and columns, with 2 loops. You can see how fast it goes: almost as fast as Nick's C version!
