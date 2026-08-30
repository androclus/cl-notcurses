
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
