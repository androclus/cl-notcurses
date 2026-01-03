;;; 00-hello-world.lisp
;;;   https://github.com/androclus/cl-notcurses/blob/main/examples/00-hello-world.lisp
;;;
;;; A Lisp CFFI wrapper version of the C notcurses simple example
;;;   https://github.com/dankamongmen/libnotcurses-sys/blob/main/examples/C/00-hello-world.c
;;;
;;; This program starts up notcurses and writes a simple string at 0,0 (top left corner of terminal)
;;; using the ncplane-putstr-yx wrapper function.
;;;
;;; Requires the following shared libraries be installed on your system:
;;;   - libnotcurses (e.g. /usr/lib64/libnotcurses.so.3)
;;;   - libnotcurses-ffi3 (e.g. /usr/lib64/libnotcurses-ffi.so.3)
;;;
;;; ..and of course a Common Lisp engine. (Built and tested on SBCL and Clisp.)

(in-package :cl-notcurses)

(defun start00 ()
  ;; Let's put the whole section below within a "try/catch" so that
  ;; we can make sure that no matter what happens, we will free up memory
  ;; and return the terminal to its normal operating mode.
  (let ((nc-handle)
        (nc-stdplane)
        (nc-opts-ptr)
        (putstr-output)
        (render-output)
        (file))
    (unwind-protect
         (progn ; protected form ("try")
           ;; get a (C) handle to STDOUT, hopefully narrow (byte, ie only 8-bit wide)
           (setf file (fopen "/dev/stdout" "a"))
           ;; create a global pointer and allocate memory (in C space) for the
           ;; notcurses-options struct
           (setf nc-opts-ptr (cffi:foreign-alloc '(:struct notcurses-options)))
           ;; Access and set the fields using foreign-slot-value (or with-foreign-slots)
           (setf (foreign-slot-value nc-opts-ptr '(:struct notcurses-options) 'term) "" ; notcurses should fill this in from env
                 (foreign-slot-value nc-opts-ptr '(:struct notcurses-options) 'loglevel) 0
                 (foreign-slot-value nc-opts-ptr '(:struct notcurses-options) 'margin-t) 0
                 (foreign-slot-value nc-opts-ptr '(:struct notcurses-options) 'margin-r) 0
                 (foreign-slot-value nc-opts-ptr '(:struct notcurses-options) 'margin-b) 0
                 (foreign-slot-value nc-opts-ptr '(:struct notcurses-options) 'margin-l) 0
                 (foreign-slot-value nc-opts-ptr '(:struct notcurses-options) 'flags) 0)
           ;; Initialize notcurses, picking up the handle (pointer) to the main ncobject
           (setf nc-handle (notcurses-init nc-opts-ptr file))
           ;; Get the stdplane from the nc object
           (setf nc-stdplane (notcurses-stdplane nc-handle))
           ;; Write the string to the standard plane
           (setf putstr-output (ncplane-putstr-yx nc-stdplane 0 0 "hello world"))
           ;; now render (show on screen)
           (setf render-output (notcurses-render nc-handle))
           ;; wait a couple of seconds at the end
           (sleep 2)
           )  ; progn protected form
      (progn ; cleanup form ("catch")
        ;; unwind-protect cleanup forms:
        ;; 1. free/release the notcurses object (and everything it contains including the stdplane)
        (notcurses-stop nc-handle)
        ;; 2. free the options struct
        (foreign-free nc-opts-ptr)
        ;; 3. close the output stream
        (fclose file)
        ) ; progn cleanup form
      ) ; unwind-protect
    ) ; let
  )  ; defun start00
