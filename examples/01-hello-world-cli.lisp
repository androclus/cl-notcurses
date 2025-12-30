;;; 01-hello-world-cli.lisp
;;;   https://github.com/androclus/cl-notcurses/blob/main/examples/00-hello-world-cli.lisp
;;;
;;; A Lisp CFFI wrapper version of the C notcurses simple example
;;;   https://github.com/dankamongmen/libnotcurses-sys/blob/main/examples/C/01-hello-world-cli.c
;;;
;;; This program starts up notcurses and writes a simple string, again using the
;;; ncplane-putstr-yx wrapper function, but this time starting at wherever the
;;; cursor was last sitting.
;;;
;;; Load the notcurses lisp wrappers defined in the parent directory of this
;;; repository. (If you run from elsewhere or from inside Emacs, you may need to
;;; set this to an absolute pathname.)
;;;(load "../cffi-notcurses.lisp")
(load "/home/jeff/nc/cl/cffi/cl-notcurses/cffi-notcurses.lisp")

;;; pick up our CFFI notcurses wrapper definitions
(in-package :cffi-notcurses)

(defparameter *nc-handle* nil)
(defparameter *nc-stdplane* nil)
(defparameter *nc-opts-ptr* nil)
(defparameter *putstr-output* nil) ;; result of putstr (unused in this example)
(defparameter *render-output* nil) ;; result of render (unused in this example)
(defparameter *ncoflags* nil) ;; flags field of notcurses options, to set behavior
(defparameter *move-rel-output* nil) ;; result of moverel (unused in this example)

;;; get a (C) handle to STDOUT, hopefully narrow (byte, ie only 8-bit wide)
(defparameter *file* (fopen "/dev/stdout" "a"))

;;; Let's put the whole section below within a "try/catch" so that
;;; we can make sure that no matter what happens, we will free up memory
;;; and return the terminal to its normal operating mode.
(unwind-protect
     (progn
       ;; create a global pointer and allocate memory (in C space) for the
       ;; notcurses-options struct
       (defparameter *nc-opts-ptr* (cffi:foreign-alloc '(:struct notcurses_options)))

       ;; set our flags
       ;; this should technically come to 370 decimal
       (defparameter *ncoflags* (ncoptions-flags-bitfield-value '(:ncoption-suppress-banners
                                                                  :ncoption-preserve-cursor
                                                                  :ncoption-no-alternate-screen
                                                                  :ncoption-no-clear-bitmaps
                                                                  :ncoption-drain-input)))

       ;; Access and set the fields using foreign-slot-value (or with-foreign-slots)
       (setf
        (foreign-slot-value *nc-opts-ptr* '(:struct notcurses_options) 'term) "" ;; notcurses should fill this in from env
        (foreign-slot-value *nc-opts-ptr* '(:struct notcurses_options) 'loglevel) 0
        (foreign-slot-value *nc-opts-ptr* '(:struct notcurses_options) 'margin-t) 0
        (foreign-slot-value *nc-opts-ptr* '(:struct notcurses_options) 'margin-r) 0
        (foreign-slot-value *nc-opts-ptr* '(:struct notcurses_options) 'margin-b) 0
        (foreign-slot-value *nc-opts-ptr* '(:struct notcurses_options) 'margin-l) 0
        (foreign-slot-value *nc-opts-ptr* '(:struct notcurses_options) 'flags) *ncoflags*)

       ;; Initialize notcurses, picking up the handle (pointer) to the main nc object
       (defparameter *nc-handle* (notcurses-core-init *nc-opts-ptr* *file*))

       ;; Get the stdplane from the nc object
       (defparameter *nc-stdplane* (notcurses-stdplane *nc-handle*))

       ;; Write the string to the standard plane
       ;; I add the space at the end of the string because
       ;; putstr does not automatcially move the cursor one
       ;; cell to the right after the last character ('d').
       (defparameter *putstr-output* (ncplane-putstr-yx *nc-stdplane* -1 -1 "hello world "))

       ;; now render (show on screen)
       (defparameter *render-output* (notcurses-render *nc-handle*)))

  ;; unwind-protect cleanups:
  ;; 1. free/release the notcurses object (and everything it contains including the stdplane)
  (notcurses-stop *nc-handle*)
  ;; 2. free the options struct
  (foreign-free *nc-opts-ptr*)
  ;; 3. close the output stream
  (fclose *file*))
