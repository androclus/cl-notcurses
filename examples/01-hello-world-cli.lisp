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
;;; (load "/home/jeff/nc/cl/cffi/cl-notcurses/cffi-notcurses.lisp")

;;; pick up our CFFI notcurses wrapper definitions
(in-package :cl-notcurses)

(defun start01 ()
  (let ((nc-handle) ; handle to main notcurses object
        (nc-stdplane) ; handle to the main (standard) plane within the notcurses object
        (nc-opts-ptr) ; a pointer to the notcurses options struct
        ;;        (putstr-result) ; result of the putstr output (unused though in this example)
        ;;        (render-result) ; result of the render output (unused though in this example)
        (ncoflags) ; bitfield for the flags we do want to set in this example
        (file)) ; pointer to the STDOUT (*standard-output*), used to open the terminal before notcurses takes over
    ;; Let's put the whole section below within a "try/catch" so that
    ;; we can make sure that no matter what happens, we will free up memory
    ;; and return the terminal to its normal operating mode.
    (unwind-protect
         (progn ; protected form (as in Java's "try")
           ;; get a (C) handle to STDOUT, hopefully narrow (byte, ie only 8-bit wide)
           (setf file (fopen "/dev/stdout" "a"))
           ;; create a global pointer and allocate memory (in C space) for the
           ;; notcurses-options struct
           (setf nc-opts-ptr (cffi:foreign-alloc '(:struct notcurses-options)))

           ;; set our flags
           ;; this should technically come to 370 decimal
           (setf ncoflags (ncoptions-flags-bitfield-value
                           '(:ncoption-suppress-banners
                             :ncoption-preserve-cursor
                             :ncoption-no-alternate-screen
                             :ncoption-no-clear-bitmaps
                             :ncoption-drain-input)))

           ;; Access and set the fields using foreign-slot-value (or with-foreign-slots)
           (setf
            (foreign-slot-value nc-opts-ptr '(:struct notcurses-options) 'term) "" ; notcurses should fill this in from env
            (foreign-slot-value nc-opts-ptr '(:struct notcurses-options) 'loglevel) 0
            (foreign-slot-value nc-opts-ptr '(:struct notcurses-options) 'margin-t) 0
            (foreign-slot-value nc-opts-ptr '(:struct notcurses-options) 'margin-r) 0
            (foreign-slot-value nc-opts-ptr '(:struct notcurses-options) 'margin-b) 0
            (foreign-slot-value nc-opts-ptr '(:struct notcurses-options) 'margin-l) 0
            (foreign-slot-value nc-opts-ptr '(:struct notcurses-options) 'flags) ncoflags)

           ;; Initialize notcurses, picking up the handle (pointer) to the main nc object
           (setf nc-handle (notcurses-core-init nc-opts-ptr file))

           ;; Get the stdplane from the nc object
           (setf nc-stdplane (notcurses-stdplane nc-handle))
           ;; I am keeping the example simple, but normally, we would test
           ;; nc-stdplane now for whether it is nil and throw an exception if it is

           ;; Write the string to the standard plane
           ;; I add the space at the end of the string because
           ;; putstr does not automatcially move the cursor one
           ;; cell to the right after the last character ('d').
           (ncplane-putstr-yx nc-stdplane -1 -1 "hello world ")
           ;; Again, I am keeping the example simple in order to match the .c version,
           ;; but normally we would want to check the result of trying to put the char
           ;; this way:
           ;; (setf putstr-result (ncplane-putstr-yx nc-stdplane -1 -1 "hello world "))
           ;; (if putstr-result ...)

           ;; now render (show on screen)
           (notcurses-render nc-handle)
           ;; Again, we could test this with a result var and checking..
           ;; (setf render-output (notcurses-render nc-handle))

           ) ; progn protected form
      (progn ; cleanup form (a combination of Java's "catch" and finally")
        ;; unwind-protect cleanups:
        ;; 1. free/release the notcurses object (and everything it contains including the stdplane)
        (notcurses-stop nc-handle)
        ;; 2. free the options struct
        (foreign-free nc-opts-ptr)
        ;; 3. close the output stream
        (fclose file)
        ) ; progn cleanup form
      ) ; unwind-protect
    ) ; let
  )  ; defun start01
