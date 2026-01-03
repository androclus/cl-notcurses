;;; 03-asterisks.lisp
;;;   https://github.com/androclus/cl-notcurses/blob/main/examples/03-asterisks.lisp
;;;
;;; A Lisp CFFI wrapper version of the C notcurses simple example
;;;   https://github.com/dankamongmen/libnotcurses-sys/blob/main/examples/C/03-asterisks.c
;;;
;;; This program prints 25 rows of 25 asterisks each, using the CFFI wrapper
;;; ncplane-putchar-yx
;;;
;;; Requires the following shared libraries be installed on your system:
;;;   - libnotcurses (e.g. /usr/lib64/libnotcurses.so.3)
;;;   - libnotcurses-ffi3 (e.g. /usr/lib64/libnotcurses-ffi.so.3)
;;;
;;; ..and of course a Common Lisp engine. (Built and tested on SBCL.)
;;;
;;; Load the notcurses lisp wrappers defined in the parent directory of this
;;; repository
(in-package :cl-notcurses)

;;; make my beloved "while" macro
;;; courtesy of
;;; https://stackoverflow.com/questions/65304891/how-to-do-a-while-loop-in-common-lisp
(defmacro while (test &body decls/tags/forms)
  `(do () ((not ,test) (values))
     ,@decls/tags/forms))


(defun start03 ()
  (let ((nc-handle) ; handle to main notcurses object
        (nc-stdplane) ; handle to the main (standard) plane within the notcurses object
        (nc-opts-ptr) ; a pointer to the notcurses options struct
        ;;        (putstr-result) ; result of the putstr output (unused though in this example)
        ;;        (render-result) ; result of the render output (unused though in this example)
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

           ;; Access and set the fields using foreign-slot-value (or with-foreign-slots)
           (setf
            (foreign-slot-value nc-opts-ptr '(:struct notcurses-options) 'term) "" ;; notcurses should fill this in from env
            (foreign-slot-value nc-opts-ptr '(:struct notcurses-options) 'loglevel) 0
            (foreign-slot-value nc-opts-ptr '(:struct notcurses-options) 'margin-t) 0
            (foreign-slot-value nc-opts-ptr '(:struct notcurses-options) 'margin-r) 0
            (foreign-slot-value nc-opts-ptr '(:struct notcurses-options) 'margin-b) 0
            (foreign-slot-value nc-opts-ptr '(:struct notcurses-options) 'margin-l) 0
            (foreign-slot-value nc-opts-ptr '(:struct notcurses-options) 'flags) 0)

;;; Debugging/verifying the notcurses-options C struct above
;;;(with-foreign-slots ((term loglevel margin-t margin-r margin-b margin-l flags) nc-opts-ptr (:struct notcurses-options))
;;;  (print (list "term:" term "loglevel:" loglevel "margin-t:" margin-t "margin-r:" margin-r
;;;               "margin-b:" margin-b "margin-l:" margin-l "flags:" flags)))
;;;(terpri)

           ;; Initialize notcurses, picking up the handle (pointer) to the main nc object
           (setf nc-handle (notcurses-init nc-opts-ptr file))

           ;; Get the stdplane
           (setf nc-stdplane (notcurses-stdplane nc-handle))

           ;; loop across 25 columns (x's) and down 25 rows (y's), drawing an asterisk
           ;; in each y,x position, and re-rendering each time.
           ;; There are much faster ways of doing this, but this is just a simple demo.
           (let ((ymax 25)    ; number of rows
                 (xmax 25)    ; number of columns
                 (y  0)       ; loop counter for rows
                 (x  0)       ; loop counter for columns
;;                 (putchar-result nil)  ; return value (unused here) for whether putchar was successful
;;                 (render-output nil)  ;; return value (unused here) for whether render was successful
                 (asterisk (char-code #\*))) ;; asterisk (*) is ASCII integer 42
             (while (< y ymax)
                    (while (< x xmax)
                           ;; draw the asterisk at row y, column x
                           (ncplane-putchar-yx nc-stdplane y x asterisk)
                           ;; Again, I am keeping the example simple here, but better practice would
                           ;; be to keep the output result and test it for a throw, e.g.
                           ;;(setq putchar-result (ncplane-putchar-yx nc-stdplane y x asterisk))

                           ;; Register to the screen
                           ;; If you want to try showing all the asterisk lines at once,
                           ;; try moving the following line to just before
                           ;; the (sleep 2) below. (For speed in that case, you may
                           ;; also want to comment out the (sleep 0.005) line as well.)
                           (notcurses-render nc-handle)
                           ;; Same: better practice: keep result and test for a throw and cleanup
                           ;; (setq render-output (notcurses-render nc-handle))
                           (setq x (1+ x)) ;; move to the right one cell
                           ;; pause for 50 milliseconds
                           (sleep 0.005))
                    (setq x 0) ; return to 1st column
                    (setq y (1+ y))) ; and go down to next row

             ;; wait a couple of seconds at the end
             (sleep 2)
             ) ; let
           ) ; progn protected form
      (progn ; cleanup form (a combination of Java's "catch" and finally")
        ;; unwind-protect cleanups:
        ;; 1. free/release the notcurses object (and everything it contains)
        (notcurses-stop nc-handle)
        ;; 2. free the options struct
        (foreign-free nc-opts-ptr)
        ;; 3. close the stream
        (fclose file)
        ) ; progn cleanup form
      ) ; unwind-protect
    ) ; let
  )  ; defun start03
