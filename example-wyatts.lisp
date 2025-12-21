;;; try.lisp
;;; --------------------------------------------------------------------------
;;; program attempts start here

;;; change next line to match location of cffi-notcurses.lisp on your machine
(load "/home/jeff/nc/cl/cffi/cl-notcurses/cffi-notcurses.lisp")
(in-package :cffi-notcurses)

;;; Testing fopen and fclose FFI's
;;; This does work.
;;;(defparameter *file* (fopen "/home/jeff/temp/test.txt" "a"))
;;;(fprintf *file* "This time I mean it REALLYREAllY.")
;;;(fclose *file*)

;;; Now let's try it with stdout and notcurses FFI's
;;; get a (C) handle to STDOUT, hopefully narrow (byte, ie only 8-bit wide)
(defparameter *file* (fopen "/dev/stdout" "a"))

(defcstruct notcurses_options
  (term :string)
  (loglevel :int)
  (margin-t :int)
  (margin-r :int)
  (margin-b :int)
  (margin-l :int)
  (flags :int64))

(defparameter *nc-opts-ptr* nil)
(defparameter *nc-handle* nil)
(defparameter *nc-stdplane* nil)

;;; Let's put the whole section below within a try/catch so
;;; we can make sure we free up memory and return the terminal to
;;; its normal operating mode no matter what happens.
(unwind-protect
     (progn
       ;; create a global pointer and allocate memory (in C space) for the notcurses-options struct
       (defparameter *nc-opts-ptr* (cffi:foreign-alloc '(:struct notcurses_options)))

       ;; Access and set the fields using foreign-slot-value (or with-foreign-slots)
       (setf
        (foreign-slot-value *nc-opts-ptr* '(:struct notcurses_options) 'term) "xterm-kitty"
        (foreign-slot-value *nc-opts-ptr* '(:struct notcurses_options) 'loglevel) 0
        (foreign-slot-value *nc-opts-ptr* '(:struct notcurses_options) 'margin-t) 0
        (foreign-slot-value *nc-opts-ptr* '(:struct notcurses_options) 'margin-r) 0
        (foreign-slot-value *nc-opts-ptr* '(:struct notcurses_options) 'margin-b) 0
        (foreign-slot-value *nc-opts-ptr* '(:struct notcurses_options) 'margin-l) 0
        (foreign-slot-value *nc-opts-ptr* '(:struct notcurses_options) 'flags) 0)

;;; Debugging/verifying the notcurses_options C struct above
;;;(with-foreign-slots ((term loglevel margin-t margin-r margin-b margin-l flags) *nc-opts-ptr* (:struct notcurses_options))
;;;  (print (list "term:" term "loglevel:" loglevel "margin-t:" margin-t "margin-r:" margin-r
;;;               "margin-b:" margin-b "margin-l:" margin-l "flags:" flags)))
;;;(terpri)

       ;; Initialize notcurses, picking up the handle (pointer) to the main nc object
       (defparameter *nc-handle* (notcurses-init *nc-opts-ptr* *file*))
       
       ;; Get a stdplane
       (defparameter *nc-stdplane* (notcurses-stdplane *nc-handle*))

       ;; make my beloved while macro
       ;; courtesy of https://stackoverflow.com/questions/65304891/how-to-do-a-while-loop-in-common-lisp
       (defmacro while (test &body decls/tags/forms)
         `(do () ((not ,test) (values))
            ,@decls/tags/forms))

       ;; y (rows): we will make 25 rows of asterisks
       (defparameter y 1)
       (defparameter ymax 26)
       ;; x (columns): we will make 25 (columns of) asterisks in each row
       (defparameter x 1)
       (defparameter xmax 26)

       ;; return value for putting a character into a cell
       (defparameter pcint nil)

       (while (< y ymax)
         (while (< x xmax)
           ;; draw the asterisk at row y, column x
           (setq pcint (ncplane-putchar-yx *nc-stdplane* y x 42 ))
           ;; Register to the screen
           (defparameter render-output (notcurses-render *nc-handle*))
           (setf x (1+ x))
           ;; pause for 50 milliseconds
           (sleep 0.005))
         (setf x 1) ; return to 1st column
         (setf y (1+ y))) ; and go down to next row

       ;; wait a couple of seconds at the end
       (sleep 2))

  ;; unwind-protect:
  ;; Make sure thea following 3 cleanups are done before quitting
  ;; 1. free/release the notcurses object (and everything it contains)
  (notcurses-stop *nc-handle*)
  ;; 2. free the options struct
  (foreign-free *nc-opts-ptr*)
  ;; 3. close the stream
  (fclose *file*))
