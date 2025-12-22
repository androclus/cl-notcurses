
;;; Once I move system to asdf (haven't learned it yet) much of this
;;; will be reduced. For now, this all just loads what is necessary
;;; into your running Lisp image in the script itself.
(load "~/quicklisp/setup.lisp")
(ql:quickload "cl-setlocale")
(ql:quickload "asdf")
(ql:quickload "cffi")
(asdf:load-system :cffi)
(cl-setlocale:setlocale :lc-all "")

;;; Name this CL package for wrapping C notcurses calls
(defpackage :cffi-notcurses
  (:use :common-lisp :cffi :cl :cl-user))

;;; Everything declared below this line will be in our package
(in-package :cffi-notcurses)

;;; For connecting to the two shared libraries on any system
;;; which contain all the API and static inline notcurses
;;; calls we'll want to be wrapping.
(define-foreign-library libnotcurses-3-c
  (:darwin (:or "libnotcurses.3.dylib" "libnotcurses.dylib"))
  (:unix (:or "libnotcurses.so.3" "libnotcurses.so"))
  (t (:default "libnotcurses")))
(use-foreign-library libnotcurses-3-c)
(define-foreign-library libnotcurses-3-ffi
  (:darwin (:or "libnotcurses-ffi.3.dylib" "libnotcurses-ffi.dylib"))
  (:unix (:or "libnotcurses-ffi.so.3" "libnotcurses-ffi.so"))
  (t (:default "libnotcurses-ffi")))
(use-foreign-library libnotcurses-3-ffi)

;;; C Def (include/notcurses/notcurses.h):
;;;    API ALLOC struct notcurses* notcurses_init(const notcurses_options* opts, FILE* fp);
;;; C Example:
;;;    struct notcurses* nc = notcurses_init(&ncopt, stdout); */
;;;
;;; Allocate a handle to a new notcurses instance
;;; As parameters, give it a pointer to a notcurses-options struct, and to stdout
(defctype nchandle :pointer)
(defcfun ("notcurses_init" notcurses-init) :pointer (opts :pointer) (fp :pointer ))
;;;(fmakunbound 'notcurses-init)

;;; C Def (include/notcurses/notcurses.h):
;;;    API int notcurses_stop(struct notcurses* nc);
;;; C example:
;;;    notcurses_stop(nc);
;;; Destroy a Notcurses context. A NULL 'nc' is a no-op.
;;; Deallocate handle to notcurses instance
(defcfun ("notcurses_stop" notcurses-stop) :void (nc :pointer))

;;; C example:
;;;    struct ncplane* stdplane = notcurses_stdplane(nc);
;;; Open up the 1st/main (i.e., "standard") plane in the notcurses instance
(defcfun ("notcurses_stdplane" notcurses-stdplane) :pointer (nc :pointer))

;;; C example:
;;;    ncplane_putchar_yx(stdplane, i, j, '*');
;;; Put a character (signed or unsigned 8 bit) into a cell and cell into
;;; the plane
(defcfun ("ncplane_putchar_yx" ncplane-putchar-yx) :int (stdplane :pointer) (i :int) (j :int) (c :uchar))

;;; C example:
;;;    notcurses_render(nc);
;;; Reflect changes made now to the actual screen
(defcfun ("notcurses_render" notcurses-render) :int (nchandle :pointer))

;;; ==================================================================
;;; libc calls for opening/closing STDOUT from the C side prior to and
;;; following the notcurses session. (I was unsuccessful making the
;;; notcurses calls happy when opening *standard-output* from the Lisp
;;; side.)
;;; ==================================================================

;;; The standard libc library includes the I/O we'll need
(define-foreign-library libc
  (:darwin (:or "libc.6.dylib" "libc.dylib"))
  (:unix (:or "libc.so.6" "libc.so"))
  (t (:default "libc")))
(use-foreign-library libc)

;;; Define the C structure for a file pointer
(defcstruct file-pointer
    (ptr :pointer))

;;; Define a foreign function to open a file
(defcfun ("fopen" fopen) :pointer
    (filename :string)
    (mode :string))

;;; Define a foreign function to close a file
(defcfun ("fclose" fclose) :int
    (file :pointer))

;;; not necessary, but I was using it for testing and just left it here
(defcfun ("fprintf" fprintf) :int
  (file :pointer)
  (str :string))

