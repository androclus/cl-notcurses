
(load "~/quicklisp/setup.lisp")
(ql:quickload "cl-setlocale")
(ql:quickload "asdf")
(ql:quickload "cffi")
(asdf:load-system :cffi)
(cl-setlocale:setlocale :lc-all "")

;;; Nothing special about the "CFFI-USER" package.  We're just
;;; using it as a substitute for your own CL package.
(defpackage :cffi-notcurses
  (:use :common-lisp :cffi :cl :cl-user))

(in-package :cffi-notcurses)

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
(defcfun ("notcurses_stdplane" notcurses-stdplane) :pointer (nc :pointer))

;;; C example:
;;;    ncplane_putchar_yx(stdplane, i, j, '*');
(defcfun ("ncplane_putchar_yx" ncplane-putchar-yx) :int (stdplane :pointer) (i :int) (j :int) (c :uchar))

;;; C example:
;;;    notcurses_render(nc);
(defcfun ("notcurses_render" notcurses-render) :int (nchandle :pointer))

(define-foreign-library libc
  (:darwin (:or "libc.6.dylib" "libc.dylib"))
  (:unix (:or "libc.so.6" "libc.so"))
  (t (:default "libc")))
(use-foreign-library libc)

;; Define the C structure for a file pointer
(defcstruct file-pointer
    (ptr :pointer))

;; Define a foreign function to open a file
(defcfun ("fopen" fopen) :pointer
    (filename :string)
    (mode :string))

;; Define a foreign function to close a file
(defcfun ("fclose" fclose) :int
    (file :pointer))

(defcfun ("fprintf" fprintf) :int
  (file :pointer)
  (str :string))

