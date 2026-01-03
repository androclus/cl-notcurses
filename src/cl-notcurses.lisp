;;; cffi-notcurses.lisp
;;; https://github.com/androclus/cl-notcurses
;;; first rough draft of minimal set of notcurses CFFI wrappers
;;; to run Wyatt Sheffield's simple demo app as a Lisp program.

;;; Once I move system to asdf (haven't learned it yet) much of this
;;; below will be reduced. For now, this all just loads what is
;;; necessary into your running Lisp image in the script itself.
;;;(load "~/quicklisp/setup.lisp")
;;;(ql:quickload "cl-setlocale")
;;;(ql:quickload "asdf")
;;;(ql:quickload "cffi")
;;;(asdf:load-system :cffi)
(cl-setlocale:setlocale :lc-all "")

(in-package :cl-notcurses)

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

;;; notcurses options structure
(cffi:defcstruct notcurses-options
  (term :string)
  (loglevel :int)
  (margin-t :int)
  (margin-r :int)
  (margin-b :int)
  (margin-l :int)
  (flags :int64))

;;; Bitfield values for the notcurses_options flags field. See notcurses.h for
;;; the original C values. (I also got help on how to do this from
;;; https://www.quicklisp.org/beta/UNOFFICIAL/docs/cffi/doc/defbitfield.html )
(defbitfield ncoption-flags
  (:ncoption-inhibit-setlocale #x0001)
  (:ncoption-no-clear-bitmaps #x0002)
  (:ncoption-no-winch-sighandler #x0004)
  (:ncoption-no-quit-sighandlers #x0008)
  (:ncoption-preserve-cursor #x0010)
  (:ncoption-suppress-banners #x0020)
  (:ncoption-no-alternate-screen #x0040)
  (:ncoption-no-font-changes #x0080)
  (:ncoption-drain-input #x0100)
  (:ncoption-scrolling #x0200)
  (:ncoption-cli-mode #x0252)) ; "CLI MODE": A convenience for an often-used combination
                               ; which sets these four all at once:
                               ;   x0040 no alternate screen
                               ;   x0002 no clear bitmaps
                               ;   x0010 preserve cursor
                               ;   x0200 scrolling
                               ;     => x252, i.e., 594

;;; I add a convenience function here which is not in notcurses.h
(defun ncoptions-flags-bitfield-value (x)
  (declare (type list x))
  "Return the bitfield sum of all the options flags in the list x.
Then this value may be used to set the flags field of the notcurses-options cstruct.

Example:
(ncoptions-flags-bitfield-value '(:ncoption-preserve-cursor
                                  :ncoption-scrolling)) => #x0210 (528)"
  (foreign-bitfield-value 'ncoption-flags x))

;;; C Def (include/notcurses/notcurses.h):
;;;    API ALLOC struct notcurses* notcurses_init(const notcurses_options* opts, FILE* fp);
;;; C Example:
;;;    struct notcurses* nc = notcurses_init(&ncopt, stdout); */
;;;
;;; Allocate a handle to a new notcurses instance
;;; As parameters, give it a pointer to a notcurses-options struct, and to stdout
(defctype nchandle :pointer)
(defcfun ("notcurses_init" notcurses-init) :pointer (opts :pointer) (fp :pointer ))

;;; And the (lightweight) version for cases where there will be no "media" needs
(defcfun ("notcurses_core_init" notcurses-core-init) :pointer (opts :pointer) (fp :pointer ))

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
;;;
;;; Note to CL users: For the c uchar, you need to supply a simple integer. For
;;; instance, to draw an asterisk *, you'd need to supply a 42 (which is an
;;; asterisk in ASCII) or else (char-code #\*) which results in the same 42
(defcfun ("ncplane_putchar_yx" ncplane-putchar-yx) :int (stdplane :pointer) (y :int) (x :int) (c :uchar))

;;; ncplane_putstr_yx(notcurses_stdplane(nc), 0, 0, "hello world");
(defcfun ("ncplane_putstr_yx" ncplane-putstr-yx) :int (stdplane :pointer) (y :int) (x :int) (s :string))

;;; Move the cursor relative to current position
;;; API int ncplane_cursor_move_rel(struct ncplane* n, int y, int x)
(defcfun ("ncplane_cursor_move_rel" ncplane-cursor-move-rel) :int (stdplane :pointer) (y :int) (x :int))

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

