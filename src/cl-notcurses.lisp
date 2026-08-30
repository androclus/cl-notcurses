(in-package :cl-notcurses)

;; A safe constant mapping to LC_ALL on Linux systems
(defconstant +lc-all+ 6)

;; 2. Update the options structure map to accept a raw pointer for the term string field
(cffi:defcstruct notcurses-options
  (term :pointer)       ; Changed from :string to raw pointer to guarantee absolute C null safety
  (loglevel :int)
  (margin-t :int)
  (margin-r :int)
  (margin-b :int)
  (margin-l :int)
  (flags :uint64))



;;; 1. Library Loading Blocks
(define-foreign-library libnotcurses3
  (:darwin (:or "libnotcurses.3.dylib" "libnotcurses.dylib"))
  (:unix (:or "libnotcurses.so.3" "libnotcurses.so"))
  (t (:default "libnotcurses")))
(use-foreign-library libnotcurses3)

(define-foreign-library libnotcurses-core3
  (:darwin (:or "libnotcurses-core.3.dylib" "libnotcurses-core.dylib"))
  (:unix (:or "libnotcurses-core.so.3" "libnotcurses-core.so"))
  (t (:default "libnotcurses-core")))
(use-foreign-library libnotcurses-core3)

(define-foreign-library libnotcurses-ffi3
  (:darwin (:or "libnotcurses-ffi.3.dylib" "libnotcurses-ffi.dylib"))
  (:unix (:or "libnotcurses-ffi.so.3" "libnotcurses-ffi.so"))
  (t (:default "libnotcurses-ffi")))
(use-foreign-library libnotcurses-ffi3)

;;; 2. Data layouts such as structs, bitfields, etc not always used but useful
;;; in certain situations

;;; .. when we want to know the apabilities, derived from terminfo, environment
;;; variables, and queries, then we need to pass this structure to ncurses and
;;; let it fill the values and pass it back.
(cffi:defcstruct nccapabilities
  (colors :uint) ; size of palette for indexed colors
  (utf8 :boolean) ; are we using utf-8 encoding? from nl_langinfo(3)
  (rgb :boolean) ; 24bit color? COLORTERM/heuristics/terminfo 'rgb'
  (can-change-colors :boolean) ; can we change the palette? terminfo 'ccc'
  ;; these are assigned wholly through TERM- and query-based heuristics
  (halfblocks :boolean) ; we assume halfblocks, but some are known to lack them
  (quadrants :boolean) ; do we have (good, vetted) Unicode 1 quadrant support?
  (sextants :boolean) ; do we have (good, vetted) Unicode 13 sextant support?
  (octants :boolean)  ; do we have (good, vetted) Unicode 16 octant support?
  (braille :boolean)) ; do we have Braille support? (linux console does not)

;;; .. when we want to initialize a session by getting an nc-handle, but with
;;; options other than just the default (which we get when we pass NULL). For
;;; instance, when we want to NOT take over the entire screen, but run notcurses
;;; in CLI mode, then we'll need to set these values, as in example 01.
;; (cffi:defcstruct notcurses-options
;;   (term :string)
;;   (loglevel :int)
;;   (margin-t :int)
;;   (margin-r :int)
;;   (margin-b :int)
;;   (margin-l :int)
;;   (flags :int64))

;;; Add this back into your main cl-notcurses.lisp base file:
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
  (:ncoption-cli-mode #x0252))

(defun ncoptions-flags-bitfield-value (x)
  (declare (type list x))
  "Return the bitfield sum of all the options flags in the list x."
  (foreign-bitfield-value 'ncoption-flags x))


;;; 4. Direct C API Binding Wrappers

(declaim (inline %notcurses-core-init %ncplane-putstr-yx))

(defcfun ("notcurses_core_init" %notcurses-core-init) :pointer
  (opts :pointer)
  (fp :pointer))

(defcfun ("ncplane_putstr_yx" %ncplane-putstr-yx) :int
  (stdplane :pointer)
  (y :int)
  (x :int)
  (str :string))

(defcfun ("notcurses_render" %notcurses-render) :int
  (nchandle :pointer))

(defcfun ("notcurses_stop" %notcurses-stop) :void
  (nc :pointer))

(defcfun ("notcurses_stdplane" %notcurses-stdplane) :pointer
  (nc :pointer))

(defcfun ("ncplane_dim_yx" %ncplane-dim-yx) :void
  (stdplane :pointer)
  (dimy-ptr (:pointer :int))
  (dimx-ptr (:pointer :int)))

(defcfun ("ncplane_set_scrolling" %ncplane-set-scrolling) :bool
  (stdplane :pointer)
  (scrollp :uint))

(defcfun ("notcurses_osversion" %notcurses-osversion) :pointer )

(defcfun ("notcurses_detected_terminal" %notcurses-detected-terminal) :pointer
  (nc :pointer))

(defcfun ("notcurses_capabilities" %notcurses-capabilities) :pointer
  (nc :pointer))

(defcfun ("notcurses_canpixel" %notcurses-canpixel) :boolean
(nc :pointer))
