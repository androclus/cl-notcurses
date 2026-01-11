(defpackage cl-notcurses
  (:use :common-lisp :cffi :cl :cl-user :cl-setlocale)
  (:export
   #:notcurses-options
   #:ncoption-flags
   #:ncoptions-flags-bitfield-value
   #:nchandle
   #:notcurses-detected-terminal
   #:notcurses-canpixel
   #:notcurses-capabilities
   #:notcurses-core-init
   #:notcurses-init
   #:notcurses-osversion
   #:notcurses-render
   #:notcurses-stop
   #:notcurses-stdplane
   #:ncplane-cursor-move-rel
   #:ncplane-dim-yx
   #:ncplane-printf-yx
   #:ncplane-putchar-yx
   #:ncplane-putstr-yx
   #:ncplane-set-scrolling
   #:file-pointer
   #:fopen
   #:fclose
   #:fprintf))
