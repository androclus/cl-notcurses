(defpackage cl-notcurses
  (:use :common-lisp :cffi :cl :cl-user :cl-setlocale)
  (:export
   #:notcurses-options
   #:ncoption-flags
   #:ncoptions-flags-bitfield-value
   #:nchandle
   #:notcurses-init
   #:notcurses-core-init
   #:notcurses-stop
   #:notcurses-stdplane
   #:ncplane-putchar-yx
   #:ncplane-putstr-yx
   #:ncplane-cursor-move-rel
   #:notcurses-render
   #:file-pointer
   #:fopen
   #:fclose
   #:fprintf))
