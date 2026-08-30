(in-package :cl-user)

(defpackage :cl-notcurses
  ;; Keep it lean: only standard Common Lisp and the CFFI layer!
  (:use :common-lisp :cffi)

  ;; Export your public macros, types, helper functions, and examples
  (:export #:ncoption-flags
           #:ncoptions-flags-bitfield-value
           #:nccapabilities
           #:notcurses-options

           ;; Raw Internal % API functions
           #:%notcurses-core-init
           #:%notcurses-stdplane
           #:%ncplane-putstr-yx
           #:%notcurses-render
           #:%notcurses-stop
           #:%notcurses-detected-terminal
           #:%notcurses-osversion
           #:%notcurses-canpixel
           #:%notcurses-capabilities
           #:%ncplane-cursor-move-rel
           #:%ncplane-dim-yx
           #:%ncplane-set-scrolling

           ;; Clean Example Entry Points
           #:start00
           #:start01
           #:start02
           #:start03))
