;;; 02-capabilities.lisp
;;;   https://github.com/androclus/cl-notcurses/blob/main/examples/02-capabilities.lisp
;;;
;;; A Lisp CFFI wrapper version of the C notcurses simple example
;;;   https://github.com/dankamongmen/libnotcurses-sys/blob/main/examples/C/02-capabilities.c
;;;
;;; This program starts up and polls notcurses for simple information, then prints it out

(in-package :cl-notcurses)

(defun start02 ()
    (let ((nc-handle) ; handle to main notcurses object
        (nc-stdplane) ; handle to the main (standard) plane within the notcurses object
        (nc-opts-ptr) ; a pointer to the notcurses options struct
        ;;        (putstr-result) ; result of the putstr output (unused though in this example)
        ;;        (render-result) ; result of the render output (unused though in this example)
        (ncoflags) ; bitfield for the flags we do want to set in this example
        (caps-ptr) ; a pointer to an nccapabilities struct
        (file) ; handle to *standard-output*
;;;        (was-scrolling-before (cffi:foreign-alloc :bool)) ; whether the plane had been scrolling before we told it to
        (dimy-ptr (cffi:foreign-alloc :uint)) ; height (in rows) of terminal
        (dimx-ptr (cffi:foreign-alloc :uint)) ; width (in columns) of terminal
        (str-osversion (cffi:foreign-alloc :string)) ; version of the o/s (according to notcurses)
        (str-terminal (cffi:foreign-alloc :string)) ; terminal name (according to notcurses)
        ) ; pointer to the STDOUT (*standard-output*), used to open the terminal before notcurses takes over
      (declare (ignorable caps-ptr)) ; this line is just so we don't get the style-warning that
                                     ; "The variable CAPS-PTR is assigned but never read."

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

           ;; test for success or failure of setting up the basic notcurses object
;;           (if (eql nc-handle nil)
  ;;             (throw))

           ;; Get the stdplane from the nc object
           (setf nc-stdplane (notcurses-stdplane nc-handle))
           ;; I am keeping the example simple, but normally, we would test
           ;; nc-stdplane now for whether it is nil and throw an exception if it is

           ;; set the standard plane to scroll
;;           (setf was-scrolling-before (ncplane-set-scrolling nc-stdplane 1))
           (ncplane-set-scrolling nc-stdplane 1)

           ;; Optionally, initialize these to 0
           (setf (cffi:mem-aref dimy-ptr :uint) 0)
           (setf (cffi:mem-aref dimx-ptr :uint) 0)

           ;; the standard plane size matches the terminal dimensions
           ;; unsigned dimy, dimx;
           ;; ncplane_dim_yx(stdn, &dimy, &dimx);
           (ncplane-dim-yx nc-stdplane dimy-ptr dimx-ptr)

           ;; show the detected os version, terminal name & dimensions
           (setf str-osversion (notcurses-osversion))
           (setf str-terminal (notcurses-detected-terminal nc-handle))

           (ncplane-printf-yx nc-stdplane -1 -1
                              (format nil "Operating System: %s~%Terminal: %s~%Dimensions: %u rows, %u columns~%")
                              :pointer str-osversion :pointer str-terminal
                              :uint (cffi:mem-aref dimy-ptr :uint)
                              :uint (cffi:mem-aref dimx-ptr :uint))

           ;; now get and show the terminal capabilities
           (setf caps-ptr (notcurses-capabilities nc-handle))

           (with-foreign-object (caps-ptr '(:struct nccapabilities))
             ;; Return the slot values starting with utf8
             (with-foreign-slots ((utf8 halfblocks quadrants sextants braille rgb colors) caps-ptr (:struct nccapabilities))
               (let ((utf8-str (if utf8 "true" "false"))
                     (halfblocks-str (if halfblocks "true" "false"))
                     (quadrants-str (if quadrants "true" "false"))
                     (sextants-str (if sextants "true" "false"))
                     (braille-str (if braille  "true" "false"))
                     (rgb-str (if rgb "true" "false"))
                     (can-pixel-str (if (notcurses-canpixel nc-handle) "true" "false")))
                 (ncplane-printf-yx nc-stdplane -1 -1
                                    (format nil "utf8: %s~%halfblocks: %s~%quadrants: %s~%sextants: %s~%braille: %s~%pixel: %s~%24bit-color: %s~%palette colors: %i~%~%")
                                    :string utf8-str
                                    :string halfblocks-str
                                    :string quadrants-str
                                    :string sextants-str
                                    :string braille-str
                                    :string can-pixel-str
                                    :string rgb-str
                                    :uint colors))))

           ;; now render (show on screen)
           (notcurses-render nc-handle)
           ;; Again, we could test this with a result var and checking..
           ;; (setf render-output (notcurses-render nc-handle))

           ) ; progn protected form
      (progn ;unwind-protect cleanups
        ;; 1. free/release the notcurses object (and everything it contains including the stdplane)
        (notcurses-stop nc-handle)
        ;; 2. free the options struct and other foreign objects
        (foreign-free nc-opts-ptr)
        (cffi:foreign-free dimy-ptr)
        (cffi:foreign-free dimx-ptr)
        (cffi:foreign-free str-osversion)
        (cffi:foreign-free str-terminal)
        ;; 3. close the *standard-output* stream
        (fclose file)
        ) ; progn cleanup form
      ) ; unwind-protect
    ) ; let
  )  ; defun start02
