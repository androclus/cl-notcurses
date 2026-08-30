(in-package :cl-notcurses)

(defun start01 ()
  (declare (optimize (speed 3) (safety 1)))

  ;; Wakes up the system's UTF-8 terminal translation capabilities
  ;;(setlocale +lc-all+ "")

  (let ((nc-handle (cffi:null-pointer)))
    (unwind-protect
         ;; Allocate option storage cleanly inside the stack
         (cffi:with-foreign-objects ((opts-ptr '(:struct notcurses-options)))

           (cffi:with-foreign-slots ((term loglevel margin-t margin-r margin-b margin-l flags)
                                     opts-ptr (:struct notcurses-options))

             ;; Pass a clean C NULL pointer to force environmental shell $TERM parsing
             (setf term (cffi:null-pointer))

             (setf loglevel 0)
             (setf margin-t 0)
             (setf margin-r 0)
             (setf margin-b 0)
             (setf margin-l 0)
             (setf flags (ldb (byte 64 0)
                              (ncoptions-flags-bitfield-value
                               '(:ncoption-suppress-banners
                                 :ncoption-preserve-cursor
                                 :ncoption-no-alternate-screen
                                 :ncoption-no-clear-bitmaps
                                 :ncoption-drain-input)))))

           ;; Initialize the engine
           (setf nc-handle (%notcurses-core-init opts-ptr (cffi:null-pointer)))
           (when (cffi:null-pointer-p nc-handle)
             (error "Failed to initialize CLI mode engine."))

           (let ((nc-stdplane (%notcurses-stdplane nc-handle)))
             (%ncplane-putstr-yx nc-stdplane -1 -1 "hello world ")
             (%notcurses-render nc-handle)))

      ;; Relinquish terminal lock control
      (unless (cffi:null-pointer-p nc-handle)
        (%notcurses-stop nc-handle)
        (format t "~%")))))

(defun main ()
  "Multi-environment script runner supporting both SLY/Slynk and SLIME/Swank."
  ;; 1. Execute your core example logic
  (start01)

  ;; 2. Dynamically check the environment
  (cond
    ;; Path A: Running inside Emacs SLY/Slynk environment
    ((or (member :sly-repl *features*)
         (find-package :slynk))
     :slynk-server-active)

    ;; Path B: Running inside Emacs SLIME/Swank environment
    ((or (member :slime-repl *features*)
         (find-package :swank))
     :swank-server-active)

    ;; Path C: Raw standalone shell script environment -> Exit completely
    (t
     (sb-ext:exit))))

;; Run the program immediately upon script invocation
(main)
