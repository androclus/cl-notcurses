(in-package :cl-notcurses)

;; Pull in a fast, inlined version of core initialization
(declaim (inline %notcurses-core-init))
(defcfun ("notcurses_core_init" %notcurses-core-init) :pointer
  (opts :pointer)
  (fp :pointer))

(defun start00 ()
  (declare (optimize (speed 3) (safety 1)))
  (let ((nc-handle (cffi:null-pointer)))
    (unwind-protect
         (progn
           ;; 1. Zero-allocation initialization: Pass NULL pointers!
           ;; Notcurses automatically infers standard output and system defaults.
           (setf nc-handle (%notcurses-core-init (cffi:null-pointer) (cffi:null-pointer)))
           
           (when (cffi:null-pointer-p nc-handle)
             (error "Failed to initialize Notcurses engine."))

           ;; 2. Print straight to the standard plane wrapper
           (let ((nc-stdplane (%notcurses-stdplane nc-handle)))
             (%ncplane-putstr-yx nc-stdplane 0 0 "hello world"))

           ;; 3. Blast the frame to the hardware display server
           (%notcurses-render nc-handle)
           (sleep 2))
      
      ;; Cleanup block executes cleanly without tracking file handles or structs
      (unless (cffi:null-pointer-p nc-handle)
        (%notcurses-stop nc-handle)))))

(defun main ()
  "Multi-environment script runner supporting both SLY/Slynk and SLIME/Swank."
  ;; 1. Execute your core example logic
  (start00)

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

(main)
