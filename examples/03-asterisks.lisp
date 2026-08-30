(in-package :cl-notcurses)

(defun start03 ()
  ;; Tell SBCL to maximize execution speed and drop unnecessary safety rails
  (declare (optimize (speed 3) (safety 1) (debug 0)))

  ;; This calls your direct, zero-dependency raw C function binding!
  ;; (setlocale +lc-all+ "")

  (let ((nc-handle (cffi:null-pointer)))
    (unwind-protect
         (progn
           ;; Zero-allocation init: pass NULL pointers to auto-detect environment
           (setf nc-handle (%notcurses-core-init (cffi:null-pointer) (cffi:null-pointer)))

           (when (cffi:null-pointer-p nc-handle)
             (error "Failed to initialize Notcurses engine."))

           (let ((nc-stdplane (%notcurses-stdplane nc-handle))
                 (ymax 25)
                 (xmax 25))
             (declare (type fixnum ymax xmax))

             ;; Clean, type-declared native Lisp execution loop
             (dotimes (y ymax)
               (declare (type fixnum y))
               (dotimes (x xmax)
                 (declare (type fixnum x))

                 ;; Use the modern string wrapper passing an EGC cluster string
                 (%ncplane-putstr-yx nc-stdplane y x "*")

                 ;; Sync frames straight to the hardware display server
                 (%notcurses-render nc-handle)

                 ;; optional 5 millisecond pause per character frame
                 ;; (sleep 0.005)
                 )))
           (sleep 3))

      ;; Robust cleanup block safely executes regardless of loop interrupts
      (unless (cffi:null-pointer-p nc-handle)
        (%notcurses-stop nc-handle)))))

(defun main ()
  "Multi-environment script runner supporting both SLY/Slynk and SLIME/Swank."
  ;; 1. Execute your core example logic
  (start03)

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
