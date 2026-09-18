(in-package :cl-notcurses)

(defun start04 ()
  "Safe non-blocking event loop that cleans up streams for SLYNK."
  ;; Flush any pending SLYNK output before Notcurses captures the TTY
  (finish-output)
  (force-output)

  (let ((nc (%notcurses-core-init (cffi:null-pointer) (cffi:null-pointer))))
    (when (cffi:null-pointer-p nc)
      (error "Failed to initialize Notcurses!"))

    (unwind-protect
         (let ((stdplane (%notcurses-stdplane nc)))
           (%ncplane-putstr-yx stdplane 2 2 "Press 'q' to exit this loop!")
           (%notcurses-render nc)

           (cffi:with-foreign-object (ni '(:struct ncinput))
             ;; Clear memory to avoid garbage stack data
             (cffi:foreign-funcall "memset" :pointer ni :int 0 :size (cffi:foreign-type-size '(:struct ncinput)) :pointer)

             (loop
               (let ((key (%notcurses-get-nblock nc ni)))
                 ;; 113 = ASCII 'q'
                 (when (= key (char-code #\q))
                   (return))

                 (sleep 0.01)))))

      ;; ALWAYS ensure stop runs, then immediately flush streams to restore SLYNK
      (ignore-errors
        (%notcurses-stop nc))
      (finish-output)
      (force-output))))

(defun main ()
  "Multi-environment script runner supporting both SLY/Slynk and SLIME/Swank."
  ;; 1. Execute your core example logic
  (start04)

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
