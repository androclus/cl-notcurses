(in-package :cl-notcurses)

(defun start02 ()
  (declare (optimize (speed 3) (safety 1)))
  (let ((nc-handle (cffi:null-pointer)))
    (unwind-protect
         (progn
           ;; 1. Initialize via the zero-allocation pathway
           (setf nc-handle (%notcurses-core-init (cffi:null-pointer) (cffi:null-pointer)))
           (when (cffi:null-pointer-p nc-handle)
             (error "Failed to initialize Notcurses engine."))

           (let ((nc-stdplane (%notcurses-stdplane nc-handle)))
             (%ncplane-set-scrolling nc-stdplane 1)

             ;; 2. STACK ALLOCATION FOR DIMENSIONS (Replaces foreign-alloc)
             ;; Allocates integers safely on the stack; automatically cleans up when leaving block.
             (cffi:with-foreign-objects ((dimy-ptr :uint) (dimx-ptr :uint))
               (%ncplane-dim-yx nc-stdplane dimy-ptr dimx-ptr)

               ;; 3. AUTOMATIC STRING CONVERSION
               ;; cffi:foreign-string-to-lisp instantly copies the C memory
               ;; into a safe Lisp string, completely avoiding leaks.
               (let ((os-ver (cffi:foreign-string-to-lisp (%notcurses-osversion)))
                     (term-name (cffi:foreign-string-to-lisp (%notcurses-detected-terminal nc-handle)))
                     (rows (cffi:mem-aref dimy-ptr :uint))
                     (cols (cffi:mem-aref dimx-ptr :uint)))

                 ;; Print the environmental attributes
                 (%ncplane-putstr-yx nc-stdplane -1 -1
                                    (format nil "Operating System: ~A~%Terminal: ~A~%Dimensions: ~D rows, ~D columns~%"
                                            os-ver term-name rows cols))))

             ;; 4. DIRECT STRUCTURE INTERACTION
             ;; Fetch the raw capability layout pointer directly from the engine handle context
             (let ((caps-ptr (%notcurses-capabilities nc-handle)))
               (unless (cffi:null-pointer-p caps-ptr)
                 (cffi:with-foreign-slots ((utf8 halfblocks quadrants sextants braille rgb colors)
                                           caps-ptr (:struct nccapabilities))
                   ;; Build the interactive capabilities table out of native values
                   (%ncplane-putstr-yx nc-stdplane -1 -1
                                      (format nil "utf8: ~A~%halfblocks: ~A~%quadrants: ~A~%sextants: ~A~%braille: ~A~%pixel: ~A~%24bit-color: ~A~%palette colors: ~D~%"
                                              (if utf8 "true" "false")
                                              (if halfblocks "true" "false")
                                              (if quadrants "true" "false")
                                              (if sextants "true" "false")
                                              (if braille "true" "false")
                                              (if (%notcurses-canpixel nc-handle) "true" "false")
                                              (if rgb "true" "false")
                                              colors))))))

             ;; Render layout out to the display canvas
             (%notcurses-render nc-handle)
             (sleep 3))

      ;; The cleanup block remains incredibly thin and bulletproof!
      (unless (cffi:null-pointer-p nc-handle)
        (%notcurses-stop nc-handle)))))

(defun main ()
  "Multi-environment script runner supporting both SLY/Slynk and SLIME/Swank."
  ;; 1. Execute your core example logic
  (start02)

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
