(in-package :cl-notcurses)

;; 1. Model — plain, immutable CL data. No structs holding pointers, no mutation.
;; For a first pass this can be as small as:

(defstruct (app-model (:constructor make-app-model))
  (message "Hello, notcurses!" :type string)
  (quit? nil :type boolean))

;; defstruct gives you immutability "by convention" (nobody calls the setters),
;; which is fine to start; later, once you're comfortable, you can look at
;; making update always return a new struct via copy-app-model + :constructor
;; overrides, so it's immutable in practice, not just by politeness.
;;
;; 2. Msg — a closed, enumerable set of events your pure update function can
;; react to. Don't leak raw ncinput structs into your pure core — translate them
;; at the boundary (exactly the seam I flagged a few messages back):

(deftype msg () '(member :key-q :key-other :resize :tick))

(defun ncinput->msg (ni-plist)
  (case (getf ni-plist :id)
    (113 :key-q)   ; #\q
    (t :key-other)))

;; 3. update — a pure function, model, msg -> new-model. No I/O, no notcurses
;; calls, completely testable at the REPL with no terminal involved:

(defun update (model msg)
  (case msg
    (:key-q (copy-app-model model :quit? t))
    (t model)))

;; The fact that you can call (update (make-app-model) :key-q) at a plain SBCL
;; REPL right now, with no notcurses loaded at all, and get back a correct new
;; model — that's the whole point of Functional Core / Imperative Shell, and
;; it's worth actually doing as a sanity check before moving on.

;; 4. view — pure function, model -> render-tree (data describing what should be
;; drawn, not drawing it). Keep this a plain data structure — a list of plists,
;; or a small set of defstructs like (text :y :x :string), (box :y :x :h :w) —
;; rather than calling ncplane-putstr-yx directly. That's the piece that lets a
;; "dialog box" widget or "thermometer" widget later be nothing more than a
;; function that returns a bigger/nested version of this same data shape.
;;
;; Then the imperative shell is the thin, boring bit that ties it together and
;; is the only code allowed to touch the FFI:

(defun run-app (nc stdplane initial-model)
  (with-foreign-object (ni '(:struct ncinput))
    (loop with model = initial-model
          until (app-model-quit? model)
          do (render-tree->notcurses! stdplane (view model))
             (%notcurses-render nc)
             (let ((id (%notcurses-get-nblock nc ni)))
               (setf model (update model (ncinput->msg (ncinput->plist ni))))))))

;; Once that skeleton runs end-to-end (even though right now view only ever
;; renders one string), then growing it is just:
;;
;; - Add cases to your msg type and update as you add real interactivity.
;;
;; - Add new render-tree node types (box, progress-bar, checkbox) and teach
;;   render-tree->notcurses! how to draw each one — this is where a thermometer
;;   widget becomes "a pure function model -> (progress-bar :y 3 :x 5 :width 20
;;   :pct 0.6)" plus one new case clause in the drawer.
;;
;; - Widgets compose naturally as functions that return lists of nodes, e.g.
;;   (defun dialog-box (title body) (list (box ...) (text ...) (text ...))) — no
;;   widget object needs to know about notcurses at all.


;; On the Lisp-learning side, two things are worth deliberately studying as you
;; do this, because they're exactly the tools this architecture wants:

;; - CLOS and generic functions, once you have 3-4 render-tree node types — a
;;   defgeneric draw-node (node stdplane) with an defmethod per node type is
;;   nicer than one big case once it grows, and it's the natural place to learn
;;   CLOS with a real, motivating problem instead of a toy example.
;;
;; - The condition system (handler-case, restart-case) — right now a bad FFI
;;   call segfaults the whole process, which is unrecoverable by definition. But
;;   logic errors in your own imperative shell (bad array index into a render
;;   tree, etc.) should be catchable, and getting comfortable with conditions
;;   now will pay off as the shell gets more complex.
;;
;; If you want, once you've got this skeleton compiling, paste it back to me and
;; I can look at whether the render-tree shape you chose will scale cleanly to
;; something like a modal dialog box (which needs to temporarily "own" the whole
;; screen and its own sub-loop) — that's the first real design fork you'll hit
;; once you go past single-widget examples.
