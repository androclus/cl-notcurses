(asdf:defsystem "cl-notcurses"
  :name "cl-notcurses"
  :version (:read-file-form "version.lisp-expr")
  :author "Jeff Stern <jasprog@posteo.net>"
  :maintainer "Jeff Stern <jasprog@posteo.net>"
  :description "CFFI bindings for Nick Black's notcurses."
  :long-description "More wittiness"
  :license "MIT License (See COPYING)"
  :depends-on ("cffi" "cl-setlocale")
  :pathname "src/"
  :serial t
  :components ((:file "package")
               (:file "cl-notcurses")))
