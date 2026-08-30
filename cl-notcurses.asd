(asdf:defsystem #:cl-notcurses
  :description "Minimal set of notcurses CFFI wrappers."
  :author "Jeff Stern <jasprog@posteo.net>"
  :license "MIT"
  :depends-on (#:cffi)
  :pathname "src"
  :components ((:file "package")
               (:file "cl-notcurses")))
