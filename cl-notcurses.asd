(asdf:defsystem #:cl-notcurses
  :description "Minimal set of notcurses CFFI wrappers."
  :author "Your Name <your.email@example.com>"
  :license "MIT"
  :depends-on (#:cffi)
  :pathname "src"
  :components ((:file "package")
               (:file "cl-notcurses")))
