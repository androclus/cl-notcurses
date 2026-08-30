#!/bin/bash
sbcl --noinform \
     --load ~/quicklisp/setup.lisp \
     --eval "(push \"$(pwd)/\" asdf:*central-registry*)" \
     --eval "(ql:quickload :cl-notcurses :silent t)" \
     --load examples/03-asterisks.lisp \
     --eval "(cl-notcurses::start03)" \
     --quit
