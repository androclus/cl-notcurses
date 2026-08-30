#!/bin/bash
sbcl --noinform \
     --load ~/quicklisp/setup.lisp \
     --eval "(push \"$(pwd)/\" asdf:*central-registry*)" \
     --eval "(ql:quickload :cl-notcurses :silent t)" \
     --load examples/00-hello-world.lisp \
     --eval "(cl-notcurses::start00)" \
     --quit
