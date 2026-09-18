#!/bin/bash
sbcl --noinform \
     --load ~/quicklisp/setup.lisp \
     --eval "(push \"$(pwd)/\" asdf:*central-registry*)" \
     --eval "(ql:quickload :cl-notcurses :silent t)" \
     --load examples/04-input-key.lisp \
     --eval "(cl-notcurses::start04)" \
     --quit
