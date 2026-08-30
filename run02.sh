#!/bin/bash
sbcl --noinform \
     --load ~/quicklisp/setup.lisp \
     --eval "(push \"$(pwd)/\" asdf:*central-registry*)" \
     --eval "(ql:quickload :cl-notcurses :silent t)" \
     --load examples/02-capabilities.lisp \
     --eval "(cl-notcurses::start02)" \
     --quit
