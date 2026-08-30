#!/bin/bash
sbcl --noinform \
     --load ~/quicklisp/setup.lisp \
     --eval "(push \"$(pwd)/\" asdf:*central-registry*)" \
     --eval "(ql:quickload :cl-notcurses :silent t)" \
     --load examples/01-hello-world-cli.lisp \
     --eval "(cl-notcurses::start01)" \
     --quit
