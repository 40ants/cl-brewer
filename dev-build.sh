#/bin/bash

# USE THIS FILE TO DEBUG cl-brewer ITSELF
# You can run these binaries by settings SLYNK_PORT=4005
# environment variable.

set -e

if [[ ! -e './bundle-libs/' ]]; then
    qlot bundle
fi

if [[ ! -e './sly/' ]]; then
    git clone https://github.com/joaotavora/sly
fi

export CL_SOURCE_REGISTRY="`pwd`/bundle-libs//:`pwd`/sly/slynk/:`pwd`/"
export ASDF_OUTPUT_TRANSLATIONS='/:/'

rm -f cl-brewer cl-brewer-buildapp cl-brewer-asdf

# This is a version built with buildapp, the same way
# how does cl-brewer formula does, but with compiled in
# Slynk.
buildapp --compress-core \
         --load-system quicklisp-starter \
         --load-system slynk \
         --load-system cl-brewer \
         --output cl-brewer \
         --entry cl-brewer::buildapp-main

mv cl-brewer cl-brewer-buildapp

# This is another version, built with plain ASDF:MAKE
sbcl --eval '(require :asdf)' \
     --eval '(asdf:load-system :quicklisp-starter)' \
     --eval '(asdf:load-system :slynk)' \
     --eval '(handler-bind ((error (lambda (c) (format *error-output* "Condition caught: ~A~%" c) (uiop:print-backtrace :condition c) (uiop:quit 1)))) (asdf:make :cl-brewer))'

mv cl-brewer cl-brewer-asdf
