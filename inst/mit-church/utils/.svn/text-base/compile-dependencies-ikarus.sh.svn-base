#!/bin/bash

# NOTE: you'll probably have to change these...

export IKARUS_PATH=/usr/local/ikarus/bin/ikarus
export TMP_FILE=/tmp/ikarus-compile-script.ss

cat > $TMP_FILE<<EOF
#!r6rs

(import
  (church church)
  (church readable-scheme)
  (church external math-env))

(church
  (load "xrps.church")
  (flip)
)

EOF

$IKARUS_PATH -O2 --compile-dependencies $TMP_FILE

rm -f $TMP_FILE
