#!/bin/bash

rm ./**/*.elc
rm *.elc

for file in ./*.el; do
    echo 'Compiling '"$file"' ...'
    cask emacs -batch -L . --eval '(progn '"(require 'comp)"' (setq byte-compile-error-on-warn t native-compile-target-directory (car native-comp-eln-load-path))  (nreverse native-comp-eln-load-path))' -f batch-byte+native-compile "$file" || break
done

for file in ./**/*.el; do
    echo 'Compiling '"$file"' ...'
    cask emacs -batch -L . --eval '(progn '"(require 'comp)"' (setq byte-compile-error-on-warn t native-compile-target-directory (car native-comp-eln-load-path))  (nreverse native-comp-eln-load-path))' -f batch-byte+native-compile "$file" || break
done


if [[ -z $NO_REMOVE_ELC ]]; then
    rm ./**/*.elc
    rm *.elc
fi
