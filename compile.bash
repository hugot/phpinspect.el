#!/bin/bash

for file in ./*.el; do
    cask emacs -batch -L . --eval '(setq byte-compile-error-on-warn t)' -f batch-byte-compile "$file" || break
done

if [[ -z $NO_REMOVE_ELC ]]; then
    rm ./*.elc
fi
