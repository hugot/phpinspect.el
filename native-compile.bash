#!/bin/bash

for file in ./*.el; do
    echo 'Compiling '"$file"' ...'
    cask emacs -batch -L . --eval '(setq native-compile-error-on-warn t)' -f batch-native-compile "$file" || break
done

for file in ./**/*.el; do
    echo 'Compiling '"$file"' ...'
    cask emacs -batch -L . --eval '(setq byte-compile-error-on-warn t)' -f batch-native-compile "$file" || break
done
