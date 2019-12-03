#!/bin/bash

cp -Rv ~/build/codygman/my-emacs-everywhere/snippets ~/.emacs.d/snippets
if [ "$TRAVIS_OS_NAME" = "osx" ]; then
    EMACSFOR="WORK" emacs -nw --load load-init-then-run-ert.el
elif [ "$TRAVIS_OS_NAME" = "linux" ]; then
    EMACSFOR="PERSONAL" emacs -nw --load load-init-then-run-ert.el
fi
