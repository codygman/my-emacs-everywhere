#!/bin/bash

EMACSFOR="PERSONAL" emacs -nw --load load-init-then-run-ert.el
cat test-results.txt
