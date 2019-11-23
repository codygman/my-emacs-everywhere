with import <nixpkgs> {}; runCommand "EMACSFOR="PERSONAL" emacs -Q --batch --load load-init-then-test.el" {} "touch $out"
