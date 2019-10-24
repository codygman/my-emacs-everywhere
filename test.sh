nix-channel --add https://github.com/rycee/home-manager/archive/master.tar.gz home-manager
nix-channel --update
export NIX_PATH=$HOME/.nix-defexpr/channels${NIX_PATH:+:}$NIX_PATH
nix-shell '<home-manager>' -A install

emacs -Q -batch -l emacs-config.el -l ert -l my-tests.el -f ert-run-tests-batch-and-exit
