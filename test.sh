nix-channel --add https://github.com/rycee/home-manager/archive/master.tar.gz home-manager
nix-channel --update
export NIX_PATH=$HOME/.nix-defexpr/channels${NIX_PATH:+:}$NIX_PATH
nix-shell '<home-manager>' -A install

mkdir -p ~/.config/
cp -vR nixpkgs ~/.config
home-manager switch

emacs -Q -batch --load load-init-then-run-ert.el
