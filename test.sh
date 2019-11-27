nix-channel --add https://github.com/rycee/home-manager/archive/master.tar.gz home-manager
nix-channel --update
export NIX_PATH=$HOME/.nix-defexpr/channels${NIX_PATH:+:}$NIX_PATH
nix-env -iA cachix -f https://cachix.org/api/v1/install

nix-shell '<home-manager>' -A install
cachix use codygman4 # did cachix work?

mkdir -p ~/.config/
cp -vR nixpkgs ~/.config

# try pushing emacs.nix to cachix
pushd ~/.config/nixpkgs
nix-build emacs.nix | cachix push codygman4
popd
# see if the cached build that succeeded is actually used
# home-manager switch

# emacs -Q -batch --load load-init-then-run-ert.el
