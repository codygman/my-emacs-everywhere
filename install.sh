nix-channel --add https://github.com/rycee/home-manager/archive/master.tar.gz home-manager
nix-channel --update
export NIX_PATH=$HOME/.nix-defexpr/channels${NIX_PATH:+:}$NIX_PATH
nix-env -iA cachix -f https://cachix.org/api/v1/install

# see if the cached build that succeeded is actually used
if [ $TRAVIS_OS_NAME = 'osx' ]; then
    echo "trusted-users = root $USER" | sudo tee -a /etc/nix/nix.conf && sudo pkill nix-daemon
fi

nix-shell '<home-manager>' -A install
