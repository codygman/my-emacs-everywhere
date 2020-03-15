{ config, pkgs, home, ... }:

with import <nixpkgs> {};
with lib;

let
  emacsHEAD = import ./emacs.nix;
  doom-emacs = pkgs.callPackage (builtins.fetchTarball {
    url = https://github.com/vlaci/nix-doom-emacs/archive/master.tar.gz;
  }) {
    doomPrivateDir = ./doom.d;  # Directory containing your config.el init.el
    # and packages.el files
  };
in
{
  # copy paste from irc directly
  nixpkgs.overlays = [(self: super: {
    emacs = emacsHEAD;
    emacsWithPackages = (pkgs.emacsPackagesNgGen emacsHEAD).emacsWithPackages;
  } )];

  nixpkgs.config = {
    allowUnfree = true;
    allowBroken = true;
  };

  programs = {
    home-manager.enable = true;
    home.file.".emacs.d/init.el".text = ''
     (load "default.el")
    '';
    emacs = {
      enable = true;
      package = emacsHEAD;
    };
    git = {
      enable = true;
      userName = "codygman";
      userEmail = "codygman.consulting@gmail.com";
    };
  };

  home = {
    packages = with pkgs; [ doom-emacs ];
    file = {
      ".bashrc" = {
        text = ''
          eval "$(direnv hook bash)"
          nixify() {
            if [ ! -e ./.envrc ]; then
              echo "use nix" > .envrc
              direnv allow
            fi
            if [ ! -e default.nix ]; then
              cat > default.nix <<'EOF'
          with import <nixpkgs> {};
          stdenv.mkDerivation {
            name = "env";
            buildInputs = [
              bashInteractive
            ];
          }
          EOF
              ${EDITOR:-vim} default.nix
            fi
          }
        '';
      };
      ".zshrc" = {
        text = ''
          eval "$(direnv hook zsh)"
          nixify() {
            if [ ! -e ./.envrc ]; then
              echo "use nix" > .envrc
              direnv allow
            fi
            if [ ! -e default.nix ]; then
              cat > default.nix <<'EOF'
          with import <nixpkgs> {};
          stdenv.mkDerivation {
            name = "env";
            buildInputs = [
              bashInteractive
            ];
          }
          EOF
              ${EDITOR:-emacs} default.nix
            fi
          }
        '';
      };
      ".direnvrc" = {
        text = ''
        use nix
        layout_haskell() {
          PATH_add ~/.cabal/bin
          [ -d .cabal-sandbox ] || cabal sandbox init
          PATH_add .cabal-sandbox/bin
          export GHC_PACKAGE_PATH=$(cabal exec -- sh -c "echo \$GHC_PACKAGE_PATH")
        }
      '';
      };
    };
  };

}
