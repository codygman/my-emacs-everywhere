{ config, pkgs, home, ... }:

with import <nixpkgs> {};
with lib;

let
  emacsHEAD = import ./emacs.nix;
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
    packages = with pkgs; [];
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
