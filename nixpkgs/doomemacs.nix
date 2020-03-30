let
  emacs-overlay = builtins.fetchTarball "https://github.com/nix-community/emacs-overlay/archive/52b9fd468cd45f85c43f9b623ed2854971d8e1ad.tar.gz";
  pkgs = import <nixpkgs> { overlays = [ (import emacs-overlay) ]; };
  nix-doom-emacs = builtins.fetchTarball "https://github.com/vlaci/nix-doom-emacs/archive/master.tar.gz";
in
  with pkgs; callPackage nix-doom-emacs {
    extraPackages = epkgs: [pkgs.emacs-all-the-icons-fonts];
    bundledPackages = false;
    emacsPackages = emacsPackagesFor emacsGit;
    doomPrivateDir = ./doom.d;
    extraConfig = ''
(setq-default evil-escape-key-sequence "jf")
(setq display-line-numbers-type nil)
(global-auto-revert-mode t)
    '';
  }

