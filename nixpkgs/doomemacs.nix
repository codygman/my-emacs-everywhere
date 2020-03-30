let
  myemacs = import ./emacs.nix;
  pkgs = import <nixpkgs> { };
  nix-doom-emacs = builtins.fetchTarball "https://github.com/vlaci/nix-doom-emacs/archive/master.tar.gz";
in
  with pkgs; callPackage nix-doom-emacs {
    bundledPackages = false;
    emacsPackages = emacsPackagesFor myemacs;
    doomPrivateDir = ./doom.d;
  }

