{ config, pkgs, home, ... }:

with import <nixpkgs> {};
with lib;

let
  emacsHEAD = import ./emacs.nix;
  doom-emacs = pkgs.callPackage (builtins.fetchTarball {
    url = https://github.com/vlaci/nix-doom-emacs/archive/develop.tar.gz;
  }) {
    dependencyOverrides = {
      emacs = emacsHEAD;
    };
    # TODO right now I just create this empty directory with config.el init.el and packages.el files but need to do something better
    doomPrivateDir = ./doom.d;
  };
in
{
  programs = {
    home-manager.enable = true;
    emacs = {
      enable = true;
      package = emacsHEAD;
    };
  };

  home = {
    packages = with pkgs; [ doom-emacs ];
    file = {
    ".emacs.d/init.el".text = ''
     (load "default.el")
    '';
      };
  };

}
