{ config, pkgs, home, ... }:

with import <nixpkgs> {};
with lib;

let
  emacsHEAD = import ./emacs.nix;
in
{

  programs = {
    # Let Home Manager install and manage itself.
    home-manager.enable = true;
    emacs = {
      enable = true;
      package = emacsHEAD;
    };
  };

  home = {
    packages = with pkgs; [
      ripgrep
      ghc
      gnumake
      fd
    ];
  };

}
