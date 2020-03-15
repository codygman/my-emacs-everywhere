{ config, pkgs, home, ... }:

with import <nixpkgs> {};
with lib;

let
  emacsHEAD = import ./emacs.nix;
in
{
  programs = {
    home-manager.enable = true;
    emacs = {
      enable = true;
      package = emacsHEAD;
    };
  };
}
