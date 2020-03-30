{ config, pkgs, home, ... }:

with import <nixpkgs> {};
with lib;

let
  pkgs = import <nixpkgs> { overlays = [ (import emacs-overlay) ]; };
  doomemacs = import ./doomemacs.nix;
in
{

  programs = {
    home-manager.enable = true;
    git = {
      enable = true;
      userName = "codygman";
      userEmail = "codygman.consulting@gmail.com";
    };
  };

  home = {
    packages = with pkgs; [ doomemacs ];
    file = {
    ".emacs.d/init.el".text = ''
     (load "default.el")
    '';
      };
  };

}
