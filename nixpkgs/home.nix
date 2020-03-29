{ config, pkgs, home, ... }:

with import <nixpkgs> {};
with lib;

let
  pkgs = import <nixpkgs> { overlays = [ (import emacs-overlay) ]; };
  doom-emacs = ./import doomemacs.nix;
in
{

  programs = {
    home-manager.enable = true;
    git = {
      enable = true;
      userName = "codygman";
      userEmail = "codygman.consulting@gmail.com";
    };
    vscode.enable = true;
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
