{ config, pkgs, home, ... }:

with import <nixpkgs> {};
with lib;

let
  pkgs = import <nixpkgs> { overlays = [ (import emacs-overlay) ]; };
  emacs-overlay = builtins.fetchTarball "https://github.com/nix-community/emacs-overlay/archive/52b9fd468cd45f85c43f9b623ed2854971d8e1ad.tar.gz";
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
    packages = with pkgs; [ doomemacs fd ripgrep source-code-pro ];
    file = {
    ".emacs.d/init.el".text = ''
     (load "default.el")
    '';
      };
  };

}
