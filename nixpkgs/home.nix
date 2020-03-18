{ config, pkgs, home, ... }:

with import <nixpkgs> {};
with lib;

let
  emacs = import ./emacs.nix;
  doom-emacs = pkgs.callPackage (builtins.fetchTarball {
    url = https://github.com/vlaci/nix-doom-emacs/archive/master.tar.gz;
  }) {
    doomPrivateDir = ./doom.d;
  };
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
