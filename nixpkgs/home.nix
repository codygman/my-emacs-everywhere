{ config, pkgs, home, ... }:

with import <nixpkgs> {};
with lib;

let
  myEnv = builtins.getEnv "MYENV";
  pkgs = import <nixpkgs> { overlays = [ (import emacs-overlay) ]; };
  emacs-overlay = builtins.fetchTarball "https://github.com/nix-community/emacs-overlay/archive/52b9fd468cd45f85c43f9b623ed2854971d8e1ad.tar.gz";
  doomemacs = import ./doomemacs.nix;
in
{
  imports = if myEnv != ""
            then if myEnv == "personal" then
              lib.info "loading PERSONAL home manager environment"
                [ ~/Sync/nix-home-manager-config/personal.nix ]
                 else
                   if myEnv == "work" then
                     lib.info "loading WORK home manager environment"
                       [ ~/Sync/nix-home-manager-config/work.nix ]
                   else
                     lib.warn "MYENV is not one of 'personal' or 'work', ONLY core home environment will be available!" []
            else
              lib.warn "MYENV not specified, ONLY core home environment will be available!" [];

  programs = {
    home-manager.enable = true;
    git = {
      enable = true;
      userName = "codygman";
      userEmail = lib.mkDefault "codygman.consulting@gmail.com";
    };
    bash = {
      enable = true;
      shellAliases = {
        new-haskell-project = "nix-shell -p cookiecutter git --run 'cookiecutter gh:utdemir/hs-nix-template'";
      };
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
