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
    ssh = mkIf stdenv.isLinux {
      enable = true;
      controlPath = "~/.ssh/master-%C";
    };
    htop.enable = true;
    firefox = mkIf stdenv.isLinux {
      # todo install with home-manager
      enable = true;
    };
    jq.enable = true;
    vim.enable = true;
    git = {
      enable = true;
      userName = "codygman";
      userEmail = "codygman.consulting@gmail.com";
    };
    gpg = {
      enable = true;
    };
  };

  home = {
    packages = with pkgs; [
      dmenu
      feh
      ripgrep
      gnumake
      fd
      ghc
      haskellPackages.lens
      haskellPackages.pandoc
      haskellPackages.ghcid
      haskellPackages.hlint
      stack
      cabal2nix
      haskellPackages.brittany
      haskellPackages.hpack
      haskellPackages.cabal-install
      source-code-pro
    ];
  };



  systemd.user.startServices = if stdenv.isLinux then true else false;

  home.keyboard = mkIf stdenv.isLinux {
    # TODO test to see if this works on osx too
    layout = "us";
    options = [
      "ctrl:nocaps"
    ];
  };

  home.sessionVariables = {
    EDITOR = "emacsclient --create-frame --alternate-editor emacs";
  };

  accounts.email.accounts = {
    "codygman.consulting@gmail.com" = {
      primary = true;
      address = "codygman.consulting@gmail.com";
      userName = "codygman.consulting@gmail.com";
      realName = "Cody Goodman";
    };
  };

  services = {
    syncthing = mkIf stdenv.isLinux {
      enable = true;
    };
    gpg-agent = {
      enable = true;
      defaultCacheTtl = 600;
      enableSshSupport = true;
    };
  };

  


}
