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
    ssh = mkIf (builtins.getEnv "TRAVIS_OS_NAME" == "" && stdenv.isLinux) {
      enable = true;
      controlPath = "~/.ssh/master-%C";
    };
    htop = mkIf (builtins.getEnv "TRAVIS_OS_NAME" == "") {enable = true;};
    firefox = mkIf (builtins.getEnv "TRAVIS_OS_NAME" == "" && stdenv.isLinux) {
      # todo install with home-manager
      enable = true;
    };
    jq.enable = true;
    vim = mkIf (builtins.getEnv "TRAVIS_OS_NAME" == "") {enable = true;};
    git = mkIf (builtins.getEnv "TRAVIS_OS_NAME" == "") {
      enable = true;
      userName = "codygman";
      userEmail = "codygman.consulting@gmail.com";
    };
    gpg = mkIf (builtins.getEnv "TRAVIS_OS_NAME" == "") {
      enable = true;
    };
  };

  home = {
    packages = with pkgs; [
      ripgrep
      fd
      ghc
    ] ++ (if builtins.getEnv "TRAVIS_OS_NAME" == "" then [
      feh
      gnumake
      dmenu
      haskellPackages.lens
      haskellPackages.pandoc
      haskellPackages.ghcid
      haskellPackages.hlint
      haskellPackages.brittany
      haskellPackages.hpack
      haskellPackages.cabal-install
      stack
      source-code-pro
      cabal2nix
    ] else []);
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
    syncthing = mkIf (builtins.getEnv "TRAVIS_OS_NAME" == "" && stdenv.isLinux) {
      enable = true;
    };
    gpg-agent = mkIf (builtins.getEnv "TRAVIS_OS_NAME" == "") {
      enable = true;
      defaultCacheTtl = 600;
      enableSshSupport = true;
    };
  };

  


}
