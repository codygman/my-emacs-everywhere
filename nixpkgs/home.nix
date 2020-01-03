{ config, pkgs, home, ... }:

with import <nixpkgs> {};
with lib;

let
  emacsHEAD = import ./emacs.nix;
  all-hies = import (fetchTarball "https://github.com/infinisil/all-hies/tarball/master") {};
  unstableTarball = fetchTarball https://github.com/NixOS/nixpkgs-channels/archive/nixos-unstable.tar.gz;
in
{
  # copy paste from irc directly
  nixpkgs.overlays = [(self: super: {
    unstable = import unstableTarball { config = config.nixpkgs.config; };
    haskellPackages = self.unstable.haskell.packages.ghc881;
  } )];

  nixpkgs.config = {
    allowUnfree = true;
    allowBroken = true;
  };

  programs = {
    # Let Home Manager install and manage itself.
    home-manager.enable = true;
    # todo figure out if this can work from inside home.nix
    # home-manager.users.cody.extraGroups = ["adbusers"];
    msmtp.enable = true;
    mbsync.enable = true;
    notmuch.enable = true;
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

    jq = mkIf (builtins.getEnv "TRAVIS_OS_NAME" == "") {enable = true;};
    vim = mkIf (builtins.getEnv "TRAVIS_OS_NAME" == "" && stdenv.isLinux) {enable = true;};
    # mu = mkIf (builtins.getEnv "TRAVIS_OS_NAME" == ""  && stdenv.isLinux) {
    #   enable = true;
    # };
    git = {
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
      direnv
      pinentry
      ripgrep
      fd
      unstable.haskell.compiler.ghc881
    ] ++ (if builtins.getEnv "TRAVIS_OS_NAME" == "" then [
      bitwarden
      bitwarden-cli
      gnumake
      mu
      haskellPackages.lens
      # unstable.haskell.packages.ghc881.lens
      # unstable.haskellPackages.lens
      # haskellPackages.pandoc
      # haskellPackages.ghcid
      # haskellPackages.hlint
      # haskellPackages.brittany
      # haskellPackages.hpack
      python
      pkgs.python36Packages.virtualenv
      pwgen
      # (all-hies.selection { selector = p: { inherit (p) ghc865; }; })
      # shellcheck
      signal-desktop
      source-code-pro
      unstable.cabal2nix
    ] else []) ++ (if (builtins.getEnv "TRAVIS_OS_NAME" == "" && stdenv.isLinux) then [
      feh
      dmenu
    ] else []);
  };

  systemd.user.startServices = if stdenv.isLinux then true else false;

  home = {

    keyboard = mkIf stdenv.isLinux {
      # TODO test to see if this works on osx too
      layout = "us";
      options = [
        "ctrl:nocaps"
      ];
    };

    sessionVariables = {
      EDITOR = "emacsclient --create-frame --alternate-editor emacs";
    };
    file = {
      ".direnvrc" = {
        text = ''
        layout_haskell() {
          PATH_add ~/.cabal/bin
          [ -d .cabal-sandbox ] || cabal sandbox init
          PATH_add .cabal-sandbox/bin
          export GHC_PACKAGE_PATH=$(cabal exec -- sh -c "echo \$GHC_PACKAGE_PATH")
        }
        use_nix() {
          local path="$(nix-instantiate --find-file nixpkgs)"
        
          if [ -f "${path}/.version-suffix" ]; then
            local version="$(< $path/.version-suffix)"
          elif [ -f "${path}/.git" ]; then
            local version="$(< $(< ${path}/.git/HEAD))"
          fi
        
          local cache=".direnv/cache-${version:-unknown}"
        
          local update_drv=0
          if [[ ! -e "$cache" ]] || \
            [[ "$HOME/.direnvrc" -nt "$cache" ]] || \
            [[ .envrc -nt "$cache" ]] || \
            [[ default.nix -nt "$cache" ]] || \
            [[ shell.nix -nt "$cache" ]];
          then
            [ -d .direnv ] || mkdir .direnv
            nix-shell --show-trace --pure "$@" --run "\"$direnv\" dump bash" > "$cache"
            update_drv=1
          else
            log_status using cached derivation
          fi
          local term_backup=$TERM path_backup=$PATH
          if [ -n ${TMPDIR+x} ]; then
            local tmp_backup=$TMPDIR
          fi
        
          eval "$(< $cache)"
          export PATH=$PATH:$path_backup TERM=$term_backup TMPDIR=$tmp_backup
          if [ -n ${tmp_backup+x} ]; then
            export TMPDIR=${tmp_backup}
          else
            unset TMPDIR
          fi
        
          # `nix-shell --pure` sets invalid ssl certificate paths
          if [ "${SSL_CERT_FILE:-}" = /no-cert-file.crt ]; then
            unset SSL_CERT_FILE
          fi
          if [ "${NIX_SSL_CERT_FILE:-}" = /no-cert-file.crt ]; then
            unset NIX_SSL_CERT_FILE
          fi
        
          # This part is based on https://discourse.nixos.org/t/what-is-the-best-dev-workflow-around-nix-shell/418/4
          if [ "$out" ] && (( $update_drv )); then
            local drv_link=".direnv/drv"
            local drv="$(nix show-derivation $out | grep -E -o -m1 '/nix/store/.*.drv')"
            local stripped_pwd=${PWD/\//}
            local escaped_pwd=${stripped_pwd//-/--}
            local escaped_pwd=${escaped_pwd//\//-}
            ln -fs "$drv" "$drv_link"
            ln -fs "$PWD/$drv_link" "/nix/var/nix/gcroots/per-user/$LOGNAME/$escaped_pwd"
            log_status renewed cache and derivation link
          fi
        
          if [[ $# = 0 ]]; then
            watch_file default.nix
            watch_file shell.nix
          fi
        }
      '';
      };
    };
  };

  accounts.email.accounts = {
    "cody@codygman.dev" = {
      mbsync.enable = true;
      primary = true;
      address = "cody@codygman.dev";
      userName = "codygman";
      realName = "Cody Goodman";
      passwordCommand = "${pkgs.coreutils}/bin/cat /home/cody/deleteme";
      # passwordCommand = "gpg --use-agent --quiet --batch -d /home/makefu/.gnupg/mail/syntax-fehler.gpg";
      msmtp.enable = true;
      notmuch.enable = true;

      imap = {
        host = "imap.mailfence.com";
        port = 993;
        tls.enable = true;
      };

      smtp = {
        host = "smtp.mailfence.com";
        port = 465;
        tls = {
          enable = true;
        };
        # useStartTls = true;
      };
    };
    "codygman.consulting@gmail.com" = {
      address = "codygman.consulting@gmail.com";
      userName = "codygman.consulting@gmail.com";
      realName = "Cody Goodman";
    };
  };

  services = {
    mbsync = {
      enable = true;
      frequency = "*:0/2"; # update every 1 minute
      # postExec = "/home/cody/notmuch-tag.sh";
      # postExec = "${config.xdg.configHome}/mbsync/postExec";
      # TODO fix this to use xdg stuff above
      postExec = "/home/cody/.config/mbsync/postExec";
    };
    syncthing = mkIf (builtins.getEnv "TRAVIS_OS_NAME" == "" && stdenv.isLinux) {
      enable = true;
    };
    gpg-agent = mkIf (builtins.getEnv "TRAVIS_OS_NAME" == ""  && stdenv.isLinux) {
      enable = true;
      defaultCacheTtl = 600;
      enableSshSupport = true;
      extraConfig = ''
          allow-emacs-pinentry
          allow-loopback-pinentry
          pinentry-program ${pkgs.pinentry}/bin/pinentry
      '';
    };
    # location.provider = "geoclue2";
    redshift = mkIf (builtins.getEnv "TRAVIS_OS_NAME" == ""  && stdenv.isLinux) {
      enable = true;
      temperature = {
        day = 6500;
        night = 3500;
      };
      brightness = {
        day = "1";
        night = "0.45";
      };
      # Dallas: 32.7763, -96.7969
      latitude = "33.7763";
      longitude = "-96.7969";
    };
  };

  


}
