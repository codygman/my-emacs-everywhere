{ pkgs, sources ? import ./sources.nix, ...}:
with
  { overlay = _: pkgs:
      { niv = import sources.niv {};    # use the sources :)
      };
  };
let
  sources = import ./nix/sources.nix;
  myEnv = builtins.getEnv "MYENV";
  pkgs = import sources.nixpkgs { overlays = [ overlay ] ; config = {};   };
  emacs-overlay = builtins.fetchTarball "https://github.com/nix-community/emacs-overlay/archive/52b9fd468cd45f85c43f9b623ed2854971d8e1ad.tar.gz";
  doomemacs = import ./doomemacs.nix;
in
{
  imports = if myEnv != ""
            then if myEnv == "personal" then
              pkgs.lib.info "loading PERSONAL home manager environment"
                [ ~/Sync/nix-home-manager-config/personal.nix ]
                 else
                   if myEnv == "work" then
                     pkgs.lib.info "loading WORK home manager environment"
                       [ ~/Sync/nix-home-manager-config/work.nix ]
                   else
                     pkgs.lib.warn "MYENV is not one of 'personal' or 'work', ONLY core home environment will be available!" []
            else
              pkgs.lib.warn "MYENV not specified, ONLY core home environment will be available!" [];

  programs = {
    home-manager.enable = true;
    git = {
      enable = true;
      userName = "codygman";
      userEmail = pkgs.lib.mkDefault "codygman.consulting@gmail.com";
    };
    direnv = {
      enable = true;
      enableZshIntegration = true;
      enableBashIntegration = true;
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

  services = {
    lorri.enable = true;
  };

}
