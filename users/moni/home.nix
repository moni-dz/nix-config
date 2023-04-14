{ config, inputs, lib, pkgs, system, ... }:

/*
  home-manager configuration
  Useful links:
  - Home Manager Manual: https://rycee.gitlab.io/home-manager/
  - Appendix A. Configuration Options: https://rycee.gitlab.io/home-manager/options.html
*/
{
  imports = [
    # Shared configuration across all users
    ../shared/home.nix

    inputs.doom.hmModule
  ];

  home = {
    packages = lib.attrValues {
      inherit (pkgs)
        coreutils-prefixed
        fd
        gnugrep
        shellcheck
        ripgrep
        discord
        pfetch
        pandoc
        vscode
        wezterm;

      #inherit (inputs.nixpkgs-f2k.packages.${system}) emacs-plus-git;
      inherit (pkgs.texlive.combined) scheme-medium;
      
      #discord-openasar = pkgs.discord.override { withOpenASAR = true; };
      pythonEnv = pkgs.python310.withPackages (ps: [ ps.jupyter ]);
    };

    sessionVariables.EDITOR = "emacs -nw";
    stateVersion = "22.11";
  };

  programs = {
    zathura = { enable = false; };

    zsh.initExtra = ''
      export PATH=$PATH:/opt/homebrew/bin:~/.config/emacs/bin
    '';
  };
}
