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
        helix
        wezterm;

      # discord-openasar = pkgs.discord-ptb.override { withOpenASAR = true; };
      # inherit (inputs.nixpkgs-f2k.packages.${system}) emacs-plus-git;
    };

    sessionVariables.EDITOR = "hx";
    stateVersion = "22.11";
  };

  programs = {
    discocss = {
      enable = true;
      discordAlias = false;
      css = builtins.readFile ./config/rose-pine.theme.css;
    };
  
    zsh.initExtra = ''
      export PATH=$PATH:/Users/moni/Library/Python/3.11/bin
    '';
  };
}
