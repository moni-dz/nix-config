{ config, inputs, lib, pkgs, system, ... }:

/*
  home-manager configuration
  Useful links:
  - Home Manager Manual: https://nix-community.gitlab.io/home-manager/
  - Appendix A. Configuration Options: https://nix-community.gitlab.io/home-manager/options.html
*/
{
  home = {
    packages = lib.attrValues {
      inherit (pkgs) ripgrep pfetch;
    };

    sessionVariables.EDITOR = "nvim";
  };
}
