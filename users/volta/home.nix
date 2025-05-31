{ pkgs, ... }:

/*
  home-manager configuration
  Useful links:
  - Home Manager Manual: https://nix-community.gitlab.io/home-manager/
  - Appendix A. Configuration Options: https://nix-community.gitlab.io/home-manager/options.html
*/
{
  home = {
    packages = __attrValues {
      inherit (pkgs)
        krabby
        pfetch
        temurin-jre-bin-17
        daemonize
        sqlite
        ;
    };
  };

  programs = {
    nix-index.enable = false;
    nix-index-database.comma.enable = false;
  };
}
