{
  config,
  inputs,
  lib,
  pkgs,
  system,
  ...
}:

/*
  home-manager configuration
  Useful links:
  - Home Manager Manual: https://nix-community.gitlab.io/home-manager/
  - Appendix A. Configuration Options: https://nix-community.gitlab.io/home-manager/options.html
*/
{
  home = {
    packages = __attrValues {
      inherit (pkgs) pfetch nix-output-monitor;
    };
  };

  programs = {
    nushell = {
      envFile.text = ''
        open ($env.XDG_RUNTIME_DIR | path join agenix/tokens) | split column " " | get column2 | split column "=" | reduce -f {} {|it, acc| $acc | upsert $it.column1 $it.column2 } | load-env
        $env.config.show_banner = false
      '';
    };
  };
}
