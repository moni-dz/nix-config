{
  config,
  lib,
  self',
  ...
}:

let
  inherit (lib)
    mkEnableOption
    mkIf
    mkOption
    types
    ;

  cfg = config.rum.programs.neovim;
  neovim = self'.packages.neovim;
in
{
  options.rum.programs.neovim = {
    enable = mkEnableOption "Neovim, wrapped with its plugins, language servers and config";

    extraPackages = mkOption {
      type = types.listOf types.package;
      default = neovim.passthru.defaultExtraPackages;
      description = "Language servers and tools put on Neovim's PATH, and nothing else's.";
    };

    package = mkOption {
      type = types.package;
      description = "The wrapped Neovim, the config in {file}`packages/neovim/lua/moni` is baked in.";
      default = neovim.override { inherit (cfg) extraPackages; };
    };
  };

  config = mkIf cfg.enable {
    packages = [ cfg.package ];
  };
}
