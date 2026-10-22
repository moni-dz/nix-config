{
  config,
  lib,
  pkgs,
  ...
}:

# Nushell on macOS reads ~/Library/Application Support/nushell unless XDG_CONFIG_HOME is set,
# so mirror the files hjem-rum writes under ~/.config/nushell.
let
  inherit (lib) genAttrs mkIf;
  names = [
    "config.nu"
    "env.nu"
  ];
in
{
  config = mkIf (pkgs.stdenv.hostPlatform.isDarwin && config.rum.programs.nushell.enable) {
    files = lib.mapAttrs' (n: _: {
      name = "Library/Application Support/nushell/${n}";
      value = {
        inherit (config.xdg.config.files."nushell/${n}") source;
      };
    }) (genAttrs names (_: null));
  };
}
