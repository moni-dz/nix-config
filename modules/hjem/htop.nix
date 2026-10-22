{
  config,
  lib,
  pkgs,
  ...
}:

let
  inherit (lib)
    concatMapAttrsStringSep
    concatStringsSep
    isBool
    isList
    mkEnableOption
    mkIf
    mkOption
    mkPackageOption
    types
    ;

  cfg = config.rum.programs.htop;

  render =
    v:
    if isBool v then
      if v then "1" else "0"
    else if isList v then
      concatStringsSep " " (map toString v)
    else
      toString v;
in
{
  options.rum.programs.htop = {
    enable = mkEnableOption "htop";
    package = mkPackageOption pkgs "htop" { };

    settings = mkOption {
      type = types.attrsOf (
        types.oneOf [
          types.bool
          types.int
          types.str
          (types.listOf (
            types.oneOf [
              types.int
              types.str
            ]
          ))
        ]
      );
      default = { };
      description = "Key/value pairs written to {file}`$XDG_CONFIG_HOME/htop/htoprc`.";
    };
  };

  config = mkIf cfg.enable {
    packages = [ cfg.package ];

    xdg.config.files."htop/htoprc" = mkIf (cfg.settings != { }) {
      text = concatMapAttrsStringSep "\n" (n: v: "${n}=${render v}") cfg.settings + "\n";
    };
  };
}
