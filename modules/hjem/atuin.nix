{
  config,
  lib,
  pkgs,
  ...
}:

let
  inherit (lib)
    mkEnableOption
    mkIf
    mkOption
    mkPackageOption
    mkAfter
    getExe
    ;

  toml = pkgs.formats.toml { };
  cfg = config.rum.programs.atuin;
in
{
  options.rum.programs.atuin = {
    enable = mkEnableOption "atuin";
    package = mkPackageOption pkgs "atuin" { };

    settings = mkOption {
      inherit (toml) type;
      default = { };
      description = "Configuration written to {file}`$XDG_CONFIG_HOME/atuin/config.toml`.";
    };

    integrations.nushell.enable = mkEnableOption "atuin integration with nushell";
  };

  config = mkIf cfg.enable {
    packages = [ cfg.package ];

    xdg.config.files."atuin/config.toml" = mkIf (cfg.settings != { }) {
      source = toml.generate "atuin-config.toml" cfg.settings;
    };

    rum.programs.nushell.extraConfig =
      mkIf (cfg.integrations.nushell.enable && config.rum.programs.nushell.enable)
        (mkAfter ''
          source ${
            pkgs.runCommand "atuin-init-nu" { } ''
              export HOME=$TMPDIR
              ${getExe cfg.package} init nu > "$out"
            ''
          }
        '');
  };
}
