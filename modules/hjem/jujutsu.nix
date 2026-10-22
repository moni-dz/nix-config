{
  config,
  lib,
  pkgs,
  ...
}:

let
  inherit (lib)
    getExe
    mkAfter
    mkEnableOption
    mkIf
    mkOption
    mkPackageOption
    ;

  toml = pkgs.formats.toml { };
  cfg = config.rum.programs.jujutsu;
in
{
  options.rum.programs.jujutsu = {
    enable = mkEnableOption "jujutsu";
    package = mkPackageOption pkgs "jujutsu" { };

    settings = mkOption {
      inherit (toml) type;
      default = { };
      description = "Configuration written to {file}`$XDG_CONFIG_HOME/jj/config.toml`.";
    };

    integrations.nushell.enable = mkEnableOption "jujutsu completions for nushell";
  };

  config = mkIf cfg.enable {
    packages = [ cfg.package ];

    xdg.config.files."jj/config.toml" = mkIf (cfg.settings != { }) {
      source = toml.generate "jj-config.toml" cfg.settings;
    };

    rum.programs.nushell.extraConfig = mkIf cfg.integrations.nushell.enable (mkAfter ''
      source ${
        pkgs.runCommand "jj-completion-nu" { } ''${getExe cfg.package} util completion nushell > "$out"''
      }
    '');
  };
}
