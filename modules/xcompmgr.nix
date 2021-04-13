{ config, lib, pkgs, ... }:

with lib;

let
  cfg = config.services.xcompmgr;
in
{
  options = {
    services.xcompmgr = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = "Enable xcompmgr, a simple composite manager";
      };

      clientSideCompositing = mkOption {
        type = types.bool;
        default = false;
        description = "Enable client-side compositing with soft shadows and translucency support";
      };

      hardShadows = mkOption {
        type = types.bool;
        default = false;
        description = "Enable hard shadows, requires clientSideCompositing = true;";
      };
    };
  };

  config =
    let
      xcompmgrCommand = "${pkgs.xcompmgr}/bin/xcompmgr"
        + optionalString cfg.clientSideCompositing " -c"
        + optionalString cfg.hardShadows " -s";
    in
    mkIf cfg.enable {
      systemd.user.services."xcompmgr" = {
        enable = true;
        description = "X composite manager";
        wantedBy = [ "default.target" ];
        serviceConfig.Restart = "always";
        serviceConfig.RestartSec = 2;
        serviceConfig.ExecStart = xcompmgrCommand;
      };
    };

  meta.maintainers = with maintainers; [ fortuneteller2k ];
}
