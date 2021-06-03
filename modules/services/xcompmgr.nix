{ config, lib, pkgs, ... }:

with lib;

let cfg = config.services.xcompmgr;
in {
  options = {
    services.xcompmgr = {
      enable = mkEnableOption "Enable xcompmgr, a simple composite manager";
      clientSideCompositing = mkEnableOption
        "Enable client-side compositing with soft shadows and translucency support";
      hardShadows = mkEnableOption
        "Enable hard shadows, requires clientSideCompositing = true;";
    };
  };

  config = let
    xcompmgrCommand = "${pkgs.xcompmgr}/bin/xcompmgr"
      + optionalString cfg.clientSideCompositing " -c"
      + optionalString cfg.hardShadows " -s";
  in mkIf cfg.enable {
    systemd.user.services."xcompmgr" = {
      enable = true;
      description = "X composite manager";
      wantedBy = [ "default.target" ];
      serviceConfig.Restart = "always";
      serviceConfig.RestartSec = 2;
      serviceConfig.ExecStart = xcompmgrCommand;
    };
  };

  meta.maintainers = with lib.maintainers; [ fortuneteller2k ];
}
