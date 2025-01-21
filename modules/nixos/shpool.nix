{
  config,
  lib,
  pkgs,
  ...
}:

let
  cfg = config.services.shpool;
in
{
  options.services.shpool = {
    enable = lib.mkEnableOption "shpool, a service that enables session persistence";
    package = lib.mkPackageOption pkgs "shpool" { };
  };

  config = lib.mkIf cfg.enable {
    environment.systemPackages = lib.mkIf (cfg.package != null) [ cfg.package ];
    systemd.user = {
      services.shpool = {
        description = "Shpool - Shell Session Pool";
        requires = [ "shpool.socket" ];

        serviceConfig = {
          Type = "simple";
          ExecStart = "${cfg.package}/bin/shpool daemon";
          KillMode = "mixed";
          TimeoutStopSec = "2s";
          SendSIGHUP = "yes";
        };

        wantedBy = [ "default.target" ];
      };

      sockets.shpool = {
        description = "Shpool Shell Session Pooler";

        socketConfig = {
          ListenStream = "%t/shpool/shpool.socket";
          SocketMode = "0600";
        };

        wantedBy = [ "sockets.target" ];
      };
    };
  };
}
