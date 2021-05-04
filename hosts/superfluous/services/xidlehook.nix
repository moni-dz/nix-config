{ config, lib, pkgs }:

{
  description = "xidlehook daemon";
  wantedBy = [ "graphical-session.target" ];
  partOf = [ "graphical-session.target" ];
  serviceConfig.ExecStart = lib.strings.concatStringsSep " "
    [
      ""
      "${pkgs.xidlehook}/bin/xidlehook"
      "--not-when-fullscreen"
      "--not-when-audio"
      "--timer 120 ${config.security.wrapperDir}/slock ''"
    ];
}
