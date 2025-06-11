{
  config,
  lib,
  pkgs,
  ...
}:

{
  wsl.defaultUser = "moni";

  users.users.moni = {
    isNormalUser = true;
    home = "/home/moni";
    shell = pkgs.nushell;
    extraGroups = [ "wheel" ];
  };
}
