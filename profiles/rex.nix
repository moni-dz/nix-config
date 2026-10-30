{ config, lib, ... }:

let
  users = lib.filterAttrs (_: user: user.isNormalUser) config.users.users;
in
{
  services.rex.loginServer = {
    enable = true;
    accounts = lib.mapAttrs (_: _: { principals = [ "tailscale:moni-dz@github" ]; }) users;
    socketActivation = true;
    tailscaleServe.enable = true;
  };
}
