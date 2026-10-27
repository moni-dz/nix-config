# Determinate Nix on nix-darwin (install it manually first!), needs inputs.determinate.darwinModules.default
{
  config,
  lib,
  inputs,
  inputs',
  self',
  pkgs,
  ...
}:

let
  nixPackage = self'.packages.nix-src;

  limits = {
    NumberOfFiles = 1048576;
    NumberOfProcesses = 1048576;
    Stack = 67108864;
  };

  nix = import ./nix-settings.nix {
    inherit
      lib
      inputs
      inputs'
      pkgs
      ;
  };
in
{
  nix.enable = false;

  environment.systemPackages = [ nixPackage ];

  # darwin-rebuild puts root's ~/.nix-profile first in PATH, which the installer filled with stock nix
  system.activationScripts.postActivation.text = ''
    echo "pointing root's nix profile at the patched nix..." >&2
    ${nixPackage}/bin/nix-env -p /nix/var/nix/profiles/per-user/root/profile --set ${nixPackage}
  '';

  launchd.daemons.determinate-nix-daemon.serviceConfig = {
    Label = "systems.determinate.nix-daemon";
    ProgramArguments = [
      "/usr/local/bin/determinate-nixd"
      "--nix-bin"
      "${nixPackage}/bin"
      "daemon"
    ];
    RunAtLoad = true;
    Sockets = {
      "nix-daemon.socket" = {
        SockPassive = true;
        SockPathName = "/var/run/nix-daemon.socket";
      };
      "determinate-nixd.socket" = {
        SockPassive = true;
        SockPathName = "/var/run/determinate-nixd.socket";
      };
    };
    StandardErrorPath = "/var/log/determinate-nix-daemon.log";
    StandardOutPath = "/var/log/determinate-nix-daemon.log";
    SoftResourceLimits = limits;
    HardResourceLimits = limits;
  };

  determinateNix = {
    enable = true;

    customSettings = lib.mkMerge [
      nix.settings
      { inherit (config.nix.settings) builders; }
      # only Determinate Nix knows these
      {
        eval-cores = 0;
        lazy-trees = true;
        extra-experimental-features = [ "parallel-eval" ];
      }
    ];
  };
}
