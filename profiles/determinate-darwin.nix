# Determinate Nix on nix-darwin (install it manually first!), needs inputs.determinate.darwinModules.default
{
  lib,
  inputs,
  inputs',
  pkgs,
  ...
}:

let
  nix-src = import inputs.nix-src.inputs.nixpkgs {
    localSystem = pkgs.stdenv.hostPlatform.system;
    overlays = [
      (final: prev: { libgit2 = prev.libgit2.overrideAttrs { doCheck = false; }; })
      inputs.nix-src.overlays.internal
    ];
  };

  nixPackage = pkgs.lib.infuse nix-src.nixComponents2.nix-everything {
    __output.doCheck.__assign = false;
  };

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

  determinateNix.enable = true;

  environment.systemPackages = [ nixPackage ];

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

  determinateNix.customSettings = lib.mkMerge [
    nix.settings
    # only Determinate Nix knows these
    {
      eval-cores = 0;
      lazy-trees = true;
      extra-experimental-features = [ "parallel-eval" ];
    }
  ];
}
