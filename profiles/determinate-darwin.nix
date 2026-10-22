# Determinate Nix on nix-darwin (install it manually first!), needs inputs.determinate.darwinModules.default
{
  lib,
  inputs,
  inputs',
  pkgs,
  ...
}:

let
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
