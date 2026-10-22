# Nix the package manager, for hosts without Determinate Nix
{
  lib,
  inputs,
  inputs',
  pkgs,
  ...
}:

{
  nix = import ./nix-settings.nix {
    inherit
      lib
      inputs
      inputs'
      pkgs
      ;
  };
}
