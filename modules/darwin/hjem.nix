{ inputs, ... }:

{
  imports = [
    inputs.hjem.darwinModules.default
    ../common/hjem.nix
  ];
}
