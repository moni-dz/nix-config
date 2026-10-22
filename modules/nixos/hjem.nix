{ inputs, ... }:

{
  imports = [
    inputs.hjem.nixosModules.default
    ../common/hjem.nix
  ];
}
