# hjem wiring shared by nix-darwin and NixOS
{
  inputs,
  inputs',
  self',
  ...
}:

{
  hjem = {
    specialArgs = { inherit inputs inputs' self'; };
    extraModules = [
      inputs.hjem-rum.hjemModules.default
      ../hjem
    ];
  };
}
