{
  self,
  inputs,
  withSystem,
  ...
}:

{
  # Minimal Hyper-V guest
  flake.nixosConfigurations.hyper-v = withSystem "x86_64-linux" (
    { inputs', self', ... }:
    inputs.nixpkgs.lib.nixosSystem {
      specialArgs = {
        inherit
          inputs
          self
          inputs'
          self'
          ;
      };
      modules = [
        inputs.agenix.nixosModules.default
        inputs.agenix-rekey.nixosModules.default
        inputs.determinate.nixosModules.default
        inputs.inshellah.nixosModules.default
        inputs.rex.nixosModules.default
        ./system.nix
        ./hardware.nix
        ./rex.nix
        ./moni.nix
        ../../modules/common/identity.nix
        ../../modules/nixos/hjem.nix
        ../../profiles/identity.nix
        ../../profiles/secrets.nix
        ../../profiles/nixpkgs.nix
        ../../profiles/nix.nix
        ../../profiles/remote-builders.nix
        ../../profiles/determinate-nixos.nix
        ../../profiles/hjem-system.nix
        ../../profiles/dix.nix
        ../../profiles/inshellah.nix
      ];
    }
  );
}
