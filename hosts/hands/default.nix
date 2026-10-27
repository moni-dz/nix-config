{
  self,
  inputs,
  withSystem,
  ...
}:

/*
  Shared profiles go after the host's own modules: list options (e.g. `extra-trusted-users`,
  `environment.systemPackages`) merge in module order, and this keeps the order the systems had
  when the builder appended them.
*/
{
  # Steam Deck (Jovian-NixOS)
  flake.nixosConfigurations.hands = withSystem "x86_64-linux" (
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
        inputs.jovian.nixosModules.default
        inputs.rex.nixosModules.default
        ../../modules/common/identity.nix
        ../../modules/nixos/hjem.nix
        ./system.nix
        ./hardware.nix
        ./desktop.nix
        ./rex.nix
        ./lyt.nix
        ../../profiles/identity.nix
        ../../profiles/secrets.nix
        ../../profiles/nixpkgs.nix
        ../../profiles/nix.nix
        ../../profiles/remote-builders.nix
        ../../profiles/determinate-nixos.nix
        ../../profiles/hjem-system.nix
        ../../profiles/base-packages.nix
        ../../profiles/dix.nix
        ../../profiles/inshellah.nix
      ];
    }
  );
}
