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
  # nix-darwin configurations
  flake.darwinConfigurations.riscake = withSystem "aarch64-darwin" (
    { inputs', self', ... }:
    inputs.darwin.lib.darwinSystem {
      specialArgs = {
        inherit
          inputs
          self
          inputs'
          self'
          ;
      };
      modules = [
        inputs.agenix.darwinModules.default
        inputs.determinate.darwinModules.default
        inputs.nix-index-database.darwinModules.nix-index
        ../modules/common/identity.nix
        ../modules/darwin/hjem.nix
        ./riscake/configuration.nix
        ../users/moni
        ../profiles/identity.nix
        ../profiles/nixpkgs.nix
        ../profiles/hjem-system.nix
        ../profiles/base-packages.nix
        ../profiles/determinate-darwin.nix
      ];
    }
  );

  # NixOS configurations
  flake.nixosConfigurations = {
    # Steam Deck (Jovian-NixOS)
    hands = withSystem "x86_64-linux" (
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
          inputs.jovian.nixosModules.default
          inputs.rex.nixosModules.default
          ../modules/common/identity.nix
          ../modules/nixos/hjem.nix
          ./hands/configuration.nix
          ../profiles/identity.nix
          ../profiles/nixpkgs.nix
          ../profiles/nix.nix
          ../profiles/hjem-system.nix
          ../profiles/base-packages.nix
        ];
      }
    );
  };
}
