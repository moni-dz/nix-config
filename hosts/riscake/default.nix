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
        inputs.agenix-rekey.darwinModules.default
        inputs.determinate.darwinModules.default
        inputs.nix-index-database.darwinModules.nix-index
        ../../modules/common/identity.nix
        ../../modules/darwin/hjem.nix
        ./system.nix
        ./moni.nix
        ../../profiles/identity.nix
        ../../profiles/secrets.nix
        ../../profiles/nixpkgs.nix
        ../../profiles/hjem-system.nix
        ../../profiles/base-packages.nix
        ../../profiles/determinate-darwin.nix
      ];
    }
  );
}
