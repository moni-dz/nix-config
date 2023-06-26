{ inputs, withSystem, ... }:

{
  flake.darwinConfigurations.shaker = withSystem "aarch64-darwin" ({ inputs', system, nixpkgs-config, overlays, ... }@args:
    let
      inherit (inputs) agenix nixpkgs;
    in
    inputs.darwin.lib.darwinSystem {
      inherit inputs system;

      modules = [
        agenix.darwinModules.default

        {
          # NOTE: you should either change this or disable it completely by commenting it out
          age = {
            identityPaths = [ "/Users/moni/.ssh/id_ed25519" ];

            secrets.github-token = {
              file = ../../secrets/github-token.age;
              owner = "moni";
              group = "staff";
              mode = "600";
            };
          };

          nix = import ../../nix-settings.nix {
            inherit inputs inputs' system nixpkgs;
          };

          nixpkgs = {
            inherit overlays;
            config = nixpkgs-config;
          };
        }

        ./configuration.nix
      ];

      specialArgs = {
        inherit system;
        inherit (args) master unstable stable;
      };
    }
  );
}
