{ inputs, withSystem, ... }:

{
  flake.darwinConfigurations.shaker = withSystem "aarch64-darwin" ({ inputs', system, nixpkgs-config, overlays, ... }@args:
    let
      inherit (inputs) agenix darwin nixpkgs;
    in
    darwin.lib.darwinSystem {
      inherit inputs system;

      modules = [
        agenix.darwinModules.default

        ({ lib, pkgs, ... }: {
          # Extra arguments passed to the module system
          _module.args = {
            inherit inputs' system;
            inherit (args) master unstable stable;
            inputs = lib.mkForce inputs;
          };

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
            inherit lib inputs inputs';
            inherit (pkgs) stdenv;
          };

          nixpkgs = {
            inherit overlays;
            config = nixpkgs-config;
          };

          /*
            NOTE: DO NOT CHANGE THIS IF YOU DON'T KNOW WHAT YOU'RE DOING.
          
            Only change this if you are ABSOLUTELY 100% SURE that you don't have stateful data.
          */
          system.stateVersion = 4;
        })

        ./configuration.nix
      ];
    }
  );
}
