{ inputs, withSystem, ... }:

{
  flake.nixosConfigurations.starcruiser = withSystem "x86_64-linux" ({ inputs', system, nixpkgs-config, overlays, ... }@args:
    # See https://github.com/NixOS/nixpkgs/blob/master/flake.nix#L24 for reference.
    let
      inherit (inputs) agenix nixpkgs;
    in
    nixpkgs.lib.nixosSystem {
      modules = [
        agenix.nixosModules.age

        {
          # NOTE: you should either change this or disable it completely by commenting it out
          age.secrets.github-token = {
            file = ../../secrets/github-token.age;
            owner = "moni";
            mode = "0444";
          };

          nix = import ../../nix-settings.nix {
            inherit inputs inputs' system nixpkgs;
          };

          nixpkgs = {
            inherit overlays;
            config = nixpkgs-config;
          };

          networking.hostName = "starcruiser";

          /*
          NOTE: DO NOT CHANGE THIS IF YOU DON'T KNOW WHAT YOU'RE DOING.
          Only change this if you are ABSOLUTELY 100% SURE that you don't have stateful data.
          */
          system.stateVersion = "22.11";
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
