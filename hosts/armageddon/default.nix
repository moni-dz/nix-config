{ config, darwin, overlays, inputs }:

let
 system = "aarch64-darwin";
in darwin.lib.darwinSystem {
  inherit inputs system;

  modules = [
    {
      nix = import ../../nix-settings.nix {
        inherit inputs system;
        inherit (inputs) nixpkgs;
        max-jobs = 4;
      };

      nixpkgs = { inherit config overlays; };
    }

    ./configuration.nix
  ];

  specialArgs = { inherit system; };
}
