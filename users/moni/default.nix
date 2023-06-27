{ inputs, withSystem, ... }:

withSystem "aarch64-darwin" ({ inputs', system, nixpkgs-config, overlays, ... }@args:
# See https://github.com/nix-community/home-manager/blob/master/flake.nix#L44 for reference.
let
  inherit (inputs) home nixpkgs;
in
home.lib.homeManagerConfiguration {
  modules = [
    ({ lib, ... }: {
      # Extra arguments passed to the module system
      _module.args = {
        inherit inputs inputs' system;
        inherit (args) master unstable stable;
      };

      nixpkgs = {
        inherit overlays;
        config = nixpkgs-config;
      };

      home = rec {
        username = "moni";
        homeDirectory = "/Users/${username}";

        /*
          NOTE: DO NOT CHANGE THIS IF YOU DON'T KNOW WHAT YOU'RE DOING.

          Only change this if you are ABSOLUTELY 100% SURE that you don't have stateful data.
        */
        stateVersion = "23.05";
      };
    })

    # Shared configuration across all users
    ../shared/home.nix

    # Particular configuration for the user
    ./home.nix
  ];

  # Default nixpkgs for home.nix
  pkgs = nixpkgs.outputs.legacyPackages.${system};
})
