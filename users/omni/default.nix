{ inputs, withSystem, ... }:

withSystem "x86-64-linux" ({ inputs', system, nixpkgs-config, overlays, ... }@args:
# See https://github.com/nix-community/home-manager/blob/master/flake.nix#L44 for reference.
let
  inherit (inputs) home nix-colors nixpkgs;
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
        homeDirectory = "/home/${username}";

        /*
              NOTE: DO NOT CHANGE THIS IF YOU DON'T KNOW WHAT YOU'RE DOING.

              Only change this if you are ABSOLUTELY 100% SURE that you don't have stateful data.
            */
        stateVersion = "21.11";
      };
    })

    # Extra home-manager modules that aren't upstream
    nix-colors.homeManagerModule

    ./home.nix
  ];

  # Default nixpkgs for home.nix
  pkgs = nixpkgs.outputs.legacyPackages.${system};
})
