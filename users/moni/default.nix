{ config, nixpkgs, home, discocss, nix-colors, overlays, inputs }:

# See https://github.com/nix-community/home-manager/blob/master/flake.nix#L44 for reference.
let
  system = "x86_64-linux";
in
home.lib.homeManagerConfiguration {
  modules = [
    {
      nixpkgs = { inherit config overlays; };

      home = rec {
        username = "moni";
        homeDirectory = "/home/${username}";

        /*
          NOTE: DO NOT CHANGE THIS IF YOU DON'T KNOW WHAT YOU'RE DOING.

          Only change this if you are ABSOLUTELY 100% SURE that you don't have stateful data.
        */
        stateVersion = "21.11";
      };
    }

    # Extra home-manager modules that aren't upstream
    discocss.hmModule
    nix-colors.homeManagerModule
    ./home.nix
  ];

  # Default nixpkgs for home.nix
  pkgs = nixpkgs.outputs.legacyPackages.${system};

  # Extra arguments passed to home.nix
  extraSpecialArgs = { inherit inputs system; };
}
