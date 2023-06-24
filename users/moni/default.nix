{ config, nixpkgs, system, home, overlays, inputs, master, unstable, stable, ... }:

# See https://github.com/nix-community/home-manager/blob/master/flake.nix#L44 for reference.
home.lib.homeManagerConfiguration {
  modules = [
    {
      nixpkgs = { inherit config overlays; };

      home = rec {
        username = "moni";
        homeDirectory = "/Users/${username}";

        /*
          NOTE: DO NOT CHANGE THIS IF YOU DON'T KNOW WHAT YOU'RE DOING.

          Only change this if you are ABSOLUTELY 100% SURE that you don't have stateful data.
        */
        stateVersion = "23.05";
      };
    }

    # Shared configuration across all users
    ../shared/home.nix

    # Particular configuration for the user
    ./home.nix
  ];

  # Default nixpkgs for home.nix
  pkgs = nixpkgs.outputs.legacyPackages.${system};

  # Extra arguments passed to home.nix
  extraSpecialArgs = { inherit inputs system master unstable stable; };
}
