{ config, nixpkgs, home, overlays, inputs }:

# See https://github.com/nix-community/home-manager/blob/master/flake.nix#L44 for reference.
home.lib.homeManagerConfiguration rec {
  system = "x86_64-linux";
  username = "zero";
  homeDirectory = "/home/${username}";

  configuration.imports = [
    { nixpkgs = { inherit config overlays; }; }
    ./home.nix
  ];

  # Default nixpkgs for home.nix
  pkgs = nixpkgs.outputs.legacyPackages.${system};

  # Extra arguments passed to home.nix
  extraSpecialArgs = { inherit inputs system; };

  /*
    NOTE: DO NOT CHANGE THIS IF YOU DON'T KNOW WHAT YOU'RE DOING.
    Only change this if you are ABSOLUTELY 100% SURE that you don't have stateful data.
  */
  stateVersion = "21.11";
}
