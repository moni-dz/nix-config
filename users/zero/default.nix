{ config, nixpkgs, home, overlays, inputs }:

# See https://github.com/nix-community/home-manager/blob/master/flake.nix#L44 for reference.
let
  system = "x86_64-linux";
in
home.lib.homeManagerConfiguration {
  modules = [
    { nixpkgs = { inherit config overlays; }; }
    ./home.nix
  ];

  # Default nixpkgs for home.nix
  pkgs = nixpkgs.outputs.legacyPackages.${system};

  # Extra arguments passed to home.nix
  extraSpecialArgs = { inherit inputs system; };
}
