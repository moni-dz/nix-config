{
  description = "A somewhat huge NixOS configuration using Nix Flakes.";
  inputs = {
    emacs.url = "github:nix-community/emacs-overlay";
    home = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nixpkgs-master.url = "github:nixos/nixpkgs/master";
    nixpkgs-fork.url = "github:fortuneteller2k/nixpkgs/add-xanmod-kernel";
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    rust.url = "github:oxalica/rust-overlay";
  };
  outputs = { self, emacs, home, nixpkgs, nixpkgs-fork, nixpkgs-master, rust }@inputs: {
    nixosConfigurations.superfluous = nixpkgs.lib.nixosSystem rec {
      system = "x86_64-linux";
      modules = [
        {
          nix = (import ./config/nix-conf.nix { inherit inputs; });
          nixpkgs = let
            filterOverlays = k: v: with nixpkgs.lib; v == "regular" && hasSuffix ".nix" k;
            userOverlays = with nixpkgs.lib; (lists.forEach (mapAttrsToList
            (name: _: ./overlays + ("/" + name))
            (filterAttrs filterOverlays (builtins.readDir ./overlays)))) import;
            nixpkgs-overlays = final: prev: {
              master = nixpkgs-master.legacyPackages.${system};
              fork = nixpkgs-fork .legacyPackages.${system};
            };
          in {
            config = {
              allowUnfree = true;
              allowBroken = true;
            };
            overlays = [
              emacs.overlay
              rust.overlay
              nixpkgs-overlays
            ] ++ userOverlays;
          };
        }
        ./nixos/configuration.nix
        home.nixosModules.home-manager
        {
          home-manager = {
            useGlobalPkgs = true;
            useUserPackages = true;
            users.fortuneteller2k = import ./home/fortuneteller2k.nix;
          };
        }
      ];
      specialArgs = { inherit inputs; };
    };
    superfluous =
      self.nixosConfigurations.superfluous.config.system.build.toplevel;
  };
}
