{
  description = "A minimal NixOS configuration using Nix Flakes.";
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    wayland.url = "github:colemickens/nixpkgs-wayland";
    home = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    rust.url = "github:oxalica/rust-overlay";
    emacs.url = "github:nix-community/emacs-overlay";
    nur.url = "github:nix-community/NUR";
    hardware.url = "github:NixOS/nixos-hardware/master";
  };
  outputs = { self, nixpkgs, wayland, home, emacs, nur, hardware, rust }@inputs: {
    nixosConfigurations.superfluous = nixpkgs.lib.nixosSystem {
      system = "x86_64-linux";
      modules = [
        {
          nix = {
            extraOptions = "experimental-features = nix-command flakes";
            gc = {
              automatic = true;
              dates = "weekly";
              options = "--delete-older-than 7d";
            };
            maxJobs = 4;
            autoOptimiseStore = true;
            daemonNiceLevel = 15;
            daemonIONiceLevel = 5;
            binaryCachePublicKeys = [
              "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
              "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
              "nixpkgs-wayland.cachix.org-1:3lwxaILxMRkVhehr5StQprHdEo4IrE8sRho9R9HOLYA="
            ];
            binaryCaches = [
              "https://cache.nixos.org"
              "https://nix-community.cachix.org"
              "https://nixpkgs-wayland.cachix.org"
            ];
          };
          nixpkgs = let
            lib = nixpkgs.lib;
            folder = ./overlays;
            toPath = name: value: folder + ("/" + name);
            filterOverlays = key: value:
              value == "regular" && lib.hasSuffix ".nix" key;
            userOverlays = lib.lists.forEach (lib.mapAttrsToList toPath
              (lib.filterAttrs filterOverlays (builtins.readDir folder)))
              import;
          in {
            config = {
              allowUnfree = true;
              allowBroken = true;
            };
            overlays = [
              emacs.overlay
              nur.overlay
              rust.overlay
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
