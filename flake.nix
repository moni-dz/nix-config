{
  description = "A somewhat huge NixOS configuration using Nix Flakes.";
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/master";
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
          nix = (import ./config/nix-conf.nix { inherit inputs; });
          nixpkgs = let
            folder = ./overlays;
            toPath = name: value: folder + ("/" + name);
            filterOverlays = key: value:
              value == "regular" && nixpkgs.lib.hasSuffix ".nix" key;
            userOverlays = nixpkgs.lib.lists.forEach (nixpkgs.lib.mapAttrsToList toPath
              (nixpkgs.lib.filterAttrs filterOverlays (builtins.readDir folder)))
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
