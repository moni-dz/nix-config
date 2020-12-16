{
  description = "A minimal NixOS configuration using Nix Flakes.";

  inputs = {
    nixpkgs = {
      url = "github:nixos/nixpkgs/nixpkgs-unstable";
    };
    home = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    emacs-overlay.url = "github:nix-community/emacs-overlay";
    nur.url = "github:nix-community/NUR";
  };

  outputs = inputs @ { self, nixpkgs, home, emacs-overlay, nur }: {
    nixosConfigurations.nixos = nixpkgs.lib.nixosSystem {
      system = "x86_64-linux";
      modules = [
        {
          nixpkgs = {
            config.allowUnfree = true;
            overlays = [
              emacs-overlay.overlay
              nur.overlay
            ];
          };
        }
        ./nixos/configuration.nix
        home.nixosModules.home-manager {
          home-manager = {
            useGlobalPkgs = true;
            useUserPackages = true;
            users.fortuneteller2k = import ./home/home.nix;
          };
        }
      ];
      specialArgs = { inherit inputs; };
    };

    nixos = self.nixosConfigurations.nixos.config.system.build.toplevel;
  };
}
