{
  description = "A minimal NixOS configuration using Nix Flakes.";

  inputs = {
    nixpkgs = {
      url = "github:nixos/nixpkgs/nixpkgs-unstable";
    };
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    emacs-overlay.url = "github:nix-community/emacs-overlay";
  };

  outputs = inputs: {
    nixosConfigurations.nixos = inputs.nixpkgs.lib.nixosSystem {
      system = "x86_64-linux";
      modules = [
        {
          nixpkgs = {
            config.allowUnfree = true;
            overlays = [ inputs.emacs-overlay.overlay ];
          };
        }
        ./configuration.nix
        inputs.home-manager.nixosModules.home-manager {
          home-manager = {
            useGlobalPkgs = true;
            useUserPackages = true;
            users.fortuneteller2k = import ../nixpkgs/home.nix;
          };
        }
      ];
      specialArgs = { inherit inputs; };
    };
  };
}
