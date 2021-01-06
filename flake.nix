{
  description = "A minimal NixOS configuration using Nix Flakes.";
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    home = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    emacs.url = "github:nix-community/emacs-overlay";
    nur.url = "github:nix-community/NUR";
    hardware.url = "github:NixOS/nixos-hardware/master";
  };
  outputs = inputs@{ self, nixpkgs, home, emacs, nur, hardware }: {
    nixosConfigurations.superfluous = nixpkgs.lib.nixosSystem {
      system = "x86_64-linux";
      modules = [
        {
          nixpkgs = {
            config = {
              allowUnfree = true;
              allowBroken = true;
            };
            overlays = [
              emacs.overlay
              nur.overlay
              (import ./overlays/slock.nix)
              (import ./overlays/picom-next.nix)
              (import ./overlays/polybar.nix)
              (import ./overlays/alacritty-ligatures.nix)
              (import ./overlays/weechat-unwrapped.nix)
              (import ./overlays/derivations.nix)
            ];
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
