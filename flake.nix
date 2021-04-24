{
  description = "A somewhat huge NixOS configuration using Nix Flakes.";

  inputs = {
    comma = {
      url = "github:Shopify/comma";
      flake = false;
    };

    home = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "unstable";
    };

    emacs.url = "github:nix-community/emacs-overlay";
    nur.url = "github:nix-community/NUR";
    neovim-nightly.url = "github:nix-community/neovim-nightly-overlay";
    rust.url = "github:oxalica/rust-overlay";

    # nixpkgs branches
    master.url = "github:nixos/nixpkgs/master";
    stable.url = "github:nixos/nixpkgs/release-20.09";
    staging.url = "github:nixos/nixpkgs/staging";
    staging-next.url = "github:nixos/nixpkgs/staging-next";
    unstable.url = "github:nixos/nixpkgs/nixpkgs-unstable";

    # default nixpkgs for packages and modules
    nixpkgs.follows = "unstable";
  };

  outputs = { self, home, master, stable, staging, staging-next, unstable, nixpkgs, ... } @ inputs: {
    nixosConfigurations.superfluous = import ./hosts/superfluous {
      inherit home inputs master stable staging staging-next unstable nixpkgs;
    };

    superfluous = self.nixosConfigurations.superfluous.config.system.build.toplevel;
  };
}
