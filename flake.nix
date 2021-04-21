{
  description = "A somewhat huge NixOS configuration using Nix Flakes.";

  inputs = rec {
    comma = {
      url = "github:Shopify/comma";
      flake = false;
    };

    home = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "unstable";
    };

    master.url = "github:nixos/nixpkgs/master";
    stable.url = "github:nixos/nixpkgs/release-20.09";
    staging.url = "github:nixos/nixpkgs/staging";
    staging-next.url = "github:nixos/nixpkgs/staging-next";
    unstable.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    emacs.url = "github:nix-community/emacs-overlay";
    nur.url = "github:nix-community/NUR";
    nvim-nightly.url = "github:nix-community/neovim-nightly-overlay";
    rust.url = "github:oxalica/rust-overlay";

    # default nixpkgs branch
    nixpkgs = unstable;
  };

  outputs = { self, home, master, stable, staging, staging-next, unstable, nixpkgs, ... } @ inputs: {
    nixosConfigurations.superfluous = import ./hosts/superfluous {
      inherit home inputs master stable staging staging-next unstable nixpkgs;
    };

    superfluous = self.nixosConfigurations.superfluous.config.system.build.toplevel;
  };
}
