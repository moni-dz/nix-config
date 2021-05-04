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
    kernel.url = "github:fortuneteller2k/nixpkgs/update-xanmod-512";

    # default nixpkgs for packages and modules
    nixpkgs.follows = "master";
  };

  outputs = { self, home, master, stable, staging, staging-next, unstable, nixpkgs, ... } @ inputs:
    with nixpkgs.lib;
    let
      config = {
        allowBroken = true;
        allowUnfree = true;
      };

      filterNixFiles = k: v: v == "regular" && hasSuffix ".nix" k;

      importNixFiles = path: (lists.forEach (mapAttrsToList (name: _: path + ("/" + name))
        (filterAttrs filterNixFiles (builtins.readDir path)))) import;

      user-overlays = importNixFiles ./overlays;
    in
    {
      nixosConfigurations.superfluous = import ./hosts/superfluous {
        inherit config home inputs master stable staging staging-next unstable nixpkgs user-overlays;
      };

      superfluous = self.nixosConfigurations.superfluous.config.system.build.toplevel;
    };
}
