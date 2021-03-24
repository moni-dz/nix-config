{
  description = "A somewhat huge NixOS configuration using Nix Flakes.";
  inputs = {
    comma = { url = "github:Shopify/comma"; flake = false; };
    home = { url = "github:nix-community/home-manager"; inputs.nixpkgs.follows = "unstable"; };
    unstable.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    master.url = "github:nixos/nixpkgs/master";
    stable.url = "github:nixos/nixpkgs/release-20.09";
    nur.url = "github:nix-community/NUR";
    nvim-nightly.url = "github:nix-community/neovim-nightly-overlay";
    rust.url = "github:oxalica/rust-overlay";
  };
  outputs = { self, home, master, stable, unstable, nur, nvim-nightly, rust, ... }@inputs: {
    nixosConfigurations.superfluous = import ./hosts/superfluous {
      inherit home inputs master stable unstable nur nvim-nightly rust;
      nixpkgs = unstable;
    };
    superfluous = self.nixosConfigurations.superfluous.config.system.build.toplevel;
  };
}
