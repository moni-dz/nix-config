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
    nixosConfigurations.superfluous = (import ./hosts/superfluous/default.nix {
      inherit emacs home inputs nixpkgs nixpkgs-fork nixpkgs-master rust;
    });
    superfluous = self.nixosConfigurations.superfluous.config.system.build.toplevel;
  };
}
