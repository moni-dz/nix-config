{
  description = "A somewhat huge NixOS configuration using Nix Flakes.";
  inputs = {
    emacs.url = "github:nix-community/emacs-overlay";
    home = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "unstable";
    };
    unstable.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    fork.url = "github:fortuneteller2k/nixpkgs/add-xanmod-kernel";
    master.url = "github:nixos/nixpkgs/master";
    stable.url = "github:nixos/nixpkgs/nixos-20.09";
    nvim-nightly.url = "github:nix-community/neovim-nightly-overlay";
    rust.url = "github:oxalica/rust-overlay";
  };
  outputs = { self, emacs, home, fork, master, stable, unstable, nvim-nightly, rust }@inputs: {
    nixosConfigurations.superfluous = (import ./hosts/superfluous/default.nix {
      inherit emacs home inputs fork master stable unstable nvim-nightly rust;
    });
    superfluous = self.nixosConfigurations.superfluous.config.system.build.toplevel;
  };
}
