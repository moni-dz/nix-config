{
  description = "A somewhat huge NixOS configuration using Nix Flakes.";
  inputs = {
    emacs.url = "github:nix-community/emacs-overlay";
    # home.url = "github:nix-community/home-manager";
    home.url = "github:fortuneteller2k/home-manager/fix-pulseeffects-package";
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    nixpkgs-fork.url = "github:fortuneteller2k/nixpkgs/add-xanmod-kernel";
    nixpkgs-master.url = "github:nixos/nixpkgs/master";
    nvim-nightly.url = "github:nix-community/neovim-nightly-overlay";
    rust.url = "github:oxalica/rust-overlay";
  };
  outputs = { self, emacs, home, nixpkgs, nixpkgs-fork, nixpkgs-master, nvim-nightly, rust }@inputs: {
    nixosConfigurations.superfluous = (import ./hosts/superfluous/default.nix {
      inherit emacs home inputs nixpkgs nixpkgs-fork nixpkgs-master nvim-nightly rust;
    });
    superfluous = self.nixosConfigurations.superfluous.config.system.build.toplevel;
  };
}
