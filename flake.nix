{
  description = "A somewhat huge NixOS/nix-darwin/home-manager configuration using Nix Flakes.";
  nixConfig.commit-lockfile-summary = "flake: bump inputs";

  outputs = inputs: inputs.parts.lib.mkFlake { inherit inputs; } {
    systems = [ "aarch64-darwin" "x86_64-linux" ];
    imports = [ ./modules/parts ./overlays ./hosts ./users ];
  };

  inputs = {
    # Flake inputs
    agenix.url = "github:ryantm/agenix";
    emacs.url = "github:nix-community/emacs-overlay";
    darwin.url = "github:lnl7/nix-darwin";
    home.url = "github:nix-community/home-manager";
    nix.url = "github:nixos/nix";
    nix-colors.url = "github:Misterio77/nix-colors";
    nix-index-database.url = "github:Mic92/nix-index-database";
    nixos-wsl.url = "github:nix-community/nixos-wsl";
    nixpkgs-f2k.url = "github:fortuneteller2k/nixpkgs-f2k";
    nixpkgs-fmt.url = "github:nix-community/nixpkgs-fmt";
    nixpkgs-wayland.url = "github:nix-community/nixpkgs-wayland";
    parts.url = "github:hercules-ci/flake-parts";
    statix.url = "github:nerdypepper/statix";

    # Nixpkgs branches
    master.url = "github:nixos/nixpkgs/master";
    stable.url = "github:nixos/nixpkgs/release-23.05";
    unstable.url = "github:nixos/nixpkgs/nixpkgs-unstable";

    # Default Nixpkgs for packages and modules
    nixpkgs.follows = "master";

    # Minimize duplicate instances of inputs
    agenix.inputs.nixpkgs.follows = "nixpkgs";
    agenix.inputs.darwin.follows = "darwin";
    darwin.inputs.nixpkgs.follows = "nixpkgs";
    emacs.inputs.nixpkgs.follows = "nixpkgs";
    home.inputs.nixpkgs.follows = "nixpkgs";
    nix.inputs.nixpkgs.follows = "nixpkgs";
    nix-index-database.inputs.nixpkgs.follows = "nixpkgs";
    nixos-wsl.inputs.nixpkgs.follows = "nixpkgs";
    nixpkgs-f2k.inputs.nixpkgs.follows = "nixpkgs";
    statix.inputs.nixpkgs.follows = "nixpkgs";
  };
}
