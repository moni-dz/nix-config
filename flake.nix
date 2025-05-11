{
  description = "A somewhat huge NixOS/nix-darwin/home-manager configuration using Nix Flakes.";
  nixConfig.commit-lockfile-summary = "flake: bump system inputs";

  outputs =
    inputs:
    inputs.parts.lib.mkFlake { inherit inputs; } {
      systems = [
        "aarch64-darwin"
        "x86_64-linux"
      ];

      imports = [
        inputs.parts.flakeModules.partitions
        ./modules/parts
        ./modules/nixos
        ./hosts
        ./users
      ];

      partitions = {
        drvs = {
          extraInputsFlake = ./packages;
          module = ./packages/module.nix;
        };

        dev = {
          extraInputsFlake = ./dev;
          module = ./dev/module.nix;
        };
      };

      partitionedAttrs = {
        overlays = "drvs";
        packages = "drvs";
        formatter = "dev";
      };
    };

  inputs = {
    # Flake inputs
    agenix.url = "github:ryantm/agenix";
    darwin.url = "github:lnl7/nix-darwin";
    home.url = "github:nix-community/home-manager/master";
    nix.url = "github:NixOS/nix/master";
    nix-colors.url = "github:Misterio77/nix-colors";
    nix-index-database.url = "github:Mic92/nix-index-database";
    nix-minecraft.url = "github:Infinidoge/nix-minecraft";
    nixos-wsl.url = "github:nix-community/nixos-wsl";
    parts.url = "github:hercules-ci/flake-parts";

    # Non-flake inputs
    infuse = {
      url = "git+https://codeberg.org/amjoseph/infuse.nix.git";
      flake = false;
    };

    # Nixpkgs branches
    master.url = "github:nixos/nixpkgs/master";

    # Default Nixpkgs for packages and modules
    nixpkgs.follows = "master";

    # Minimize duplicate instances of inputs
    agenix.inputs.nixpkgs.follows = "nixpkgs";
    agenix.inputs.darwin.follows = "darwin";
    darwin.inputs.nixpkgs.follows = "nixpkgs";
    home.inputs.nixpkgs.follows = "nixpkgs";
    nix.inputs.nixpkgs.follows = "nixpkgs";
    nix.inputs.flake-parts.follows = "parts";
    nix-index-database.inputs.nixpkgs.follows = "nixpkgs";
    nix-minecraft.inputs.nixpkgs.follows = "nixpkgs";
    nixos-wsl.inputs.nixpkgs.follows = "nixpkgs";
  };
}
