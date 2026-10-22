{
  description = "A somewhat huge NixOS/nix-darwin/hjem configuration using Nix Flakes.";

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
    darwin.url = "github:nix-darwin/nix-darwin";
    determinate.url = "https://flakehub.com/f/DeterminateSystems/determinate/3";
    hjem.url = "github:feel-co/hjem";
    hjem-rum.url = "github:snugnug/hjem-rum";
    jovian.url = "github:Jovian-Experiments/Jovian-NixOS/6ba37ebb7c68b1b6711027a67122aa41060d216e";
    nix.url = "github:NixOS/nix/master";
    nix-index-database.url = "github:Mic92/nix-index-database";
    parts.url = "github:hercules-ci/flake-parts";
    rex.url = "https://pkgs.superlogical.com/rex/nix/snapshot.tar.gz";

    # Non-flake inputs
    infuse = {
      url = "git+https://codeberg.org/amjoseph/infuse.nix.git";
      flake = false;
    };

    # Nixpkgs branches
    master.url = "github:NixOS/nixpkgs/master";

    # Default Nixpkgs for packages and modules
    nixpkgs.follows = "master";

    # Minimize duplicate instances of inputs
    agenix.inputs.nixpkgs.follows = "nixpkgs";
    agenix.inputs.darwin.follows = "darwin";
    darwin.inputs.nixpkgs.follows = "nixpkgs";
    hjem.inputs.nixpkgs.follows = "nixpkgs";
    hjem-rum.inputs.nixpkgs.follows = "nixpkgs";
    hjem-rum.inputs.hjem.follows = "hjem";
    jovian.inputs.nixpkgs.follows = "nixpkgs";
    nix.inputs.nixpkgs.follows = "nixpkgs";
    nix.inputs.flake-parts.follows = "parts";
    nix-index-database.inputs.nixpkgs.follows = "nixpkgs";
  };
}
