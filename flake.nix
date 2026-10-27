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
        ./modules
        ./secrets/module.nix
        ./hosts/riscake
        ./hosts/hands
        ./hosts/hyper-v
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
    agenix-rekey.url = "github:oddlama/agenix-rekey";
    darwin.url = "github:nix-darwin/nix-darwin";
    determinate.url = "https://flakehub.com/f/DeterminateSystems/determinate/3";
    dix.url = "github:manic-systems/dix";
    hjem.url = "github:feel-co/hjem";
    hjem-rum.url = "github:snugnug/hjem-rum";
    inshellah.url = "github:manic-systems/inshellah";
    jovian.url = "github:Jovian-Experiments/Jovian-NixOS/6ba37ebb7c68b1b6711027a67122aa41060d216e";
    nix.url = "github:NixOS/nix/master";
    nix-index-database.url = "github:Mic92/nix-index-database";
    nix-src.url = "github:moni-dz/nix-src/all-mine";
    parts.url = "github:hercules-ci/flake-parts";
    rex.url = "https://pkgs.superlogical.com/rex/nix/snapshot.tar.gz";

    # Non-flake inputs
    infuse = {
      url = "https://codeberg.org/amjoseph/infuse.nix/archive/364ea18b5611b5fd6a6acd7151411b430a70e194.tar.gz";
      flake = false;
    };

    # Nixpkgs branches
    master.url = "github:NixOS/nixpkgs/master";

    # Default Nixpkgs for packages and modules
    nixpkgs.follows = "master";

    # Minimize duplicate instances of inputs
    agenix.inputs.nixpkgs.follows = "nixpkgs";
    agenix-rekey.inputs.nixpkgs.follows = "nixpkgs";
    agenix-rekey.inputs.flake-parts.follows = "parts";
    darwin.inputs.nixpkgs.follows = "nixpkgs";
    dix.inputs.nixpkgs.follows = "nixpkgs";
    hjem.inputs.nixpkgs.follows = "nixpkgs";
    hjem-rum.inputs.nixpkgs.follows = "nixpkgs";
    hjem-rum.inputs.hjem.follows = "hjem";
    jovian.inputs.nixpkgs.follows = "nixpkgs";
    nix.inputs.nixpkgs.follows = "nixpkgs";
    nix.inputs.flake-parts.follows = "parts";
    nix-index-database.inputs.nixpkgs.follows = "nixpkgs";
    nix-src.inputs.flake-parts.follows = "parts";
    rex.inputs.nixpkgs.follows = "nixpkgs";
  };
}
