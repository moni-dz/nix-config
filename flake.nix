{
  description = "A somewhat huge NixOS/nix-darwin/home-manager configuration using Nix Flakes.";

  outputs = { self, home, darwin, nixpkgs, ... }@inputs:
    let
      config = {
        allowBroken = true;
        allowUnfree = true;
        allowUnfreePredicate = _: true;
        tarball-ttl = 0;

        # WTF: don't do this kids...
        # replaceStdenv = { pkgs }: pkgs.optimizedV3Stdenv;

        /*
          NOTE: experimental option, disable if you don't know what this does

          See https://github.com/NixOS/rfcs/pull/62 for more information.
        */
        contentAddressedByDefault = false;
      };

      importNixFiles = path: with nixpkgs.lib; map import (__filter (hasSuffix "nix") (filesystem.listFilesRecursive path));

      overlays = with inputs; [
        emacs.overlay
        inputs.nixpkgs-f2k.overlays.stdenvs
      ]
      # Overlays from ./overlays directory
      ++ (importNixFiles ./overlays);

      configFrom =
        let
          pkgsFrom = branch: system: import branch { inherit config system; };
        in
        path: system: import path {
          inherit config system nixpkgs home darwin overlays inputs;

          /*
            Nixpkgs branches, replace when https://github.com/NixOS/nixpkgs/pull/160061 is live.

            One can access these branches like so:

            `stable.mpd'
            `master.linuxPackages_xanmod'
          */
          master = pkgsFrom inputs.master system;
          unstable = pkgsFrom inputs.unstable system;
          stable = pkgsFrom inputs.stable system;
        };
    in
    {
      darwinConfigurations.shaker = configFrom ./hosts/shaker "aarch64-darwin";

      nixosConfigurations = {
        starcruiser = configFrom ./hosts/starcruiser "x86_64-linux";
        turncoat = configFrom ./hosts/turncoat "x86_64-linux";
      };

      homeConfigurations = {
        moni = configFrom ./users/moni "aarch64-darwin";
        omni = configFrom ./users/omni "x86_64-linux";
        zero = configFrom ./users/zero "x86_64-linux";
      };

      # Default formatter for the entire repo
      formatter = nixpkgs.lib.genAttrs [ "x86_64-linux" "aarch64-darwin" ] (system: inputs.nixpkgs-fmt.defaultPackage.${system});
    };

  nixConfig = {
    commit-lockfile-summary = "flake: bump inputs";

    substituters = [
      "https://cache.nixos.org?priority=10"
      "https://cache.ngi0.nixos.org/"
      "https://nix-community.cachix.org?priority=5"
      "https://nixpkgs-wayland.cachix.org"
      "https://fortuneteller2k.cachix.org"
    ];
  };

  inputs = {
    # Flake inputs
    agenix.url = "github:ryantm/agenix";
    emacs.url = "github:nix-community/emacs-overlay";
    darwin.url = "github:lnl7/nix-darwin/master";
    home.url = "github:nix-community/home-manager";
    nix.url = "github:nixos/nix";
    nix-colors.url = "github:Misterio77/nix-colors";
    nixos-wsl.url = "github:nix-community/nixos-wsl";
    nixpkgs-f2k.url = "github:fortuneteller2k/nixpkgs-f2k";
    nixpkgs-fmt.url = "github:nix-community/nixpkgs-fmt";
    nixpkgs-wayland.url = "github:nix-community/nixpkgs-wayland";
    statix.url = "github:nerdypepper/statix";

    # Nixpkgs branches
    master.url = "github:nixos/nixpkgs/master";
    stable.url = "github:nixos/nixpkgs/nixos-21.11";
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
    nixos-wsl.inputs.nixpkgs.follows = "nixpkgs";
    nixpkgs-f2k.inputs.nixpkgs.follows = "nixpkgs";
    statix.inputs.nixpkgs.follows = "nixpkgs";
  };
}
