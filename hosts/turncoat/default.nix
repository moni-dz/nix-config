{ config, nixpkgs, agenix, nixos-wsl, overlays, inputs }:

# See https://github.com/NixOS/nixpkgs/blob/master/flake.nix#L24 for reference.
nixpkgs.lib.nixosSystem rec {
  system = "x86_64-linux";

  modules = [
    agenix.nixosModules.age
    nixos-wsl.nixosModules.wsl

    {
      # NOTE: you should either change this or disable it completely by commenting it out
      age = {
        identityPaths = [ "/home/zero/.ssh/id_ed25519" ];

        secrets.github-token = {
          file = ../../secrets/github-token.age;
          owner = "zero";
          mode = "0444";
        };
      };

      wsl = {
        enable = true;
        automountPath = "/mnt";
        defaultUser = "zero";
        startMenuLaunchers = true;
      };

      nix = import ../../nix-settings.nix { inherit inputs system nixpkgs; };
      nixpkgs = { inherit config overlays; };
    }

    ./configuration.nix
  ];

  specialArgs = { inherit inputs system; };
}