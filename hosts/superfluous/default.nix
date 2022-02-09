{ config, agenix, inputs, nixpkgs, overlays, ... }:

# See https://github.com/NixOS/nixpkgs/blob/master/flake.nix#L24 for reference.
nixpkgs.lib.nixosSystem rec {
  system = "x86_64-linux";

  modules = [
    agenix.nixosModules.age

    {
      # NOTE: you should either change this or disable it completely by commenting it out
      age.secrets.github-token = {
        file = ../../secrets/github-token.age;
        owner = "fortuneteller2k";
        mode = "0444";
      };

      nix = import ../../nix-settings.nix { inherit inputs system nixpkgs; };
      nixpkgs = { inherit config overlays; };
    }

    ./configuration.nix
  ];

  specialArgs = { inherit inputs; };
}
