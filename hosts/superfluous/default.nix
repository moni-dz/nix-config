{ config, nixpkgs, agenix, overlays, inputs }:

# See https://github.com/NixOS/nixpkgs/blob/master/flake.nix#L24 for reference.
nixpkgs.lib.nixosSystem rec {
  system = "x86_64-linux";

  modules = [
    agenix.nixosModules.age

    {
      # NOTE: you should either change this or disable it completely by commenting it out
      age.secrets.github-token = {
        file = ../../secrets/github-token.age;
        owner = "moni";
        mode = "0444";
      };

      nix = import ../../nix-settings.nix {
        inherit inputs system nixpkgs;
        max-jobs = 4;
      };

      nixpkgs = { inherit config overlays; };
      networking.hostName = "superfluous";

      /*
        NOTE: DO NOT CHANGE THIS IF YOU DON'T KNOW WHAT YOU'RE DOING.
        Only change this if you are ABSOLUTELY 100% SURE that you don't have stateful data.
      */
      system.stateVersion = "21.05";
    }

    ./configuration.nix
  ];

  specialArgs = { inherit inputs system; };
}
