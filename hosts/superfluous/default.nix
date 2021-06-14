{ config, agenix, home, inputs, nixpkgs, overlays, ... }:

nixpkgs.lib.nixosSystem rec {
  system = "x86_64-linux";

  modules = [
    agenix.nixosModules.age
    home.nixosModules.home-manager
    nixpkgs.nixosModules.notDetected

    {
      # NOTE: you should either change this or disable it completely by commenting it out
      age.secrets.github-token = {
        file = ../../secrets/github-token.age;
        owner = "fortuneteller2k";
        mode = "0444";
      };

      home-manager = {
        useGlobalPkgs = true;
        useUserPackages = true;
        users.fortuneteller2k = import ../../users/fortuneteller2k;
      };

      nix = import ../../config/nix-conf.nix { inherit inputs system nixpkgs; };
      nixpkgs = { inherit config overlays; };
    }

    ./configuration.nix
  ];

  specialArgs = { inherit inputs; };
}
