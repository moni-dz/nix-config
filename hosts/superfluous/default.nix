{ config, agenix, home, inputs, nixpkgs, input-overlays, nixpkgs-overlays, user-overlays, ... }:

nixpkgs.lib.nixosSystem rec {
  system = "x86_64-linux";

  modules = [
    agenix.nixosModules.age
    home.nixosModules.home-manager
    nixpkgs.nixosModules.notDetected

    {
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

      nixpkgs = {
        inherit config;

        overlays = with inputs; [
          nixpkgs-overlays
          emacs.overlay
          nur.overlay
          rust.overlay
          input-overlays
        ] ++ user-overlays;
      };
    }

    ./configuration.nix
  ];

  specialArgs = { inherit inputs; };
}
