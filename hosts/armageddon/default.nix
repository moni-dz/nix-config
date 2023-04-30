{ config, darwin, overlays, nixpkgs, inputs }:

let
  system = "aarch64-darwin";
in
darwin.lib.darwinSystem {
  inherit inputs system;

  modules = [
    inputs.agenix.darwinModules.default
    inputs.home.darwinModules.default
    inputs.shyim.darwinModules.default

    {
      # NOTE: you should either change this or disable it completely by commenting it out
      age = {
        identityPaths = [ "/Users/moni/.ssh/id_ed25519" ];

        secrets.github-token = {
          file = ../../secrets/github-token.age;
          owner = "moni";
          group = "staff";
          mode = "600";
        };
      };

      nix = import ../../nix-settings.nix {
        inherit inputs system nixpkgs;
      };

      nixpkgs = { inherit config overlays; };

      home-manager = {
        useGlobalPkgs = true;
        useUserPackages = true;
        users.moni = import ../../users/moni/home.nix;
        extraSpecialArgs = { inherit inputs system; };
      };
    }

    ./configuration.nix
  ];

  specialArgs = { inherit system; };
}
