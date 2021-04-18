{ home, inputs, master, stable, staging, staging-next, unstable, nixpkgs, ... }:

nixpkgs.lib.nixosSystem rec {
  system = "x86_64-linux";

  modules = [
    {
      nix = import ../../config/nix-conf.nix { inherit inputs system nixpkgs; };

      nixpkgs = with nixpkgs.lib; let
        config = {
          allowBroken = true;
          allowUnfree = true;
        };

        filterNixFiles = k: v: v == "regular" && hasSuffix ".nix" k;

        importNixFiles = path: (lists.forEach (mapAttrsToList (name: _: path + ("/" + name))
          (filterAttrs filterNixFiles (builtins.readDir path)))) import;

        userOverlays = importNixFiles ../../overlays;

        nixpkgsOverlays = _: _: {
          head = import master { inherit config system; };
          unstable = import unstable { inherit config system; };
          stable = import stable { inherit config system; };
          staging = import staging { inherit config system; };
          staging-next = import staging-next { inherit config system; };
        };

        inputOverlays = _: _: {
          comma = import inputs.comma { pkgs = unstable.legacyPackages."${system}"; };
        };
      in
      {
        inherit config;

        overlays = [
          nixpkgsOverlays
          inputs.emacs.overlay
          inputs.nvim-nightly.overlay
          inputs.nur.overlay
          inputs.rust.overlay
          inputOverlays
        ] ++ userOverlays;
      };
    }
    ./configuration.nix
    home.nixosModules.home-manager
    {
      home-manager = {
        useGlobalPkgs = true;
        useUserPackages = true;
        users.fortuneteller2k = import ../../users/fortuneteller2k;
      };
    }
    nixpkgs.nixosModules.notDetected
  ];

  specialArgs = { inherit inputs; };
}
