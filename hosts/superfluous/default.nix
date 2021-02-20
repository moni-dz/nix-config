{ emacs, home, nixpkgs, nixpkgs-fork, nixpkgs-master, nvim-nightly, rust, inputs }:

nixpkgs.lib.nixosSystem rec {
  system = "x86_64-linux";
  modules = [
    {
      nix = (import ../../config/nix-conf.nix { inherit inputs system; });
      nixpkgs = let
      filterOverlays = k: v: with nixpkgs.lib; v == "regular" && hasSuffix ".nix" k;
      userOverlays = with nixpkgs.lib; (lists.forEach (mapAttrsToList
      (name: _: ../../overlays + ("/" + name))
      (filterAttrs filterOverlays (builtins.readDir ../../overlays)))) import;
      nixpkgs-overlays = final: prev: {
        master = nixpkgs-master.legacyPackages.${system};
        fork = nixpkgs-fork.legacyPackages.${system};
      };
      in {
        config = {
          allowUnfree = true;
          allowBroken = true;
        };
        overlays = [
          emacs.overlay
          nvim-nightly.overlay
          rust.overlay
          nixpkgs-overlays
        ] ++ userOverlays;
      };
    }
    ./configuration.nix
    home.nixosModules.home-manager {
      home-manager = {
        useGlobalPkgs = true;
        useUserPackages = true;
        users.fortuneteller2k = import ../../home/fortuneteller2k.nix;
      };
    }
    nixpkgs.nixosModules.notDetected
  ];
  specialArgs = { inherit inputs; };
}
