{ home, inputs, fork, master, stable, unstable, nixpkgs, nur, nvim-nightly, rust, ... }:

nixpkgs.lib.nixosSystem rec {
  system = "x86_64-linux";
  modules = [
    {
      nix = (import ../../config/nix-conf.nix { inherit inputs system; });
      nixpkgs = with nixpkgs.lib; let
        filterOverlays = k: v: v == "regular" && hasSuffix ".nix" k;
        getOverlays = path: (lists.forEach (mapAttrsToList (name: _: path + ("/" + name))
          (filterAttrs filterOverlays (builtins.readDir path)))) import;
        userOverlays = getOverlays ../../overlays;
        nixpkgsOverlays = _: _: {
          fork = fork.legacyPackages.${system};
          head = master.legacyPackages.${system};
          unstable = unstable.legacyPackages.${system};
          stable = stable.legacyPackages.${system};
        };
        inputOverlays = _: _: {
          comma = import inputs.comma { pkgs = unstable.legacyPackages."${system}"; };
        };
      in
      {
        config = {
          allowUnfree = true;
          allowBroken = true;
        };
        overlays = [
          nixpkgsOverlays
          nvim-nightly.overlay
          nur.overlay
          rust.overlay
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
        users.fortuneteller2k = import ../../home/fortuneteller2k.nix;
      };
    }
    nixpkgs.nixosModules.notDetected
  ];
  specialArgs = { inherit inputs; };
}
