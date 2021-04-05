{ home, inputs, master, stable, unstable, nixpkgs, nur, nvim-nightly, rust, ... }:

nixpkgs.lib.nixosSystem rec {
  system = "x86_64-linux";
  modules = [
    {
      nix = (import ../../config/nix-conf.nix { inherit inputs system nixpkgs; });
      nixpkgs = with nixpkgs.lib; let
        allowBroken = true;
        allowUnfree = true;
        config = { inherit allowBroken allowUnfree; };
        filterOverlays = k: v: v == "regular" && hasSuffix ".nix" k;
        importNixFiles = path: (lists.forEach (mapAttrsToList (name: _: path + ("/" + name))
          (filterAttrs filterOverlays (builtins.readDir path)))) import;
        userOverlays = importNixFiles ../../overlays;
        nixpkgsOverlays = _: _: {
          head = (import master { inherit config system; });
          unstable = (import unstable { inherit config system; });
          stable = (import stable { inherit config system; });
        };
        inputOverlays = _: _: {
          comma = import inputs.comma { pkgs = unstable.legacyPackages."${system}"; };
        };
      in
      {
        inherit config;
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
