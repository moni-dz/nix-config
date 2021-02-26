{ emacs, home, inputs, fork, master, stable, unstable, nvim-nightly, rust }:

let
  nixpkgs = unstable;
in
nixpkgs.lib.nixosSystem rec {
  system = "x86_64-linux";
  modules = [
    {
      nix = (import ../../config/nix-conf.nix { inherit inputs system; });
      nixpkgs =
        let
          filterOverlays = k: v: with unstable.lib; v == "regular" && hasSuffix ".nix" k;
          userOverlays = with unstable.lib; (lists.forEach (mapAttrsToList
            (name: _: ../../overlays + ("/" + name))
            (filterAttrs filterOverlays (builtins.readDir ../../overlays)))) import;
          nixpkgs-overlays = _: _: {
            fork = fork.legacyPackages.${system};
            master = master.legacyPackages.${system};
            stable = stable.legacyPackages.${system};
          };
        in
        {
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
