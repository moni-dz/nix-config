{ inputs, ... }:

{
  imports = [
    ./home-manager.nix
    ./nix-darwin.nix
    ./nixos.nix
  ];

  perSystem = { lib, pkgs, system, ... }: {
    _module.args =
      let
        nixpkgs-config = {
          allowBroken = true;
          allowUnfree = true;
          allowUnfreePredicate = _: true;
          tarball-ttl = 0;

          # Experimental options, disable if you don't know what you are doing!
          contentAddressedByDefault = false;
        };

        pkgsFrom = branch: system: import branch {
          inherit system;
          config = nixpkgs-config;
        };

        pipe' = lib.flip lib.pipe;

        importFilesRecursive = pipe' [
          lib.filesystem.listFilesRecursive
          (__filter (lib.hasSuffix "nix"))
          (map import)
        ];
      in
      {
        inherit nixpkgs-config;

        lib = lib.extend (_: _: {
          inherit pipe' importFilesRecursive;
        });

        overlays = with inputs; [ emacs.overlay nixpkgs-f2k.overlays.stdenvs ]
          ++ (importFilesRecursive ./../../overlays); # Overlays from the `overlays` directory

        /*
          One can access these nixpkgs branches like so:

          `stable.mpd'
          `master.linuxPackages_xanmod'
        */
        master = pkgsFrom inputs.master system;
        unstable = pkgsFrom inputs.unstable system;
        stable = pkgsFrom inputs.stable system;
      };

    formatter = inputs.nixpkgs-fmt.defaultPackage.${system};
  };
}
