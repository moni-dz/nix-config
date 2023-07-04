{ self, inputs, ... }:

{
  imports = [
    ./home-manager.nix
    ./nix-darwin.nix
    ./nixos.nix
  ];

  perSystem = { lib, pkgs, system, ... }: {
    _module.args = rec {
      # nixpkgs configuration
      nixpkgs = {
        config = {
          allowBroken = true;
          allowUnfree = true;
          allowUnfreePredicate = _: true;
          tarball-ttl = 0;

          # Experimental options, disable if you don't know what you are doing!
          contentAddressedByDefault = false;
        };

        hostPlatform = system;

        overlays = [
          inputs.emacs.overlay
          inputs.nixpkgs-f2k.overlays.stdenvs
          self.overlays.default
        ];
      };

      /*
        One can access these nixpkgs branches like so:

        `branches.stable.mpd'
        `branches.master.linuxPackages_xanmod'
      */
      branches =
        let
          pkgsFrom = branch: system: import branch {
            inherit system;
            inherit (nixpkgs) config overlays;
          };
        in
        {
          master = pkgsFrom inputs.master system;
          unstable = pkgsFrom inputs.unstable system;
          stable = pkgsFrom inputs.stable system;
        };
    };

    formatter = inputs.nixpkgs-fmt.defaultPackage.${system};
  };
}
