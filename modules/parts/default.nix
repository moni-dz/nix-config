{ self, inputs, ... }:

{
  imports = [
    ./home-manager.nix
    ./nix-darwin.nix
    ./nixos.nix
  ];

  perSystem = { lib, pkgs, system, inputs', ... }: {
    _module.args = rec {
      # nix the package manager configuration
      nix = import ./nix-settings.nix {
        inherit lib inputs inputs';
        inherit (pkgs) stdenv;
      };

      # nixpkgs configuration (not the flake input)
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

      # Extra arguments passed to the module system for nix-darwin, NixOS, and home-manager
      extraModuleArgs = {
        inherit inputs' system;
        inputs = lib.mkForce inputs;

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

      # NixOS and nix-darwin base environment.systemPackages
      basePackagesFor = pkgs: __attrValues {
        inherit (pkgs)
          nano
          curl
          fd
          home-manager
          man-pages
          man-pages-posix
          ripgrep
          wget;

        gnu-coreutils = if pkgs.stdenv.isLinux then pkgs.coreutils else pkgs.coreutils-prefixed;
        git = pkgs.git.overrideAttrs { __contentAddressed = true; };
        svn = pkgs.subversion.overrideAttrs { __contentAddressed = true; };
      };
    };

    formatter = inputs.nixpkgs-fmt.defaultPackage.${system};
  };
}
