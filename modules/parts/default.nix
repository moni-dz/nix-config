{ self, inputs, ... }:

{
  imports = [
    ./home-manager.nix
    ./nix-darwin.nix
    ./nixos.nix
  ];

  perSystem =
    {
      lib,
      pkgs,
      system,
      self',
      inputs',
      ...
    }:
    {
      _module.args = rec {
        # nix the package manager configuration
        nix = import ./nix-settings.nix {
          inherit lib inputs inputs';
          inherit (pkgs) stdenv;
        };

        # nixpkgs configuration (not the flake input)
        nixpkgs = {
          config = lib.mkForce {
            allowBroken = true;
            allowUnfree = true;
            tarball-ttl = 0;

            # Experimental options, disable if you don't know what you are doing!
            contentAddressedByDefault = false;
          };

          hostPlatform = system;
          overlays = [ self.overlays.default ];
        };

        # Extra arguments passed to the module system for nix-darwin, NixOS, and home-manager
        extraModuleArgs = {
          inherit self' inputs' inputs system;
        };

        # NixOS and nix-darwin base environment.systemPackages
        basePackagesFor =
          pkgs:
          __attrValues {
            inherit (pkgs)
              nano
              curl
              fd
              ripgrep
              man-pages-posix
              wget
              git
              subversion
              ;

            home-manager = inputs'.home.packages.home-manager.override { path = "${inputs.home}"; };

            man-pages = if pkgs.stdenv.isLinux then pkgs.man-pages else self'.packages.man-pages-xnu;

            gnu-coreutils = if pkgs.stdenv.isLinux then pkgs.coreutils else pkgs.coreutils-prefixed;
          };
      };
    };
}
