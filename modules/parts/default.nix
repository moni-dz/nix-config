{ self, inputs, ... }:

{
  imports = [
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
      _module.args =
        let
          # infuse.nix (https://codeberg.org/amjoseph/infuse.nix)
          inherit ((import "${inputs.infuse.outPath}/default.nix" { inherit (inputs.nixpkgs) lib; }).v1)
            infuse
            ;
        in
        {
          inherit infuse;

          # nix the package manager configuration
          nix = import ../base/nix-settings.nix {
            inherit
              lib
              inputs
              inputs'
              infuse
              pkgs
              ;
          };

          # nixpkgs configuration (not the flake input)
          nixpkgs = {
            config = lib.mkForce {
              allowBroken = true;
              allowUnfree = true;

              # Experimental options, disable if you don't know what you are doing!
              contentAddressedByDefault = false;
            };

            hostPlatform = system;
            overlays = [ self.overlays.default ];
          };

          # Extra arguments passed to the module system for nix-darwin, NixOS, and home-manager
          extraModuleArgs = {
            inherit
              self'
              infuse
              inputs'
              inputs
              system
              ;
          };

          # NixOS and nix-darwin base environment.systemPackages
          basePackagesFor =
            pkgs:
            __attrValues {
              inherit (pkgs)
                msedit
                curl
                fd
                ripgrep
                man-pages-posix
                wget
                git
                subversion
                ;

              gnu-coreutils =
                if pkgs.stdenv.hostPlatform.isLinux then pkgs.coreutils else pkgs.coreutils-prefixed;
            }
            ++ lib.optional pkgs.stdenv.hostPlatform.isLinux pkgs.man-pages;
        };
    };
}
