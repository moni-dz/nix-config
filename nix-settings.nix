{ inputs, system, nixpkgs, max-jobs }:

# Nix daemon settings that can't be put in `nixConfig`.
{
  extraOptions = ''
    experimental-features = ca-derivations nix-command flakes
    keep-outputs = true
    keep-derivations = true
    http-connections = 0
  '';

  nixPath =
    let path = toString ./.;
    in
    [
      "nixpkgs=${nixpkgs}"
      "home-manager=${inputs.home}"
    ];

  package = inputs.master.legacyPackages.${system}.nix;

  registry = {
    system.flake = inputs.self;
    default.flake = nixpkgs;
    home-manager.flake = inputs.home;
  };

  settings = {
    accept-flake-config = true;
    inherit max-jobs;

    # home-manager will attempt to rebuild the world otherwise...
    trusted-substituters = [
      "https://cache.nixos.org?priority=10"
      "https://cache.ngi0.nixos.org/"
      "https://nix-community.cachix.org?priority=5"
      "https://nixpkgs-wayland.cachix.org"
      "https://fortuneteller2k.cachix.org"
    ];

    trusted-public-keys = [
      "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
      "cache.ngi0.nixos.org-1:KqH5CBLNSyX184S9BKZJo1LxrxJ9ltnY2uAs5c/f1MA="
      "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
      "nixpkgs-wayland.cachix.org-1:3lwxaILxMRkVhehr5StQprHdEo4IrE8sRho9R9HOLYA="
      "fortuneteller2k.cachix.org-1:kXXNkMV5yheEQwT0I4XYh1MaCSz+qg72k8XAi2PthJI="
    ];

    trusted-users = [ "root" "moni" "zero" ];
  };
}
