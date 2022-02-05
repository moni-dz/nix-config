{ inputs, system, nixpkgs }:

{
  daemonCPUSchedPolicy = "idle";
  daemonIOSchedClass = "idle";
  daemonIOSchedPriority = 5;

  extraOptions = ''
    experimental-features = ca-derivations nix-command flakes
    keep-outputs = true
    keep-derivations = true
  '';

  gc = {
    automatic = true;
    dates = "weekly";
    options = "--delete-older-than 7d";
  };

  nixPath =
    let path = toString ./.;
    in
    [
      "repl=${path}/repl.nix"
      "nixpkgs=${nixpkgs}"
      "home-manager=${inputs.home}"
    ];

  optimise = {
    automatic = true;
    dates = [ "03:00" ];
  };

  package = nixpkgs.legacyPackages."${system}".nixUnstable;

  registry = {
    system.flake = inputs.self;
    default.flake = nixpkgs;
    home-manager.flake = inputs.home;
  };

  settings = rec {
    max-jobs = 4;

    substituters = [
      "https://cache.nixos.org?priority=10"
      "https://cache.ngi0.nixos.org/"
      "https://mjlbach.cachix.org"
      "https://nix-community.cachix.org"
      "https://nixpkgs-wayland.cachix.org"
      "https://fortuneteller2k.cachix.org"
    ];

    trusted-public-keys = [
      "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
      "cache.ngi0.nixos.org-1:KqH5CBLNSyX184S9BKZJo1LxrxJ9ltnY2uAs5c/f1MA="
      "mjlbach.cachix.org-1:dR0V90mvaPbXuYria5mXvnDtFibKYqYc2gtl9MWSkqI="
      "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
      "nixpkgs-wayland.cachix.org-1:3lwxaILxMRkVhehr5StQprHdEo4IrE8sRho9R9HOLYA="
      "fortuneteller2k.cachix.org-1:kXXNkMV5yheEQwT0I4XYh1MaCSz+qg72k8XAi2PthJI="
    ];

    trusted-substituters = substituters;
    trusted-users = [ "root" "fortuneteller2k" ];
  };
}
