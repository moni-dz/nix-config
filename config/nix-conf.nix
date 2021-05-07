{ inputs, system, nixpkgs }:

rec {
  autoOptimiseStore = true;

  binaryCaches = [
    "https://cache.nixos.org"
    "https://nix-community.cachix.org"
    "https://fortuneteller2k.cachix.org"
  ];

  binaryCachePublicKeys = [
    "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
    "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
    "fortuneteller2k.cachix.org-1:kXXNkMV5yheEQwT0I4XYh1MaCSz+qg72k8XAi2PthJI="
  ];

  daemonNiceLevel = 1;
  daemonIONiceLevel = 1;

  extraOptions = ''
    experimental-features = ca-references ca-derivations nix-command flakes
    keep-outputs = true
    keep-derivations = true
  '';

  gc = {
    automatic = true;
    dates = "weekly";
    options = "--delete-older-than 7d";
  };

  maxJobs = 4;

  nixPath = let path = toString ../.; in
    [
      "repl=${path}/repl.nix"
      "nixpkgs=${nixpkgs}"
      "home-manager=${inputs.home}"
    ];

  package = nixpkgs.legacyPackages."${system}".nixFlakes;

  registry = {
    system.flake = inputs.self;
    nixpkgs.flake = nixpkgs;
    default.flake = nixpkgs;
    home-manager.flake = inputs.home;
  };

  trustedBinaryCaches = binaryCaches;
  trustedUsers = [ "root" "fortuneteller2k" ];
}
