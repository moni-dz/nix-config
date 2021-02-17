{ inputs, system }:

{
  autoOptimiseStore = true;
  binaryCachePublicKeys = [
    "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
    "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
  ];
  binaryCaches = [
    "https://cache.nixos.org"
    "https://nix-community.cachix.org"
  ];
  daemonNiceLevel = 15;
  daemonIONiceLevel = 5;
  extraOptions = "experimental-features = nix-command flakes";
  gc = {
    automatic = true;
    dates = "weekly";
    options = "--delete-older-than 7d";
  };
  maxJobs = 4;
  package = inputs.nixpkgs-master.legacyPackages."${system}".nixFlakes;
}
