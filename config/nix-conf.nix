{
  extraOptions = "experimental-features = nix-command flakes";
  nixPath = let path = toString ./.; in [ "repl=${path}/config/repl.nix" "nixpkgs=${inputs.nixpkgs}" ];
  gc = {
    automatic = true;
    dates = "weekly";
    options = "--delete-older-than 7d";
  };
  maxJobs = 4;
  autoOptimiseStore = true;
  daemonNiceLevel = 15;
  daemonIONiceLevel = 5;
  binaryCachePublicKeys = [
    "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
    "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
    "nixpkgs-wayland.cachix.org-1:3lwxaILxMRkVhehr5StQprHdEo4IrE8sRho9R9HOLYA="
  ];
  binaryCaches = [
    "https://cache.nixos.org"
    "https://nix-community.cachix.org"
    "https://nixpkgs-wayland.cachix.org"
  ];
}
