{ inputs, inputs', system, nixpkgs }:

# Nix daemon settings that can't be put in `nixConfig`.
{
  buildMachines = nixpkgs.lib.optional (system == "aarch64-darwin" || system == "x86_64-darwin")
    {
      hostName = "192.168.1.9";
      system = "x86_64-linux";
      sshUser = "moni";
      sshKey = "/Users/moni/.ssh/id_ed25519";
      publicHostKey = "c3NoLWVkMjU1MTkgQUFBQUMzTnphQzFsWkRJMU5URTVBQUFBSUtyUGRxSWlUckdxbk42ZUFoUnVHbDlaVjJzVXovSVI4NVQzL1R6VVQ0T2wgcm9vdEBzdGFyY3J1aXNlcgo=";
      maxJobs = 6;
      speedFactor = 2;
      supportedFeatures = [ "nixos-test" "benchmark" "big-parallel" "kvm" ];
      mandatoryFeatures = [ ];
    };

  distributedBuilds = true;

  extraOptions = ''
    keep-outputs = true
    keep-derivations = true
    auto-allocate-uids = false
    builders-use-substitutes = true
    http-connections = 0
  ''
  +
  (nixpkgs.lib.optionalString (system == "aarch64-darwin") ''
    extra-platforms = aarch64-darwin x86_64-darwin
  '');

  nixPath = [ "nixpkgs=${nixpkgs}" ];
  package = inputs'.nix.packages.default;

  registry = {
    system.flake = inputs.self;
    default.flake = nixpkgs;
    home-manager.flake = inputs.home;
  };

  settings = {
    accept-flake-config = true;

    experimental-features = [
      "auto-allocate-uids"
      "ca-derivations"
      "flakes"
      "nix-command"
    ];

    max-jobs = "auto";

    # home-manager will attempt to rebuild the world otherwise...
    trusted-substituters = [
      "https://cache.nixos.org?priority=7"
      "https://mirrors.tuna.tsinghua.edu.cn/nix-channels/store/?priority=10"
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
