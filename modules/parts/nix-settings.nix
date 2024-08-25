{ lib, stdenv, inputs, inputs' }:

# Nix daemon settings that can't be put in `nixConfig`.
{
  buildMachines = __attrValues {
    mistral = {
      hostName = "mistral";
      system = "x86_64-linux";
      sshUser = "moni";
      sshKey = "/Users/moni/.ssh/id_ed25519";
      publicHostKey = "c3NoLWVkMjU1MTkgQUFBQUMzTnphQzFsWkRJMU5URTVBQUFBSUI4OElSOXl0QnJCWlRycVpja0p0b1N2OVR6d1hNdDVQMm85RlcvVjNQd1Ygcm9vdEBtaXN0cmFsCg==";
      maxJobs = 2;
      speedFactor = 1;
      supportedFeatures = [ "nixos-test" "benchmark" "big-parallel" "kvm" ];
      mandatoryFeatures = [ ];
    };
  };

  distributedBuilds = true;

  extraOptions = ''
    keep-outputs = true
    keep-derivations = true
    auto-allocate-uids = true
    builders-use-substitutes = true
    http-connections = 0
  '';

  nixPath = [ "nixpkgs=${inputs.nixpkgs}" ];

  /*
  package = inputs'.nix-parallel.packages.default.override {
    pname = "nix-parallel";
    doCheck = false;
    doInstallCheck = false;
    installUnitTests = false;
  };
  */

  package = inputs'.lix.packages.default.overrideAttrs { doCheck = false; };

  registry = {
    system.flake = inputs.self;
    default.flake = inputs.nixpkgs;
    home-manager.flake = inputs.home;
  };

  settings = lib.mkMerge [
    {
      accept-flake-config = true;
      flake-registry = __toFile "begone-evil.json" (__toJSON { flakes = [ ]; version = 2; });

      experimental-features = [
        "auto-allocate-uids"
        "ca-derivations"
        #"configurable-impure-env"
        "dynamic-derivations"
        "flakes"
        "nix-command"
      ];

      max-jobs = "auto";

      trusted-substituters = [
        "https://mirrors.tuna.tsinghua.edu.cn/nix-channels/store?priority=10"
        "https://mirror.sjtu.edu.cn/nix-channels/store?priority=10"
        "https://mirrors.ustc.edu.cn/nix-channels/store?priority=15"
        "https://mirrors.cernet.edu.cn/nix-channels/store?priority=15"
        "https://mirrors.cqupt.edu.cn/nix-channels/store?priority=15"
        "https://mirror.iscas.ac.cn/nix-channels/store?priority=15"
        "https://mirror.nju.edu.cn/nix-channels/store?priority=15"
        "https://mirrors4.sau.edu.cn/nix-channels/store?priority=15"
        "https://nix-mirror.freetls.fastly.net?priority=11"
        "https://cache.nixos.org?priority=12"
        "https://nix-community.cachix.org?priority=13"
        "https://nixpkgs-wayland.cachix.org"
        "https://cache.lix.systems"
      ];

      trusted-public-keys = [
        "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
        "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
        "nixpkgs-wayland.cachix.org-1:3lwxaILxMRkVhehr5StQprHdEo4IrE8sRho9R9HOLYA="
        "cache.lix.systems:aBnZUw8zA7H35Cz2RyKFVs3H4PlGTLawyY5KRbvJR8o="
      ];

      trusted-users = [ "root" "moni" "zero" ];

      use-xdg-base-directories = true;
    }

    (lib.mkIf (stdenv.isDarwin && stdenv.isAarch64) { extra-platforms = "x86_64-darwin"; })
  ];
}
