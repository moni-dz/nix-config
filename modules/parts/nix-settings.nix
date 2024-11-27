{
  lib,
  stdenv,
  inputs,
  inputs',
}:

{
  buildMachines = __attrValues {
    mistral = {
      hostName = "mistral";
      system = "x86_64-linux";
      sshUser = "moni";
      sshKey = "/Users/moni/.ssh/id_ed25519";
      publicHostKey = "c3NoLWVkMjU1MTkgQUFBQUMzTnphQzFsWkRJMU5URTVBQUFBSUZEUkd5RFFsSFBvZ1lJdDBJSXdJKy8xRCtVM3FiT0hPWk95UHNBTjJOV3Qgcm9vdEB2bWkxOTk4NTU4Cg==";
      maxJobs = 2;
      speedFactor = 1;
      supportedFeatures = [
        "nixos-test"
        "benchmark"
        "big-parallel"
        "kvm"
      ];
      mandatoryFeatures = [ ];
    };
  };

  distributedBuilds = true;
  nixPath = [ "nixpkgs=${inputs.nixpkgs}" ];

  registry = {
    system.flake = inputs.self;
    nixpkgs.flake = inputs.nixpkgs;
    home.flake = inputs.home;
  };

  settings = lib.mkMerge [
    {
      auto-allocate-uids = true;
      keep-outputs = true;
      keep-derivations = true;
      keep-going = true;
      builders-use-substitutes = true;
      allow-unsafe-native-code-during-evaluation = true;
      accept-flake-config = true;
      http-connections = 0;

      flake-registry = __toFile "begone-evil.json" (__toJSON {
        flakes = [ ];
        version = 2;
      });

      experimental-features = [
        "auto-allocate-uids"
        "ca-derivations"
        # "configurable-impure-env"
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
        "https://cache.flakehub.com"
      ];

      trusted-public-keys = [
        "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
        "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
        "nixpkgs-wayland.cachix.org-1:3lwxaILxMRkVhehr5StQprHdEo4IrE8sRho9R9HOLYA="
        "cache.lix.systems:aBnZUw8zA7H35Cz2RyKFVs3H4PlGTLawyY5KRbvJR8o="
        "cache.flakehub.com-3:hJuILl5sVK4iKm86JzgdXW12Y2Hwd5G07qKtHTOcDCM="
        "cache.flakehub.com-4:Asi8qIv291s0aYLyH6IOnr5Kf6+OF14WVjkE6t3xMio="
        "cache.flakehub.com-5:zB96CRlL7tiPtzA9/WKyPkp3A2vqxqgdgyTVNGShPDU="
        "cache.flakehub.com-6:W4EGFwAGgBj3he7c5fNh9NkOXw0PUVaxygCVKeuvaqU="
        "cache.flakehub.com-7:mvxJ2DZVHn/kRxlIaxYNMuDG1OvMckZu32um1TadOR8="
        "cache.flakehub.com-8:moO+OVS0mnTjBTcOUh2kYLQEd59ExzyoW1QgQ8XAARQ="
        "cache.flakehub.com-9:wChaSeTI6TeCuV/Sg2513ZIM9i0qJaYsF+lZCXg0J6o="
        "cache.flakehub.com-10:2GqeNlIp6AKp4EF2MVbE1kBOp9iBSyo0UPR9KoR0o1Y="
      ];

      trusted-users = [
        "root"
        "moni"
        "zero"
      ];

      use-xdg-base-directories = true;
      ssl-cert-file = "/etc/nix/macos-keychain.crt";
      netrc-file = "/nix/var/determinate/netrc";
      post-build-hook = "/nix/var/determinate/post-build-hook.sh";
    }

    (lib.mkIf (stdenv.isDarwin && stdenv.isAarch64) { extra-platforms = "x86_64-darwin"; })
    (lib.mkIf stdenv.isDarwin { sandbox = "relaxed"; })
  ];
}
