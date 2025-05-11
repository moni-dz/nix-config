{
  lib,
  stdenv,
  inputs,
  inputs',
  infuse,
  ...
}:

{
  nixPath = [ "nixpkgs=${inputs.nixpkgs}" ];

  package =
    infuse
      (inputs'.nix.packages.default.appendPatches [
        ../../packages/patches/improve-build-failure-error.patch
        ../../packages/patches/lazy-trees-v2.patch
      ])
      {
        __output.doCheck.__assign = false;
      };

  registry = {
    system.flake = inputs.self;
    nixpkgs.flake = inputs.nixpkgs;
    home.flake = inputs.home;
  };

  settings = lib.mkMerge [
    {
      keep-outputs = true;
      keep-derivations = true;
      keep-going = true;
      lazy-trees = true;
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
        "dynamic-derivations"
        "flakes"
        "nix-command"
        "pipe-operators"
      ];

      max-jobs = "auto";

      trusted-substituters = [
        "https://mirrors.tuna.tsinghua.edu.cn/nix-channels/store?priority=10"
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

      trusted-users = [
        "root"
        "moni"
        "zero"
      ];

      use-xdg-base-directories = true;
    }

    (lib.mkIf (stdenv.isDarwin && stdenv.isAarch64) { extra-platforms = "x86_64-darwin"; })
    (lib.mkIf stdenv.isDarwin { sandbox = "relaxed"; })
  ];
}
