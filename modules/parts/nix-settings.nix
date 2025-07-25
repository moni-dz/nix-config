{
  lib,
  inputs,
  inputs',
  infuse,
  pkgs,
  ...
}:

{
  nixPath = [ "nixpkgs=${inputs.nixpkgs.outPath}" ];

  registry.nixpkgs.to = {
    type = "github";
    owner = "NixOS";
    repo = "nixpkgs";
    rev = inputs.nixpkgs.rev;
  };

  /*
  package =
    infuse
      (inputs'.nix.packages.default.appendPatches [

      ])
      {
        __output.doCheck.__assign = false;
      };
  */

  package = infuse (inputs'.nix.packages.default) {
    __output.doCheck.__assign = false;
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
      use-xdg-base-directories = true;

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
      ];

      trusted-public-keys = [
        "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
        "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
        "nixpkgs-wayland.cachix.org-1:3lwxaILxMRkVhehr5StQprHdEo4IrE8sRho9R9HOLYA="
      ];

      trusted-users = [
        "root"
        "moni"
      ];
    }

    (lib.mkIf (pkgs.stdenv.isDarwin && pkgs.stdenv.isAarch64) { extra-platforms = "x86_64-darwin"; })
    (lib.mkIf pkgs.stdenv.isDarwin { sandbox = "relaxed"; })
  ];
}
