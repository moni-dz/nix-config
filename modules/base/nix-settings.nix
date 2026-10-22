{
  lib,
  inputs,
  inputs',
  infuse,
  pkgs,
  ...
}:

{
  package = lib.mkDefault (
    infuse inputs'.nix.packages.default {
      __output.doCheck.__assign = false;
    }
  );

  # Determinate Nix manages the Nix path itself, so this is only used on hosts without it
  nixPath = [ "nixpkgs=${inputs.nixpkgs.outPath}" ];

  settings = lib.mkMerge [
    {
      keep-outputs = true;
      keep-going = true;
      builders-use-substitutes = true;
      allow-unsafe-native-code-during-evaluation = true;
      accept-flake-config = true;
      http-connections = 0;
      use-xdg-base-directories = true;
      download-buffer-size = 524288000;

      flake-registry = __toFile "flake-registry.json" (__toJSON {
        flakes = [
          {
            from = {
              id = "nixpkgs";
              type = "indirect";
            };
            to = {
              inherit (inputs.nixpkgs) rev;
              type = "github";
              owner = "NixOS";
              repo = "nixpkgs";
            };
          }
        ];
        version = 2;
      });

      extra-experimental-features = [
        "auto-allocate-uids"
        "ca-derivations"
        "dynamic-derivations"
        "flakes"
        "nix-command"
        "pipe-operators"
      ];

      max-jobs = "auto";

      extra-trusted-users = [
        "root"
        "moni"
      ];
    }

    (
      let
        caches = [
          "https://mirrors.tuna.tsinghua.edu.cn/nix-channels/store?priority=10"
          "https://nix-mirror.freetls.fastly.net?priority=11"
          "https://cache.nixos.org?priority=12"
          "https://nix-community.cachix.org?priority=13"
        ];
      in
      {
        extra-substituters = caches;
        extra-trusted-substituters = caches;

        extra-trusted-public-keys = [
          "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
        ];
      }
    )

    (lib.mkIf (pkgs.stdenv.hostPlatform.isDarwin && pkgs.stdenv.hostPlatform.isAarch64) {
      extra-platforms = "x86_64-darwin";
    })
    (lib.mkIf pkgs.stdenv.hostPlatform.isDarwin { sandbox = "relaxed"; })
  ];
}
