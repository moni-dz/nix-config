{
  lib,
  inputs,
  self',
  ...
}:

{
  nix = {
    package = lib.mkForce self'.packages.nix-src;
    nixPath = lib.mkForce [ "nixpkgs=flake:nixpkgs" ];

    registry.nixpkgs.to = lib.mkForce {
      type = "github";
      owner = "NixOS";
      repo = "nixpkgs";
      inherit (inputs.nixpkgs) rev;
    };

    settings = {
      eval-cores = 0;
      lazy-trees = true;
      extra-experimental-features = [ "parallel-eval" ];
    };
  };
}
