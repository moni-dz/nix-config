{ inputs, pkgs }:

let
  nix-src = import inputs.nix-src.inputs.nixpkgs {
    localSystem = pkgs.stdenv.hostPlatform.system;
    overlays = [
      (_: prev: { libgit2 = prev.libgit2.overrideAttrs { doCheck = false; }; })
      inputs.nix-src.overlays.internal
    ];
  };
in
pkgs.lib.infuse nix-src.nixComponents2.nix-everything {
  __output.doCheck.__assign = false;
}
