{ lib, ... }:

{
  flake.nixosModules = lib.pipe (lib.filesystem.listFilesRecursive ./.) [
    (__filter (f: lib.hasSuffix "nix" f && !(lib.hasSuffix "default.nix" f)))
    (map (f: lib.nameValuePair (lib.removeSuffix ".nix" (baseNameOf f)) (import f)))
    __listToAttrs
  ];
}
