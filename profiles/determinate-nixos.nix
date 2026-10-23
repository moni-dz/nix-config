{
  lib,
  self',
  ...
}:

{
  nix.package = lib.mkForce self'.packages.nix-src;

  nix.settings = {
    eval-cores = 0;
    lazy-trees = true;
    extra-experimental-features = [ "parallel-eval" ];
  };
}
