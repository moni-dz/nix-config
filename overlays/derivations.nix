final: prev: {
  phocus = prev.callPackage ../derivations/phocus.nix { src = prev.phocus-src; };
}
