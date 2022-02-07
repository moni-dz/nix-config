final: prev: {
  phocus = prev.callPackage ../derivations/phocus.nix { src = prev.phocus-src; };
  multimc-offline = prev.libsForQt5.callPackage ../derivations/multimc-offline.nix { };
}
