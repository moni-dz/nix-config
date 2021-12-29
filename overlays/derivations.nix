final: prev: {
  output-fonts = prev.callPackage ../derivations/output-fonts.nix { };
  phocus = prev.callPackage ../derivations/phocus.nix { src = prev.phocus-src; };
}
