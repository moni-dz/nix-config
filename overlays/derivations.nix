final: prev: {
  asitop = prev.callPackage ../derivations/asitop.nix { };

  python3 = prev.python3.override {
    packageOverrides = pfinal: pprev: {
      pydashing = pfinal.callPackage ../derivations/dashing.nix { };
    };
  };

  multimc-offline = prev.libsForQt5.callPackage ../derivations/multimc-offline.nix { };
}
