final: prev: {
  output-fonts = prev.callPackage ../derivations/output-fonts.nix { };

  phocus = prev.callPackage ../derivations/phocus.nix {
    src = prev.phocus-src;
    theme = import ../config/theme.nix;
  };

  spotify-adblock = prev.callPackage ../derivations/spotify-adblock.nix { };

  spotify-wrapped = prev.callPackage ../derivations/spotify-wrapped.nix {
    inherit (final.unstable) spotify;
    spotify-adblock = final.spotify-adblock;
  };

  taiwins = prev.callPackage ../derivations/taiwins.nix { };
}
