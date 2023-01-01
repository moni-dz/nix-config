final: prev: {
  firefox-unwrapped = prev.firefox-unwrapped.overrideAttrs (old: {
    patches = (old.patches or [ ]) ++ [ ../patches/D164578.diff ];
  });
}
