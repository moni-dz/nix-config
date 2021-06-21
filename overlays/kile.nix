final: prev: {
  kile-wl = prev.kile-wl.overrideAttrs (old: {
    src = final.kile-wl-src;
  });
}
