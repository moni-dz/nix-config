final: prev: {
  slock = prev.slock.overrideAttrs (old: {
    patches = [
      ./patches/slock-blur_pixelated_screen-1.4.diff
      ./patches/slock-dpms-1.4.diff
      ./patches/slock-quickcancel-1.4.diff
    ];
  });
}
