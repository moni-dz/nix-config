final: prev: {
  haskellPackages = prev.haskellPackages.extend
    (prev.haskell.lib.packageSourceOverrides {
      xmonad = prev.xmonad-src;
      xmonad-contrib = prev.xmonad-contrib-src;
    });
}
