final: prev: {
  haskellPackages = prev.haskellPackages.extend (hfinal: hprev: {
    X11 = hprev.X11_1_10;

    xmonad = hprev.callPackage ../derivations/xmonad.nix {
      src = prev.xmonad-src;
    };

    xmonad-contrib = hprev.callPackage ../derivations/xmonad-contrib.nix {
      src = prev.xmonad-contrib-src;
    };
  });
}
