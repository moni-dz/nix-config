final: prev: {
  haskellPackages = prev.haskellPackages.extend (prev.haskell.lib.packageSourceOverrides {
    xmonad = prev.fetchFromGitHub {
      owner = "xmonad";
      repo = "xmonad";
      rev = "131fd3669f6c2952d3094016d14873fdfe66f98c";
      sha256 = "sha256-NU+QAAhCxnkuF9fn6AJ0vvgq4+dFwDHS7oDde2hPfEQ=";
    };

    xmonad-contrib = prev.fetchFromGitHub {
      owner = "xmonad";
      repo = "xmonad-contrib";
      rev = "a622c0808ffd00b479fad5e6d3c6b059f50777e4";
      sha256 = "sha256-Zh9Yq+1UBkJNig6U/6TI2nURe/HQFU/OspekVTLxjOM=";
    };
  });
}
