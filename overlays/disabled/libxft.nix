final: prev: {
  xorg.libXft = prev.xorg.libXft.overrideAttrs (old: {
    src = builtins.fetchTarball {
      url = "https://xorg.freedesktop.org/releases/individual/lib/libXft-2.3.3.tar.bz2";
      sha256 = prev.stdenv.lib.fakeSha256;
    };

    patches = (old.patches or [ ]) ++ [
      (final.fetchpatch {
        url = "https://gitlab.freedesktop.org/xorg/lib/libxft/-/commit/7808631e7a9a605d5fe7a1077129c658d9ec47fc.patch";
        sha256 = prev.stdenv.lib.fakeSha256;
      })
    ];

    buildInputs = (old.buildInputs or [ ]) ++ [ ];
  });
}
