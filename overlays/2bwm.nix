final: prev: {
  _2bwm = prev._2bwm.overrideAttrs (old: {
    src = prev.twobwm-src;

    # libX11 is a dependency of twobwm_config.patch
    buildInputs = (old.buildInputs or [ ]) ++ [ prev.xorg.libX11 ];

    patches = [
      ./patches/twobwm_autostart.patch
      ./patches/twobwm_config.patch
      ./patches/twobwm_optimizations.patch
    ];
  });
}
