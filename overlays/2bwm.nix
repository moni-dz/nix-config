final: prev: {
  _2bwm = (prev._2bwm.overrideAttrs (old: {
    src = prev.fetchFromGitHub {
      owner = "venam";
      repo = old.pname;
      rev = "148d83254ca5f0fbc464ced5c487103aaf959559";
      sha256 = "sha256-f/wYty1C0OD7Cc2djTo17EpFS26+HRJAZ1K4zLWOo3s=";
    };

    buildInputs = (old.buildInputs or [ ]) ++ [ prev.xorg.libX11 ];

    patches = [
      ./patches/twobwm_autostart.patch
      ./patches/twobwm_config.patch
      ./patches/twobwm_optimizations.patch
    ];
  })).override { stdenv = prev.clangStdenv; };
}
